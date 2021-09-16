use color_eyre::eyre::eyre;
use derive_more::{Deref, DerefMut};
use inflector::Inflector;
use itertools::Itertools;
use proc_macro2::TokenStream;
use quote::{format_ident, quote, IdentFragment, ToTokens};
use rand::SeedableRng;
use rand_chacha::ChaCha12Rng;
use syn::{parse_quote, visit::Visit, Expr, Ident};
use tap::{Pipe, Tap, TapFallible};

use super::lower::*;
use super::*;
use crate::parser::{self, AstVisit, Descriptor, PassFail, Requirement, Statement, Visitable};

#[derive(Debug, Clone)]
pub(crate) struct CodegenCtx {
    rng: ChaCha12Rng,
}

impl CodegenCtx {
    pub(crate) fn new() -> CodegenCtx {
        CodegenCtx {
            rng: ChaCha12Rng::seed_from_u64(12345),
        }
    }
    pub(crate) fn scoped<T>(&self, func: impl FnOnce(&mut Self) -> T) -> T {
        let mut copy = self.clone();
        func(&mut copy)
    }
}

pub(crate) trait ToRust: Sized {
    fn to_rust(
        &self,
        mode: Mode,
        ctx: &mut CodegenCtx,
    ) -> color_eyre::Result<proc_macro2::TokenStream>;
}

fn sig_to_signer(sig: impl IdentFragment) -> Ident {
    format_ident!("{}_signer", sig)
}

fn new_secret(rng: &mut impl rand::RngCore) -> String {
    use libsecp256k1::SecretKey;
    let secret = SecretKey::random(rng);
    format!("{:x}", secret)
}

fn root_for(mode: Mode) -> TokenStream {
    match mode {
        Mode::Unit => quote! { crate },
        Mode::Integration => quote! { ccprocessor_rust },
    }
}

fn sig_to_walletid(sig: impl IdentFragment) -> Ident {
    format_ident!("{}_wallet_id_", sig)
}

fn command_to_guid(command: impl IdentFragment) -> Ident {
    format_ident!("{}_guid_", command)
}

impl ToRust for Descriptor {
    fn to_rust(
        &self,
        mode: Mode,
        ctx: &mut CodegenCtx,
    ) -> color_eyre::Result<proc_macro2::TokenStream> {
        let descriptor = self;
        let mut counter = WalletCounter::new();
        descriptor.statements().visit_with(&mut counter);
        let tip_start = counter.count + 2;
        let tse_defined = FindLocal::named("tse").in_ast(&descriptor);
        let tse = if tse_defined {
            quote! {}
        } else {
            quote! { let mut tse = ToStateEntryCtx::new( #tip_start ); }
        };
        let meta = TestMetaData::try_from(descriptor.meta())?;
        let name = meta.name.to_snake_case().to_lowercase();
        let test_name = format_ident!("{}", name);
        let sighash_ids = meta
            .sighashes
            .iter()
            .map(|i| format_ident!("{}", i))
            .collect_vec();
        let signers = meta.sighashes.iter().map(sig_to_signer).collect_vec();
        let secrets = meta
            .sighashes
            .iter()
            .map(|_| new_secret(&mut ctx.rng))
            .collect_vec();
        let sighash_decls = quote! {
            #(
                let #signers = signer_with_secret(#secrets);
                let #sighash_ids = SigHash::from(&#signers);
            )*
        };

        match mode {
            Mode::Unit => {
                let imports = quote! {
                    use crate::handler::types::*;
                    use std::str::FromStr as _;
                };

                let tx_fee = meta.tx_fee;
                let tx_fee_decl = if let Some(expr) = tx_fee {
                    quote! {
                        let mut tx_fee = #expr;
                    }
                } else {
                    quote! {
                        let mut tx_fee = TX_FEE.clone();
                    }
                };

                let request = meta.request;
                let request_decl = if let Some(expr) = request {
                    quote! {
                        let mut request = #expr;
                    }
                } else {
                    quote! {
                        let mut request = TpProcessRequest { tip: tse.tip().into(), ..Default::default() };
                    }
                };

                let mock_setup = quote! {
                    let mut tx_ctx = MockTransactionContext::default();
                    let mut ctx = MockHandlerContext::default();

                };

                let stmts: Vec<_> = descriptor
                    .statements()
                    .into_iter()
                    .map(|f| ToRust::to_rust(&f, mode, ctx))
                    .try_collect()?;

                let execute = if let PassFail::Fail { err, .. } = meta.pass_fail {
                    quote! {
                        execute_failure(command, &request, &tx_ctx, &mut ctx, #err);
                    }
                } else {
                    quote! {
                        execute_success(command, &request, &tx_ctx, &mut ctx);
                    }
                };

                let body = quote! {
                    #imports
                    #sighash_decls
                    #tse
                    #tx_fee_decl
                    #request_decl
                    #mock_setup

                    #( #stmts )*

                    #execute
                };
                let decl = quote! {
                    #[test]
                    #[allow(unused_variables, unused_parens, unused_imports, unused_mut, unused_braces)]
                    fn #test_name () {
                        #body
                    }
                };
                Ok(decl)
            }
            Mode::Integration => {
                let imports = quote! {
                    use rug::Integer;
                    use ccprocessor_rust::handler::*;
                    use ccprocessor_rust::ext::*;
                    use ccprocessor_rust::handler::types::*;
                    use prost::Message as _;
                    use protobuf::Message as _;
                    use std::str::FromStr as _;
                    use std::convert::TryFrom as _;
                };

                let tx_fee = meta.tx_fee;
                let tx_fee_decl = if let Some(fee) = tx_fee {
                    quote! {
                        let mut tx_fee = #fee;
                    }
                } else {
                    quote! {
                        let mut tx_fee = ccprocessor_rust::handler::constants::TX_FEE.clone();
                    }
                };
                let request = meta.request;
                let request_decl = if let Some(req) = request {
                    quote! {
                        let mut request = #req;
                    }
                } else {
                    quote! {
                        let mut request = TpProcessRequest::default();
                    }
                };
                let statements = descriptor.statements();

                let (expectations, stmts): (Vec<_>, Vec<_>) = statements
                    .iter()
                    .partition(|s| matches!(s, Statement::Expect(..)));
                let expectations: Vec<_> = expectations
                    .iter()
                    .map(|f| ToRust::to_rust(*f, mode, ctx))
                    .try_collect()?;
                let stmts: Vec<_> = stmts
                    .into_iter()
                    .map(|f| ToRust::to_rust(f, mode, ctx))
                    .try_collect()?;

                let signer = meta.signer;
                let guid = command_to_guid("command");
                let execute = if let PassFail::Fail { err, .. } = meta.pass_fail {
                    quote! {
                        execute_failure(command, #err, ports, Some(Nonce::from(#guid.clone())), &#signer);
                    }
                } else {
                    quote! {
                        execute_success(command, ports, Some(Nonce::from(#guid.clone())), &#signer);
                    }
                };

                let head = quote! {
                    #imports
                    setup_logs();

                };
                let body = quote! {
                    #sighash_decls
                    #tse
                    #tx_fee_decl
                    #request_decl

                    #( #stmts )*

                    #execute

                    #( #expectations )*
                };
                let decl = quote! {
                    #[test]
                    #[allow(unused_variables, unused_parens, unused_imports, unused_mut, unused_braces, dead_code)]
                    fn #test_name () {
                        #head
                        integration_test(|ports| {
                            #body
                        });
                    }
                };
                Ok(decl)
            }
        }
    }
}

impl ToRust for Statement {
    fn to_rust(
        &self,
        mode: Mode,
        ctx: &mut CodegenCtx,
    ) -> color_eyre::Result<proc_macro2::TokenStream> {
        todo!()
    }
}
