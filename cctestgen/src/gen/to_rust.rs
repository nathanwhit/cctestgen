use inflector::Inflector;
use itertools::Itertools;
use proc_macro2::TokenStream;
use quote::{format_ident, quote, IdentFragment, ToTokens};
use rand::SeedableRng;
use rand_chacha::ChaCha12Rng;
use syn::{parse_quote, visit::Visit, Expr, Ident};

use super::lower::*;
use super::*;
use crate::parser::{
    AstVisit, Descriptor, ExpectStmt, Expectation, Meta, MetaDatum, MetaOrStatement, PassFail,
    RequireStmt, Requirement, RustBlock, StateEntry, StateMapping, Statement, StmtBlock, Visitable,
};

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
}

pub(crate) trait ToRust: Sized {
    fn to_rust(
        &self,
        mode: Mode,
        ctx: &mut CodegenCtx,
    ) -> color_eyre::Result<proc_macro2::TokenStream>;
}

pub(crate) fn sig_to_signer(sig: impl IdentFragment) -> Ident {
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

pub(crate) fn sig_to_walletid(sig: impl IdentFragment) -> Ident {
    format_ident!("{}_wallet_id_", sig)
}

pub(crate) fn command_to_guid(command: impl IdentFragment) -> Ident {
    format_ident!("{}_guid_", command)
}

impl ToRust for Descriptors {
    fn to_rust(
        &self,
        mode: Mode,
        ctx: &mut CodegenCtx,
    ) -> color_eyre::Result<proc_macro2::TokenStream> {
        let mut ts = TokenStream::new();
        for descriptor in &self.descriptors {
            ts.extend(descriptor.to_rust(mode, ctx)?);
        }
        let header = match mode {
            Mode::Unit => quote! { use super::*; },
            Mode::Integration => quote! {
                #![cfg(feature = "integration-testing")]

                use super::common::*;
            },
        };

        Ok(quote! {
            #header

            #ts
        })
    }
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
        let guid_fallback = if meta.guid.is_none() {
            let guid_name = command_to_guid("command");
            quote! {
                let #guid_name = Guid!();
            }
        } else {
            quote! {}
        };
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

        let tx_fee = meta.tx_fee;
        let tx_fee_decl = if let Some(fee) = tx_fee {
            quote! {
                let mut tx_fee = #fee;
            }
        } else {
            let root = root_for(mode);
            quote! {
                let mut tx_fee = #root::handler::constants::TX_FEE.clone();
            }
        };

        match mode {
            Mode::Unit => {
                let imports = quote! {
                    use crate::handler::types::*;
                    use std::str::FromStr as _;
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
                    .body
                    .iter()
                    .map(|f| ToRust::to_rust(f, mode, ctx))
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
                    #guid_fallback
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
                let statements = descriptor.body.clone();

                let (expectations, stmts): (Vec<_>, Vec<_>) = statements
                    .iter()
                    .partition(|s| matches!(s, MetaOrStatement::Statement(Statement::Expect(..))));
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
                    #guid_fallback

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

impl<'ctx, T> ToTokens for Integration<'ctx, T>
where
    T: ToRust,
{
    fn to_tokens(&self, tokens: &mut TokenStream) {
        tokens.extend(
            self.inner
                .to_rust(Mode::Integration, &mut self.ctx.borrow_mut())
                .unwrap(),
        );
    }
}
impl<'ctx, T> ToTokens for Unit<'ctx, T>
where
    T: ToRust,
{
    fn to_tokens(&self, tokens: &mut TokenStream) {
        tokens.extend(
            self.inner
                .to_rust(Mode::Unit, &mut self.ctx.borrow_mut())
                .unwrap(),
        );
    }
}

impl ToRust for RustBlock {
    fn to_rust(
        &self,
        _mode: Mode,
        _ctx: &mut CodegenCtx,
    ) -> color_eyre::Result<proc_macro2::TokenStream> {
        Ok(self.block.to_token_stream())
    }
}

impl ToRust for Statement {
    fn to_rust(
        &self,
        mode: Mode,
        ctx: &mut CodegenCtx,
    ) -> color_eyre::Result<proc_macro2::TokenStream> {
        Ok(match self {
            Statement::Expect(expect) => expect.to_rust(mode, ctx)?,
            Statement::Verbatim(block) => block.to_rust(mode, ctx)?,
            Statement::Integration(block) if mode == Mode::Integration => {
                block.to_rust(mode, ctx)?
            }
            Statement::Unit(block) if mode == Mode::Unit => block.to_rust(mode, ctx)?,
            Statement::Integration(_) | Statement::Unit(_) => quote! {},
            Statement::Require(require) => require.to_rust(mode, ctx)?,
            Statement::Rust(stmt) => stmt.to_token_stream(),
        })
    }
}

macro_rules! impl_to_rust_iter {
    ($id: ident, $iter: ident) => {
        impl ToRust for $id {
            fn to_rust(
                &self,
                mode: Mode,
                ctx: &mut CodegenCtx,
            ) -> color_eyre::Result<proc_macro2::TokenStream> {
                let mut ts = proc_macro2::TokenStream::new();
                for el in &self.$iter {
                    ts.extend(el.to_rust(mode, ctx)?);
                }
                Ok(ts)
            }
        }
    };
}

impl_to_rust_iter!(ExpectStmt, expectations);

impl_to_rust_iter!(StmtBlock, stmts);

impl_to_rust_iter!(RequireStmt, requirements);

#[allow(unused)]
impl ToRust for Requirement {
    fn to_rust(
        &self,
        mode: Mode,
        ctx: &mut CodegenCtx,
    ) -> color_eyre::Result<proc_macro2::TokenStream> {
        Ok(match mode {
            Mode::Unit => match self {
                Requirement::Wallet { ident, .. } => {
                    let id = sig_to_walletid(&ident);
                    let sig = ident;
                    quote! {
                        let #id = WalletId::from(&#sig);
                    }
                }
                Requirement::SendTx { .. } => quote! {},
            },
            Mode::Integration => match self {
                Requirement::Wallet {
                    wallet_token,
                    for_token,
                    ident,
                    with_token,
                    amount_token,
                    eq_token,
                    expr,
                } => {
                    use rand::Rng;
                    let id = sig_to_walletid(&ident);
                    let sig = ident;
                    let amount = expr;
                    let signer = sig_to_signer(sig.clone());
                    let random_tx_id: String = (&mut ctx.rng)
                        .sample_iter(rand::distributions::Alphanumeric)
                        .map(char::from)
                        .take(15)
                        .collect();
                    quote! {
                        {
                            let amount = #amount;
                            let collect_coins = ccprocessor_rust::handler::CollectCoins {
                                amount: amount.into(),
                                eth_address: "dummy".into(),
                                blockchain_tx_id: #random_tx_id.into(),
                            };
                            let response = send_command_with_signer(collect_coins, ports, None, &#signer);
                            assert_matches!(complete_batch(&response.link, None), Some(BatchStatus::Committed));

                        }
                        let #id = WalletId::from(&#sig);
                    }
                }
                Requirement::SendTx {
                    send_token,
                    tx_token,
                    eq_token,
                    tx,
                    with_token,
                    signer_token,
                    eq_token2,
                    signer,
                    guid,
                } => {
                    let guid = match guid {
                        Some(expr) => {
                            let expr = &expr.guid;
                            quote! {
                                Some(Nonce::from(#expr))
                            }
                        }
                        None => quote! { None },
                    };

                    quote! {
                        {
                            let tx = #tx;
                            let response = send_command_with_signer(tx, ports, #guid, &#signer);
                            assert_matches!(complete_batch(&response.link, None), Some(BatchStatus::Committed));
                        }
                    }
                }
            },
        })
    }
}

struct Finder<const N: usize> {
    values: [&'static str; N],
    found: Option<&'static str>,
}

impl<'ast, const N: usize> Visit<'ast> for Finder<N> {
    fn visit_expr_path(&mut self, p: &'ast syn::ExprPath) {
        if !p.path.segments.is_empty() {
            let ident = &p.path.segments.last().unwrap().ident;
            for value in self.values {
                if ident == value {
                    self.found = Some(value);
                    return;
                }
            }
        }
    }
}

impl<'ast, const N: usize> AstVisit<'ast> for Finder<N> {}

const OPTION_FINDER: Finder<2> = Finder {
    values: ["Some", "None"],
    found: None,
};

const RESULT_FINDER: Finder<2> = Finder {
    values: ["Ok", "Err"],
    found: None,
};

fn is_option(expr: &Expr, finder: Option<&mut Finder<2>>) -> bool {
    if let Some(finder) = finder {
        finder.visit_expr(expr);
        finder.found.is_some()
    } else {
        let mut finder = OPTION_FINDER;
        finder.visit_expr(expr);
        finder.found.is_some()
    }
}

fn into_option(expr: &Expr, finder: Option<&mut Finder<2>>) -> Expr {
    if is_option(expr, finder) {
        expr.clone()
    } else {
        parse_quote!(Some(#expr))
    }
}
fn is_result(expr: &Expr) -> bool {
    let mut finder = RESULT_FINDER;
    finder.visit_expr(expr);
    finder.found.is_some()
}

fn into_result(expr: &Expr) -> Expr {
    if is_result(expr) {
        expr.clone()
    } else {
        parse_quote!(Ok(#expr))
    }
}

impl ToRust for Expectation {
    fn to_rust(
        &self,
        mode: Mode,
        ctx: &mut CodegenCtx,
    ) -> color_eyre::Result<proc_macro2::TokenStream> {
        Ok(match mode {
            Mode::Unit => match self {
                Expectation::GetBalance {
                    address_expr,
                    return_expr,
                    ..
                } => {
                    let is_option = is_option(return_expr, None);

                    let casted = if is_option {
                        quote! {
                            ret
                        }
                    } else {
                        quote! {
                            Option::from(ret)
                        }
                    };

                    quote! {
                        {
                            let address = #address_expr;
                            let ret = #return_expr;
                            tx_ctx.expect_get_state_entry()
                                .withf(move |addr| address.as_str() == addr)
                                .return_once(move |_| Ok( wallet_with( #casted ) ) );
                        }
                    }
                }
                Expectation::GetState {
                    address_expr,
                    return_expr,
                    ..
                } => {
                    let mut finder = OPTION_FINDER;
                    let ret = into_option(return_expr, Some(&mut finder));
                    let ret = if let Some(value) = finder.found {
                        match value {
                            "None" => parse_quote!(<Option<crate::protos::Wallet>>::None),
                            _ => ret,
                        }
                    } else {
                        ret
                    };

                    quote! {
                        expect_get_state_entry(
                            &mut tx_ctx,
                            #address_expr,
                            #ret,
                            None,
                        );

                    }
                }
                Expectation::SetState { .. } => todo!(),
                Expectation::DeleteState { .. } => todo!(),
                Expectation::DeleteStates { addresses, .. } => {
                    let entries: Vec<_> = addresses.elems.clone().into_iter().collect();
                    quote! {
                        expect_delete_state_entries(
                            &mut tx_ctx,
                            vec![
                                #(
                                    #entries.to_string()
                                ),*
                            ]
                        );
                    }
                }
                Expectation::SetStates { mapping, .. } => {
                    let mapping = mapping.to_rust(mode, ctx)?;
                    quote! {
                        expect_set_state_entries(
                            &mut tx_ctx,
                            #mapping
                        );
                    }
                }
                Expectation::GetSigHash { return_expr, .. } => {
                    quote! {
                        {
                            let sig = crate::handler::types::SigHash(#return_expr.to_string());

                            ctx.expect_sighash()
                                .return_once(move |_| Ok(sig));
                        }
                    }
                }
                Expectation::GetGuid { return_expr, .. } => {
                    quote! {
                        {
                            let guid = #return_expr;

                            ctx.expect_guid()
                                .returning(move |_| guid.clone());
                        }
                    }
                }
                Expectation::Verify { return_expr, .. } => {
                    let ret = into_result(return_expr);
                    quote! {
                        {
                            let ret = #ret;

                            ctx.expect_verify()
                                .return_once(move |_| ret);
                        }
                    }
                }
            },
            Mode::Integration => match self {
                Expectation::SetState {
                    address_expr,
                    return_expr,
                    ..
                } => {
                    quote! {
                        expect_set_state_entry(#address_expr.to_string(), #return_expr.into()).unwrap();
                    }
                }
                Expectation::SetStates { mapping, .. } => {
                    let mapping = mapping.to_rust(mode, ctx)?;

                    quote! {
                        expect_set_state_entries(
                            ports,
                            #mapping
                        ).unwrap();
                    }
                }
                Expectation::DeleteState { .. } => todo!(),
                Expectation::DeleteStates { addresses, .. } => {
                    let entries: Vec<_> = addresses.elems.clone().into_iter().collect();
                    quote! {
                        expect_delete_state_entries(
                            ports,
                            vec![
                                #(
                                    #entries.to_string()
                                ),*
                            ]
                        ).unwrap();
                    }
                }
                Expectation::GetBalance { .. }
                | Expectation::GetState { .. }
                | Expectation::GetGuid { .. }
                | Expectation::GetSigHash { .. }
                | Expectation::Verify { .. } => quote! {},
            },
        })
    }
}

impl ToRust for StateMapping {
    fn to_rust(
        &self,
        mode: Mode,
        ctx: &mut CodegenCtx,
    ) -> color_eyre::Result<proc_macro2::TokenStream> {
        let entries: Vec<_> = self
            .fields
            .iter()
            .map(|e| e.to_rust(mode, ctx))
            .try_collect()?;

        let rest = if let Some(rest) = &self.rest {
            let dot2 = self.dot2_token.unwrap();
            quote! {
                #dot2 #rest
            }
        } else {
            quote! {}
        };

        Ok(quote! {
            vec![
                #( #entries ),* #rest
            ]
        })
    }
}

impl ToRust for StateEntry {
    fn to_rust(
        &self,
        _mode: Mode,
        _ctx: &mut CodegenCtx,
    ) -> color_eyre::Result<proc_macro2::TokenStream> {
        let key = &self.key;
        if let Some(value) = &self.value {
            Ok(quote! {
                (#key, #value)
            })
        } else {
            Ok(quote! {
                #key
            })
        }
    }
}

impl ToRust for MetaDatum {
    fn to_rust(
        &self,
        mode: Mode,
        ctx: &mut CodegenCtx,
    ) -> color_eyre::Result<proc_macro2::TokenStream> {
        self.meta.to_rust(mode, ctx)
    }
}

impl ToRust for Meta {
    fn to_rust(
        &self,
        _mode: Mode,
        _ctx: &mut CodegenCtx,
    ) -> color_eyre::Result<proc_macro2::TokenStream> {
        match self {
            Meta::Command(cmd) => {
                let expr = cmd.value();
                Ok(quote! { let mut command = #expr; })
            }
            Meta::Guid(guid) => {
                let expr = guid.value();
                let id = command_to_guid("command");
                Ok(quote! { let mut #id = #expr; })
            }
            Meta::TxFee(_)
            | Meta::Request(_)
            | Meta::TestType(_)
            | Meta::Signer(_)
            | Meta::SigHashes(_)
            | Meta::TestName(_) => Ok(quote! {}),
        }
    }
}

impl ToRust for MetaOrStatement {
    fn to_rust(
        &self,
        mode: Mode,
        ctx: &mut CodegenCtx,
    ) -> color_eyre::Result<proc_macro2::TokenStream> {
        match self {
            MetaOrStatement::Meta(m) => m.to_rust(mode, ctx),
            MetaOrStatement::Statement(s) => s.to_rust(mode, ctx),
        }
    }
}
