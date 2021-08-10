use std::{convert::TryFrom, str::FromStr};

use color_eyre::eyre::eyre;
use color_eyre::Result;
use inflector::Inflector;
use itertools::Itertools;
use proc_macro2::TokenStream;
use quote::{format_ident, quote, IdentFragment};
use syn::Ident;

use super::ast::*;

impl ToRust for Field {
    fn to_rust(self, mode: Mode) -> Result<TokenStream> {
        let Self { name, value } = self;
        let value = value.to_rust(mode)?;
        Ok(quote! {
            #name : #value
        })
    }
}

impl ToRust for Command {
    fn to_rust(self, mode: Mode) -> Result<TokenStream> {
        let Command { name, fields } = self;
        let name: syn::Path = syn::parse_str(&name)?;
        let fields = fields
            .into_iter()
            .map(|f| ToRust::to_rust(f, mode))
            .collect::<Result<Vec<_>>>()?;

        Ok(quote! {
            #name {
                #(
                    #fields
                ),*
            }
        })
    }
}

fn sig_to_signer(sig: impl IdentFragment) -> Ident {
    format_ident!("_{}_signer", sig)
}

#[derive(Clone, Copy, Debug)]
pub enum Mode {
    Integration,
    Unit,
}

impl TryFrom<&str> for Mode {
    type Error = color_eyre::Report;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match &*value.trim().to_ascii_lowercase() {
            "integration" => Ok(Mode::Integration),
            "unit" => Ok(Mode::Unit),
            other => Err(eyre!("Invalid mode: {}", &other)),
        }
    }
}

pub trait ToRust {
    fn to_rust(self, mode: Mode) -> Result<TokenStream>;
}

impl ToRust for Expr {
    fn to_rust(self, mode: Mode) -> Result<TokenStream> {
        Ok(match self {
            Expr::RustCode(code) => {
                let code = match mode {
                    Mode::Unit => code,
                    Mode::Integration => code.replace("crate", "ccprocessor_rust"),
                };
                let ts = TokenStream::from_str(&code)
                    .map_err(|e| eyre!("failed to parse tokenstream for code {} : {}", code, e))?;
                quote! {
                    { #ts }
                }
            }
            Expr::Sighash(sig) => {
                let id = format_ident!("{}", sig.0);
                quote! {
                    #id .clone().into()
                }
            }
            Expr::Default => quote! {
                { ::core::default::Default::default() }
            },
            Expr::Ident(ident) => quote! {
                { #ident.clone() }
            },
            Expr::Construction { name, fields } => {
                let fields = fields
                    .into_iter()
                    .map(|f| ToRust::to_rust(f, mode))
                    .collect::<Result<Vec<_>>>()?;
                quote! {
                    #name {
                        #( #fields ),*
                    }
                }
            }
            Expr::Array(exprs) => {
                let exprs = exprs
                    .into_iter()
                    .map(|f| ToRust::to_rust(f, mode))
                    .collect::<Result<Vec<_>>>()?;
                quote! {
                    vec! [
                        #( #exprs ),*
                    ]
                }
            }
            Expr::Mapping(_) => todo!(),
            Expr::Literal(literal) => quote! {
                { #literal }
            },
            Expr::WalletId(id) => {
                let wallet_id = sig_to_walletid(id);
                quote! {
                    { #wallet_id.clone() }
                }
            }
            Expr::Option(opt) => match opt {
                Some(expr) => {
                    let expr = expr.to_rust(mode)?;
                    quote! {
                        { Some(#expr) }
                    }
                }
                None => quote! {
                    { None }
                },
            },
            Expr::GuidFor(cmd) => {
                let guid = command_to_guid(cmd);
                quote! {
                    { #guid.clone() }
                }
            }
            Expr::SignerFor(sig) => match mode {
                Mode::Unit => quote! {},
                Mode::Integration => {
                    let signer = sig_to_signer(sig);
                    quote! {
                        #signer
                    }
                }
            },
            Expr::MethodCall { value, methods } => {
                let value = value.to_rust(mode)?;
                let ts = TokenStream::from_str(&methods).map_err(|e| {
                    eyre!("failed to parse tokenstream for code {} : {}", methods, e)
                })?;
                quote! {
                    #value #ts
                }
            }
        })
    }
}

impl ToRust for Stmt {
    fn to_rust(self, mode: Mode) -> Result<TokenStream> {
        Ok(match self {
            Stmt::Binding { name, value } => {
                let ident = format_ident!("{}", name);
                let value = value.to_rust(mode)?;

                quote! {
                    let mut #ident = { #value };
                }
            }
            Stmt::Require { requirements } => {
                let reqs = requirements
                    .into_iter()
                    .map(|f| f.to_rust(mode))
                    .collect::<Result<Vec<_>>>()?;

                quote! {
                    #(
                        #reqs
                    )*
                }
            }
            Stmt::Expect { expectations } => {
                let exps = expectations
                    .into_iter()
                    .map(|f| f.to_rust(mode))
                    .collect::<Result<Vec<_>>>()?;

                quote! {
                    #(
                        #exps
                    )*
                }
            }
            Stmt::RustCode(code) => {
                let code = match mode {
                    Mode::Unit => code,
                    Mode::Integration => code.replace("crate", "ccprocessor_rust"),
                };
                let ts = TokenStream::from_str(&code).unwrap();

                quote! {
                    { #ts }
                }
            }
        })
    }
}

fn sig_to_walletid(sig: impl IdentFragment) -> Ident {
    format_ident!("_{}_wallet_id", sig)
}

fn command_to_guid(command: impl IdentFragment) -> Ident {
    format_ident!("_{}_guid", command)
}

impl ToRust for Requirement {
    fn to_rust(self, mode: Mode) -> Result<TokenStream> {
        Ok(match mode {
            Mode::Unit => match self {
                Requirement::Wallet {
                    sighash: Sighash(sig),
                    amount: _,
                } => {
                    let id = sig_to_walletid(&sig);
                    let sig = format_ident!("{}", sig);
                    quote! {
                        let #id = WalletId::from(&#sig);
                    }
                }
                Requirement::Guid { id } => {
                    let id = command_to_guid(&id);
                    quote! {
                        let #id = Guid("some_guid".into());
                    }
                }
            },
            Mode::Integration => match self {
                Requirement::Wallet {
                    sighash: Sighash(sig),
                    amount,
                } => {
                    use rand::Rng;
                    let id = sig_to_walletid(&sig);
                    let sig = format_ident!("{}", sig);
                    let amount = amount.to_rust(mode)?;
                    let signer = sig_to_signer(sig.clone());
                    let random_tx_id: String = rand::thread_rng()
                        .sample_iter(rand::distributions::Alphanumeric)
                        .map(char::from)
                        .take(15)
                        .collect();
                    quote! {
                        {
                            let collect_coins = ccprocessor_rust::handler::CollectCoins {
                                amount: { #amount.into() },
                                eth_address: "dummy".into(),
                                blockchain_tx_id: #random_tx_id.into(),
                            };
                            let response = send_command_with_signer(collect_coins, ports, None, &#signer);
                            assert!(matches!(complete_batch(&response.link, None), Some(BatchStatus::Committed)));

                        }
                        let #id = WalletId::from(&#sig);
                    }
                }
                Requirement::Guid { id } => {
                    let id = command_to_guid(&id);
                    quote! {
                        let #id = Guid::from(make_nonce());
                    }
                }
            },
        })
    }
}

impl ToRust for Expectation {
    fn to_rust(self, mode: Mode) -> Result<TokenStream> {
        Ok(match mode {
            Mode::Unit => match self {
                Expectation::GetBalance { id, ret } => {
                    let is_option = matches!(ret, Expr::Option(_));

                    let casted = if is_option {
                        quote! {
                            ret
                        }
                    } else {
                        quote! {
                            Option::from(ret)
                        }
                    };
                    let address = id.to_rust(mode)?;
                    let ret = ret.to_rust(mode)?;

                    quote! {
                        {
                            let address = #address .clone();
                            let ret = #ret .clone();
                            tx_ctx.expect_get_state_entry()
                                .withf(move |addr| address.as_str() == addr)
                                .return_once(move |_| Ok( wallet_with( #casted ) ) );
                        }
                    }
                }
                Expectation::GetStateEntry { id, ret } => {
                    let id = id.to_rust(mode)?;
                    let ret = ret.to_rust(mode)?;

                    quote! {
                        {
                            expect_get_state_entry(
                                &mut tx_ctx,
                                #id,
                                #ret,
                                None,
                            );
                        }
                    }
                }
                Expectation::SetStateEntry { id: _, value: _ } => todo!(),
                Expectation::DeleteStateEntry { id: _ } => todo!(),
                Expectation::SetStateEntries { values } => {
                    let entries: Vec<_> =
                        values.into_iter().map(|e| e.to_rust(mode)).try_collect()?;

                    quote! {
                        expect_set_state_entries(
                            &mut tx_ctx,
                            vec![
                                #(
                                    #entries
                                ),*
                            ]
                        );
                    }
                }
                Expectation::GetSighash { sig } => {
                    let sig = sig.to_rust(mode)?;

                    quote! {
                        {
                            let sig = crate::handler::types::SigHash(#sig.to_string());

                            ctx.expect_sighash()
                                .return_once(move |_| Ok(sig));
                        }
                    }
                }
                Expectation::GetGuid { guid } => {
                    let guid = guid.to_rust(mode)?;
                    quote! {
                        {
                            let guid = #guid.clone();

                            ctx.expect_guid()
                                .return_once(move |_| guid);
                        }
                    }
                }
            },
            Mode::Integration => match self {
                Expectation::SetStateEntry { id, value } => {
                    let id = id.to_rust(mode)?;
                    let value = value.to_rust(mode)?;

                    quote! {
                        expect_set_state_entry(#id.to_string(), #value.into()).unwrap();
                    }
                }
                Expectation::SetStateEntries { values } => {
                    let entries: Vec<_> =
                        values.into_iter().map(|e| e.to_rust(mode)).try_collect()?;

                    quote! {
                        {
                            expect_set_state_entries(
                                ports,
                                vec![
                                    #(
                                        #entries
                                    ),*
                                ]
                            ).unwrap();
                        }
                    }
                }
                Expectation::DeleteStateEntry { id: _ } => todo!(),
                Expectation::GetBalance { .. }
                | Expectation::GetStateEntry { .. }
                | Expectation::GetSighash { .. }
                | Expectation::GetGuid { .. } => quote! {},
            },
        })
    }
}

impl ToRust for MapEntry {
    fn to_rust(self, mode: Mode) -> Result<TokenStream> {
        Ok(match self {
            MapEntry::Pair(key, value) => {
                let key = key.to_rust(mode)?;
                let value = value.to_rust(mode)?;

                quote! {
                    (#key.to_string(), #value.into())
                }
            }
            MapEntry::Single(value) => {
                let value = value.to_rust(mode)?;

                quote! {
                    #value
                }
            }
        })
    }
}

impl ToRust for StructEntry {
    fn to_rust(self, mode: Mode) -> Result<TokenStream> {
        Ok(match self {
            StructEntry::Pair(Field { name, value }) => {
                let value = value.to_rust(mode)?;

                quote! {
                    #name: #value
                }
            }
            StructEntry::Single(value) => {
                let value = value.to_rust(mode)?;

                quote! {
                    #value
                }
            }
            StructEntry::Update(value) => {
                let value = value.to_rust(mode)?;

                quote! {
                    ..#value
                }
            }
        })
    }
}

fn new_secret() -> String {
    use libsecp256k1::SecretKey;
    let mut rng = rand::thread_rng();
    let secret = SecretKey::random(&mut rng);
    format!("{:x}", secret)
}

impl ToRust for Descriptor {
    fn to_rust(self, mode: Mode) -> Result<TokenStream> {
        let descriptor = self;
        if let Mode::Unit = mode {
            let name = descriptor.name.to_snake_case().to_lowercase();
            let test_name = format_ident!("{}", name);

            let imports = quote! {
                use crate::handler::types::*;
            };

            let fns = quote! {
                fn wallet_with(balance: Option<impl Into<Integer> + Clone>) -> Option<Vec<u8>> {
                    balance.map(|b| {
                        let wallet = crate::protos::Wallet {
                            amount: b.into().to_string(),
                        };
                        let mut buf = Vec::with_capacity(wallet.encoded_len());
                        wallet.encode(&mut buf).unwrap();
                        buf
                    })
                }
            };

            let sighashes = descriptor.sighashes.clone();
            let sighash_ids: Vec<Ident> =
                sighashes.iter().map(|i| format_ident!("{}", i)).collect();
            let signers: Vec<Ident> = sighashes.iter().map(sig_to_signer).collect();
            let secrets: Vec<String> = sighashes.iter().map(|_| new_secret()).collect();
            let sighash_decls = quote! {
                #(
                    let #signers = signer_with_secret(#secrets);
                    let #sighash_ids = SigHash::from(&#signers);
                )*
            };
            let command = descriptor.command.to_rust(mode)?;
            let command_decl = quote! {
                let mut command = #command;
            };
            let tx_fee = descriptor.tx_fee;
            let tx_fee_decl = if let Some(expr) = tx_fee {
                let tx_fee = expr.to_rust(mode)?;
                quote! {
                    let mut tx_fee = #tx_fee;
                }
            } else {
                quote! {
                    let mut tx_fee = TX_FEE.clone();
                }
            };

            let request = descriptor.request;
            let request_decl = if let Some(expr) = request {
                let request = expr.to_rust(mode)?;
                quote! {
                    let mut request = #request;
                }
            } else {
                quote! {
                    let mut request = TpProcessRequest::default();
                }
            };

            let mock_setup = quote! {
                let mut tx_ctx = MockTransactionContext::default();
                let mut ctx = MockHandlerContext::default();

            };

            let stmts: Vec<_> = descriptor
                .stmts
                .into_iter()
                .map(|f| ToRust::to_rust(f, mode))
                .try_collect()?;

            let execute = if let TestKind::Fail(err) = descriptor.pass_fail {
                let err = err.to_rust(mode)?;
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
                #fns
                #sighash_decls
                #command_decl
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
        } else {
            let name = descriptor.name.to_snake_case().to_lowercase();
            let test_name = format_ident!("{}", name);

            let imports = quote! {
                use rug::Integer;
                use ccprocessor_rust::handler::*;
                use ccprocessor_rust::ext::*;
                use ccprocessor_rust::handler::types::*;
                use prost::Message as _;
                use protobuf::Message as _;
            };

            let fns = quote! {};

            let sighashes = descriptor.sighashes.clone();
            let sighash_ids: Vec<Ident> =
                sighashes.iter().map(|i| format_ident!("{}", i)).collect();
            let signers: Vec<Ident> = sighashes.iter().map(sig_to_signer).collect();
            let secrets: Vec<String> = sighashes.iter().map(|_| new_secret()).collect();
            let sighash_decls = quote! {
                #(
                    let #signers = signer_with_secret(#secrets);
                    let #sighash_ids = SigHash::from(&#signers);
                )*
            };
            let command = descriptor.command.to_rust(mode)?;
            let command_decl = quote! {
                let mut command = #command;
            };
            let tx_fee = descriptor.tx_fee;
            let tx_fee_decl = if let Some(expr) = tx_fee {
                let tx_fee = expr.to_rust(mode)?;
                quote! {
                    let mut tx_fee = #tx_fee;
                }
            } else {
                quote! {
                    let mut tx_fee = ccprocessor_rust::handler::constants::TX_FEE.clone();
                }
            };

            let (expectations, stmts): (Vec<_>, Vec<_>) = descriptor
                .stmts
                .into_iter()
                .partition(|s| matches!(s, Stmt::Expect { .. }));
            let expectations: Vec<_> = expectations
                .into_iter()
                .map(|f| ToRust::to_rust(f, mode))
                .try_collect()?;
            let stmts: Vec<_> = stmts
                .into_iter()
                .map(|f| ToRust::to_rust(f, mode))
                .try_collect()?;
            let cmd = "command";
            let guid = command_to_guid(cmd);
            let signer = descriptor.signer.to_rust(mode)?;
            let execute = if let TestKind::Fail(err) = descriptor.pass_fail {
                let err = err.to_rust(mode)?;
                quote! {
                    execute_failure(command, #err, ports, Some(Nonce::from(#guid)), &#signer);
                }
            } else {
                quote! {
                    execute_success(command, ports, Some(Nonce::from(#guid)), &#signer);
                }
            };

            let head = quote! {
                #imports
                #fns
                setup_logs();

            };
            let body = quote! {
                #sighash_decls
                #command_decl
                #tx_fee_decl

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
