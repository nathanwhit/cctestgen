use std::{convert::TryFrom, str::FromStr};

use color_eyre::eyre::eyre;
use color_eyre::Result;
use inflector::Inflector;
use itertools::Itertools;
use proc_macro2::TokenStream;
use quote::{format_ident, quote, IdentFragment, ToTokens};
use rand::SeedableRng;
use rand_chacha::ChaCha12Rng;
use syn::Ident;

use super::ast::*;

impl ToRust for Field {
    fn to_rust(self, mode: Mode, ctx: &mut CodegenCtx) -> Result<TokenStream> {
        let Self { name, value } = self;
        let value = value.to_rust(mode, ctx)?;
        Ok(quote! {
            #name : #value
        })
    }
}

impl ToRust for Command {
    fn to_rust(self, mode: Mode, ctx: &mut CodegenCtx) -> Result<TokenStream> {
        let Command { name, fields } = self;
        let name: syn::Path = syn::parse_str(&name)?;
        let fields = fields
            .into_iter()
            .map(|f| ToRust::to_rust(f, mode, ctx))
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
    format_ident!("{}_signer", sig)
}

#[derive(Clone, Copy, Debug, PartialEq)]
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

#[derive(Clone, Copy, Debug)]
pub enum CloneStrategy {
    Insert,
    Nothing,
}

impl ToRust for CloneStrategy {
    fn to_rust(self, _mode: Mode, _ctx: &mut CodegenCtx) -> Result<TokenStream> {
        match self {
            CloneStrategy::Insert => Ok(quote! { .clone() }),
            CloneStrategy::Nothing => Ok(TokenStream::new()),
        }
    }
}

#[derive(Clone, Debug)]
pub struct CodegenCtx {
    rng: ChaCha12Rng,
    clone: CloneStrategy,
}

impl CodegenCtx {
    pub fn new() -> CodegenCtx {
        CodegenCtx {
            rng: ChaCha12Rng::seed_from_u64(12345),
            clone: CloneStrategy::Insert,
        }
    }
    pub fn scoped<T>(&self, func: impl FnOnce(&mut Self) -> T) -> T {
        let mut copy = self.clone();
        func(&mut copy)
    }
}

impl Default for CodegenCtx {
    fn default() -> Self {
        Self::new()
    }
}

pub trait ToRust {
    fn to_rust(self, mode: Mode, ctx: &mut CodegenCtx) -> Result<TokenStream>;
}

impl ToRust for syn::Path {
    fn to_rust(mut self, mode: Mode, _ctx: &mut CodegenCtx) -> Result<TokenStream> {
        match mode {
            Mode::Unit => Ok(self.to_token_stream()),
            Mode::Integration => {
                for segment in &mut self.segments {
                    if segment.ident == "crate" {
                        segment.ident = format_ident!("ccprocessor_rust");
                    }
                }
                Ok(self.to_token_stream())
            }
        }
    }
}

impl ToRust for Option<Expr> {
    fn to_rust(self, mode: Mode, ctx: &mut CodegenCtx) -> Result<TokenStream> {
        Ok(match self {
            Some(expr) => {
                let expr = expr.to_rust(mode, ctx)?;
                quote! {
                    Some(#expr)
                }
            }
            None => quote! { None },
        })
    }
}

impl<T> ToRust for Vec<T>
where
    T: ToRust,
{
    fn to_rust(self, mode: Mode, ctx: &mut CodegenCtx) -> Result<TokenStream> {
        let elements: Vec<_> = self
            .into_iter()
            .map(|e| e.to_rust(mode, ctx))
            .try_collect()?;

        Ok(quote! {
            #(
                #elements
            ),*
        })
    }
}

impl ToRust for MethodCall {
    fn to_rust(self, mode: Mode, ctx: &mut CodegenCtx) -> Result<TokenStream> {
        let MethodCall { method, args } = self;

        let args = args.to_rust(mode, ctx)?;
        Ok(quote! {
            . #method ( #args )
        })
    }
}

impl ToRust for FieldAccess {
    fn to_rust(self, _mode: Mode, _ctx: &mut CodegenCtx) -> Result<TokenStream> {
        let FieldAccess { field } = self;
        Ok(quote! {
            . #field
        })
    }
}

impl ToRust for Vec<Postfix> {
    fn to_rust(self, mode: Mode, ctx: &mut CodegenCtx) -> Result<TokenStream> {
        let mut iter = self.into_iter().peekable();
        let mut ts = TokenStream::new();
        while let Some(item) = iter.next() {
            match item {
                Postfix::FieldAccess(access) => {
                    let access = access.to_rust(mode, ctx)?;
                    ts.extend(access);
                    while let Some(Postfix::FieldAccess(access)) = iter.peek() {
                        let access = access.clone().to_rust(mode, ctx)?;
                        ts.extend(access);
                        iter.next();
                    }
                    if let Some(Postfix::MethodCall(MethodCall { method, .. })) = iter.peek() {
                        if method == "clone" {
                            continue;
                        }
                    }
                    ts.extend(quote! { .clone() });
                }
                Postfix::MethodCall(call) => {
                    let call = call.to_rust(mode, ctx)?;
                    ts.extend(call);
                }
            }
        }
        Ok(ts)
    }
}

impl ToRust for Expr {
    fn to_rust(self, mode: Mode, ctx: &mut CodegenCtx) -> Result<TokenStream> {
        let clone = ctx.clone.to_rust(mode, ctx)?;
        Ok(match self {
            Expr::RustCode(code) => {
                let code = match mode {
                    Mode::Unit => code,
                    Mode::Integration => code.replace("crate", "ccprocessor_rust"),
                };
                let ts = TokenStream::from_str(&code)
                    .map_err(|e| eyre!("failed to parse tokenstream for code {} : {}", code, e))?;
                quote! {
                    #ts
                }
            }
            Expr::Sighash(sig) => {
                let id = format_ident!("{}", sig.0);
                quote! {
                    #id #clone
                }
            }
            Expr::Default => quote! {
                ::core::default::Default::default()
            },
            Expr::Ident(ident) => quote! {
                #ident #clone
            },
            Expr::Construction { name, fields } => {
                let name = name.to_rust(mode, ctx)?;
                let fields = fields
                    .into_iter()
                    .map(|f| ToRust::to_rust(f, mode, ctx))
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
                    .map(|f| ToRust::to_rust(f, mode, ctx))
                    .collect::<Result<Vec<_>>>()?;
                quote! {
                    vec! [
                        #( #exprs ),*
                    ]
                }
            }
            Expr::Mapping(_) => todo!(),
            Expr::Literal(literal) => quote! {
                #literal
            },
            Expr::WalletId(id) => {
                let wallet_id = sig_to_walletid(id);
                quote! {
                    #wallet_id.clone()
                }
            }
            Expr::Option(opt) => match opt {
                Some(expr) => {
                    let expr = expr.to_rust(mode, ctx)?;
                    quote! {
                        Some(#expr)
                    }
                }
                None => quote! {
                    None
                },
            },
            Expr::Result(res) => match res {
                ResultExpr::Ok(expr) => {
                    let expr = expr.to_rust(mode, ctx)?;
                    quote! {
                        Ok(#expr)
                    }
                }
                ResultExpr::Err(expr) => {
                    let expr = expr.to_rust(mode, ctx)?;
                    quote! {
                        Err(#expr)
                    }
                }
            },
            Expr::GuidFor(cmd) => match cmd {
                Some(id) => {
                    let guid = command_to_guid(id);
                    quote! {
                        #guid.clone()
                    }
                }
                None => {
                    quote! {
                        Guid::from(make_nonce())
                    }
                }
            },
            Expr::SignerFor(sig) => match mode {
                Mode::Unit => quote! {},
                Mode::Integration => {
                    let signer = sig_to_signer(sig);
                    quote! {
                        #signer
                    }
                }
            },
            Expr::Postfix { value, postfix } => {
                let value = match postfix.first() {
                    Some(Postfix::FieldAccess(_)) => ctx.scoped(|ctx| {
                        ctx.clone = CloneStrategy::Nothing;
                        value.to_rust(mode, ctx)
                    }),
                    Some(Postfix::MethodCall(MethodCall { method, .. }))
                        if method == "to_state_entry" || method == "clone" =>
                    {
                        ctx.scoped(|ctx| {
                            ctx.clone = CloneStrategy::Nothing;
                            value.to_rust(mode, ctx)
                        })
                    }
                    _ => value.to_rust(mode, ctx),
                }?;
                let postfix = postfix.to_rust(mode, ctx)?;
                quote! {
                    #value #postfix
                }
            }
            Expr::Tuple(elements) => {
                let elements: Vec<_> = elements
                    .into_iter()
                    .map(|e| e.to_rust(mode, ctx))
                    .try_collect()?;

                quote! {
                    (
                        #( #elements ),*
                    )
                }
            }
            Expr::FnCall { func, args } => {
                let func = func.to_rust(mode, ctx)?;
                let args = args.to_rust(mode, ctx)?;

                quote! {
                    #func(#args)
                }
            }
            Expr::Ref { kind, expr } => {
                let refer = match kind {
                    RefKind::Mut => quote! { &mut },
                    RefKind::Immut => quote! { & },
                };
                let expr = expr.to_rust(mode, ctx)?;

                quote! {
                    #refer #expr
                }
            }
        })
    }
}

impl Pat {
    fn prefixed_with(&self, ts: TokenStream) -> TokenStream {
        match self {
            Pat::Ident(id) => quote! {
                #ts #id
            },
            Pat::Tuple(ids) => quote! {
                ( #( #ts #ids ),* )
            },
        }
    }
}

impl ToRust for Stmt {
    fn to_rust(self, mode: Mode, ctx: &mut CodegenCtx) -> Result<TokenStream> {
        Ok(match self {
            Stmt::Binding { lhs, value } => {
                let value = value.to_rust(mode, ctx)?;
                let pat = lhs.prefixed_with(quote! { mut });
                quote! {
                    let #pat = #value;
                }
            }
            Stmt::Require { requirements } => {
                let reqs = requirements
                    .into_iter()
                    .map(|f| f.to_rust(mode, ctx))
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
                    .map(|f| f.to_rust(mode, ctx))
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
                    #ts
                }
            }
            Stmt::ModeSpecific { mode: m, stmts } => {
                if mode == m {
                    stmts.to_rust(mode, ctx)?
                } else {
                    quote! {}
                }
            }
        })
    }
}

fn sig_to_walletid(sig: impl IdentFragment) -> Ident {
    format_ident!("{}_wallet_id_", sig)
}

fn command_to_guid(command: impl IdentFragment) -> Ident {
    format_ident!("{}_guid_", command)
}

impl ToRust for Requirement {
    fn to_rust(self, mode: Mode, ctx: &mut CodegenCtx) -> Result<TokenStream> {
        Ok(match mode {
            Mode::Unit => match self {
                Requirement::Wallet { sighash, amount: _ } => {
                    let id = sig_to_walletid(&sighash);
                    let sig = format_ident!("{}", sighash);
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
                Requirement::SendTx { .. } => quote! {},
            },
            Mode::Integration => match self {
                Requirement::Wallet { sighash, amount } => {
                    use rand::Rng;
                    let id = sig_to_walletid(&sighash);
                    let sig = format_ident!("{}", sighash);
                    let amount = amount.to_rust(mode, ctx)?;
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
                Requirement::Guid { id } => {
                    let id = command_to_guid(&id);
                    quote! {
                        let #id = Guid::from(make_nonce());
                    }
                }
                Requirement::SendTx { tx, signer, guid } => {
                    let tx = tx.to_rust(mode, ctx)?;
                    let signer = signer.to_rust(mode, ctx)?;
                    let guid = match guid {
                        Some(expr) => {
                            let expr = expr.to_rust(mode, ctx)?;
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

fn into_result(expr: Expr) -> Expr {
    match expr {
        Expr::Result(_) => expr,
        Expr::RustCode(_) => expr,
        e => Expr::Result(ResultExpr::Ok(Box::new(e))),
    }
}

fn into_option(expr: Expr) -> Expr {
    match expr {
        Expr::Option(_) => expr,
        Expr::RustCode(_) => expr,
        e => Expr::Option(Some(Box::new(e))),
    }
}

impl ToRust for Expectation {
    fn to_rust(self, mode: Mode, ctx: &mut CodegenCtx) -> Result<TokenStream> {
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
                    let address = id.to_rust(mode, ctx)?;
                    let ret = ret.to_rust(mode, ctx)?;

                    quote! {
                        {
                            let address = #address;
                            let ret = #ret;
                            tx_ctx.expect_get_state_entry()
                                .withf(move |addr| address.as_str() == addr)
                                .return_once(move |_| Ok( wallet_with( #casted ) ) );
                        }
                    }
                }
                Expectation::GetStateEntry { id, ret } => {
                    let id = id.to_rust(mode, ctx)?;

                    let ret = into_option(ret);

                    let ret = if let Expr::Option(o) = ret {
                        match o {
                            Some(expr) => Expr::Option(Some(expr)).to_rust(mode, ctx)?,
                            None => quote! {
                                <Option<crate::protos::Wallet>>::None
                            },
                        }
                    } else {
                        ret.to_rust(mode, ctx)?
                    };

                    quote! {
                        expect_get_state_entry(
                            &mut tx_ctx,
                            #id,
                            #ret,
                            None,
                        );

                    }
                }
                Expectation::SetStateEntry { id: _, value: _ } => todo!(),
                Expectation::DeleteStateEntry { id: _ } => todo!(),
                Expectation::DeleteStateEntries { values } => {
                    let entries: Vec<_> = values
                        .into_iter()
                        .map(|e| e.to_rust(mode, ctx))
                        .try_collect()?;
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
                Expectation::SetStateEntries { values } => {
                    let entries: Vec<_> = values
                        .into_iter()
                        .map(|e| e.to_rust(mode, ctx))
                        .try_collect()?;

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
                    let sig = sig.to_rust(mode, ctx)?;

                    quote! {
                        {
                            let sig = crate::handler::types::SigHash(#sig.to_string());

                            ctx.expect_sighash()
                                .return_once(move |_| Ok(sig));
                        }
                    }
                }
                Expectation::GetGuid { guid } => {
                    let guid = guid.to_rust(mode, ctx)?;
                    quote! {
                        {
                            let guid = #guid;

                            ctx.expect_guid()
                                .returning(move |_| guid.clone());
                        }
                    }
                }
                Expectation::Verify(ret) => {
                    let ret = into_result(ret).to_rust(mode, ctx)?;
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
                Expectation::SetStateEntry { id, value } => {
                    let id = id.to_rust(mode, ctx)?;
                    let value = value.to_rust(mode, ctx)?;

                    quote! {
                        expect_set_state_entry(#id.to_string(), #value.into()).unwrap();
                    }
                }
                Expectation::SetStateEntries { values } => {
                    let entries: Vec<_> = values
                        .into_iter()
                        .map(|e| e.to_rust(mode, ctx))
                        .try_collect()?;

                    quote! {
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
                Expectation::DeleteStateEntry { id: _ } => todo!(),
                Expectation::DeleteStateEntries { values } => {
                    let entries: Vec<_> = values
                        .into_iter()
                        .map(|e| e.to_rust(mode, ctx))
                        .try_collect()?;

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
                | Expectation::GetStateEntry { .. }
                | Expectation::GetSighash { .. }
                | Expectation::GetGuid { .. }
                | Expectation::Verify(_) => quote! {},
            },
        })
    }
}

impl ToRust for MapEntry {
    fn to_rust(self, mode: Mode, ctx: &mut CodegenCtx) -> Result<TokenStream> {
        Ok(match self {
            MapEntry::Pair(key, value) => {
                let key = key.to_rust(mode, ctx)?;
                let value = value.to_rust(mode, ctx)?;

                quote! {
                    (#key.to_string(), #value.into())
                }
            }
            MapEntry::Single(value) => {
                let value = value.to_rust(mode, ctx)?;

                quote! {
                    #value
                }
            }
        })
    }
}

impl ToRust for StructEntry {
    fn to_rust(self, mode: Mode, ctx: &mut CodegenCtx) -> Result<TokenStream> {
        Ok(match self {
            StructEntry::Pair(Field { name, value }) => {
                let value = value.to_rust(mode, ctx)?;

                quote! {
                    #name: #value
                }
            }
            StructEntry::Single(value) => {
                let value_ts = value.clone().to_rust(mode, ctx)?;

                if let Expr::Ident(id) = value {
                    quote! {
                        #id : #value_ts .clone()
                    }
                } else {
                    quote! {
                        #value_ts
                    }
                }
            }
            StructEntry::Update(value) => {
                let value = value.to_rust(mode, ctx)?;

                quote! {
                    ..#value
                }
            }
        })
    }
}

fn new_secret(rng: &mut impl rand::RngCore) -> String {
    use libsecp256k1::SecretKey;
    let secret = SecretKey::random(rng);
    format!("{:x}", secret)
}

impl ToRust for Descriptor {
    fn to_rust(self, mode: Mode, ctx: &mut CodegenCtx) -> Result<TokenStream> {
        let descriptor = self;
        if let Mode::Unit = mode {
            let name = descriptor.name.to_snake_case().to_lowercase();
            let test_name = format_ident!("{}", name);

            let imports = quote! {
                use crate::handler::types::*;
                use std::str::FromStr as _;
            };

            let fns = quote! {};

            let sighashes = descriptor.sighashes.clone();
            let sighash_ids: Vec<Ident> =
                sighashes.iter().map(|i| format_ident!("{}", i)).collect();
            let signers: Vec<Ident> = sighashes.iter().map(sig_to_signer).collect();
            let secrets: Vec<String> = sighashes.iter().map(|_| new_secret(&mut ctx.rng)).collect();
            let sighash_decls = quote! {
                #(
                    let #signers = signer_with_secret(#secrets);
                    let #sighash_ids = SigHash::from(&#signers);
                )*
            };
            let tx_fee = descriptor.tx_fee;
            let tx_fee_decl = if let Some(expr) = tx_fee {
                let tx_fee = expr.to_rust(mode, ctx)?;
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
                let request = expr.to_rust(mode, ctx)?;
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
                .map(|f| ToRust::to_rust(f, mode, ctx))
                .try_collect()?;

            let execute = if let TestKind::Fail(err) = descriptor.pass_fail {
                let err = err.to_rust(mode, ctx)?;
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
                use std::str::FromStr as _;
                use std::convert::TryFrom as _;
            };

            let fns = quote! {};

            let sighashes = descriptor.sighashes.clone();
            let sighash_ids: Vec<Ident> =
                sighashes.iter().map(|i| format_ident!("{}", i)).collect();
            let signers: Vec<Ident> = sighashes.iter().map(sig_to_signer).collect();
            let secrets: Vec<String> = sighashes.iter().map(|_| new_secret(&mut ctx.rng)).collect();
            let sighash_decls = quote! {
                #(
                    let #signers = signer_with_secret(#secrets);
                    let #sighash_ids = SigHash::from(&#signers);
                )*
            };
            let tx_fee = descriptor.tx_fee;
            let tx_fee_decl = if let Some(expr) = tx_fee {
                let tx_fee = expr.to_rust(mode, ctx)?;
                quote! {
                    let mut tx_fee = #tx_fee;
                }
            } else {
                quote! {
                    let mut tx_fee = ccprocessor_rust::handler::constants::TX_FEE.clone();
                }
            };
            let request = descriptor.request;
            let request_decl = if let Some(expr) = request {
                let request = expr.to_rust(mode, ctx)?;
                quote! {
                    let mut request = #request;
                }
            } else {
                quote! {
                    let mut request = TpProcessRequest::default();
                }
            };

            let (expectations, stmts): (Vec<_>, Vec<_>) = descriptor
                .stmts
                .into_iter()
                .partition(|s| matches!(s, Stmt::Expect { .. }));
            let expectations: Vec<_> = expectations
                .into_iter()
                .map(|f| ToRust::to_rust(f, mode, ctx))
                .try_collect()?;
            let stmts: Vec<_> = stmts
                .into_iter()
                .map(|f| ToRust::to_rust(f, mode, ctx))
                .try_collect()?;

            let signer = descriptor.signer.to_rust(mode, ctx)?;
            let guid = command_to_guid("command");
            let execute = if let TestKind::Fail(err) = descriptor.pass_fail {
                let err = err.to_rust(mode, ctx)?;
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
                #fns
                setup_logs();

            };
            let body = quote! {
                #sighash_decls
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
