#![allow(unused)]

use cctestgen_internal_macros::Foldable;
use derive_syn_parse::Parse;
use quote::IdentFragment;
use syn::{
    braced, bracketed,
    fold::{fold_ident, Fold},
    parse::{Parse, ParseStream},
    punctuated::Punctuated,
    token::{self, Bracket},
    Block, Expr, ExprArray, FieldValue, Ident, LitStr, Stmt, Token,
};

mod kw {
    syn::custom_keyword!(at);
    syn::custom_keyword!(rust);
    syn::custom_keyword!(signer);
    syn::custom_keyword!(tx_fee);
    syn::custom_keyword!(request);
    syn::custom_keyword!(command);
    syn::custom_keyword!(name);
    syn::custom_keyword!(result);
    syn::custom_keyword!(meta);
    syn::custom_keyword!(pass);
    syn::custom_keyword!(fail);
    syn::custom_keyword!(err);
    syn::custom_keyword!(default);
    syn::custom_keyword!(require);
    syn::custom_keyword!(expect);
    syn::custom_keyword!(send);
    syn::custom_keyword!(transaction);
    syn::custom_keyword!(Wallet);
    syn::custom_keyword!(Guid);
    syn::custom_keyword!(with);
    syn::custom_keyword!(amount);
    syn::custom_keyword!(guid);
    syn::custom_keyword!(get);
    syn::custom_keyword!(set);
    syn::custom_keyword!(delete);
    syn::custom_keyword!(balance);
    syn::custom_keyword!(state);
    syn::custom_keyword!(states);
    syn::custom_keyword!(sighash);
    syn::custom_keyword!(sighashes);
    syn::custom_keyword!(verify);
    syn::custom_keyword!(integration);
    syn::custom_keyword!(unit);
}

syn::custom_punctuation!(TripleDash, ---);

#[derive(Debug, Foldable)]
pub(crate) struct Descriptors {
    #[foldable_iter]
    pub(crate) descriptors: Vec<Descriptor>,
}

impl Parse for Descriptors {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut descriptors = Vec::new();
        while !input.is_empty() {
            let descriptor = input.parse()?;
            log::debug!("Parsed descriptor {:?}", descriptor);
            descriptors.push(descriptor);

            if input.is_empty() {
                break;
            } else {
                let _triple_dash: TripleDash = input.parse()?;
            }
        }
        Ok(Self { descriptors })
    }
}

#[derive(Debug, Foldable)]
pub(crate) struct Descriptor {
    #[foldable_iter]
    pub(crate) statements: Vec<Statement>,
}

impl Parse for Descriptor {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut statements = Vec::new();
        while !(input.is_empty() || input.peek(TripleDash)) {
            let stmt = input.parse()?;
            log::info!("Parsed statement: {:?}", stmt);
            statements.push(stmt);
        }
        Ok(Self { statements })
    }
}

#[derive(Debug, Parse, Foldable, Clone, PartialEq)]
pub(crate) struct RustBlock {
    pub(crate) rust_token: kw::rust,
    pub(crate) block: Block,
}

#[derive(Debug, Parse)]
pub(crate) struct IntegrationBlock {
    pub(crate) integration_token: kw::integration,

    pub(crate) block: StmtBlock,
}

#[derive(Debug, Foldable)]
pub(crate) struct StmtBlock {
    pub(crate) brace_token: token::Brace,
    #[foldable_iter]
    pub(crate) stmts: Vec<Statement>,
}

impl StmtBlock {
    fn parse_within(input: ParseStream) -> syn::Result<Vec<Statement>> {
        let mut stmts = Vec::new();
        loop {
            while let Some(semi) = input.parse::<Option<Token![;]>>()? {
                stmts.push(Statement::Rust(Stmt::Semi(
                    Expr::Verbatim(proc_macro2::TokenStream::new()),
                    semi,
                )));
            }
            if input.is_empty() {
                break;
            }
            let s = input.parse()?;
            stmts.push(s);
        }
        Ok(stmts)
    }
}

impl Parse for StmtBlock {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let content;
        Ok(StmtBlock {
            brace_token: braced!(content in input),
            stmts: content.call(StmtBlock::parse_within)?,
        })
    }
}

#[derive(Debug, Foldable)]
pub(crate) enum Statement {
    #[foldable]
    Expect(ExpectStmt),
    #[foldable]
    Verbatim(RustBlock),
    #[foldable]
    Integration(StmtBlock),
    #[foldable]
    Unit(StmtBlock),
    #[foldable]
    Require(RequireStmt),
    #[fold(stmt)]
    Rust(Stmt),
    #[foldable]
    Meta(Meta),
}

impl Parse for Statement {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let lookahead = input.lookahead1();
        if lookahead.peek(kw::expect) {
            Ok(Statement::Expect(input.parse()?))
        } else if lookahead.peek(kw::rust) {
            Ok(Statement::Verbatim(input.parse()?))
        } else if lookahead.peek(kw::integration) {
            Ok(Statement::Integration(input.parse()?))
        } else if lookahead.peek(kw::unit) {
            Ok(Statement::Unit(input.parse()?))
        } else if lookahead.peek(kw::require) {
            Ok(Statement::Require(input.parse()?))
        } else if lookahead.peek(kw::meta) {
            let _meta: kw::meta = input.parse()?;
            Ok(Statement::Meta(input.parse()?))
        } else {
            Ok(Statement::Rust(input.parse()?))
        }
    }
}

#[derive(Debug, Parse, Foldable, Clone, PartialEq)]
pub(crate) enum Meta {
    #[peek(kw::command, name = "Command")]
    Command(CommandMeta),
    #[peek(kw::guid, name = "Guid")]
    Guid(GuidMeta),
    #[peek(kw::tx_fee, name = "Transaction fee")]
    TxFee(TxFeeMeta),
    #[peek(kw::result, name = "Test type")]
    TestType(TestTypeMeta),
    #[peek(kw::request, name = "Request")]
    Request(RequestMeta),
    #[peek(kw::signer, name = "Signer")]
    Signer(SignerMeta),
    #[peek(kw::sighashes, name = "SigHashes")]
    SigHashes(SigHashesMeta),
    #[peek(kw::name, name = "Test name")]
    TestName(TestNameMeta),
}

#[derive(Debug, Foldable, Clone, PartialEq)]
pub(crate) enum Expectation {
    GetBalance {
        get_token: kw::get,
        balance_token: kw::balance,
        at_token: kw::at,
        #[fold(expr)]
        address_expr: Expr,
        arrow_token: Token![->],
        #[fold(expr)]
        return_expr: Expr,
    },
    GetState {
        get_token: kw::get,
        state_token: kw::state,
        at_token: kw::at,
        #[fold(expr)]
        address_expr: Expr,
        arrow_token: Token![->],
        #[fold(expr)]
        return_expr: Expr,
    },
    SetState {
        set_token: kw::set,
        state_token: kw::state,
        at_token: kw::at,
        #[fold(expr)]
        address_expr: Expr,
        arrow_token: Token![->],
        #[fold(expr)]
        return_expr: Expr,
    },
    SetStates {
        set_token: kw::set,
        states_token: kw::states,
        #[foldable]
        mapping: StateMapping,
    },
    GetSigHash {
        sighash_token: kw::sighash,
        arrow_token: Token![->],
        #[fold(expr)]
        return_expr: Expr,
    },
    GetGuid {
        guid_token: kw::guid,
        arrow_token: Token![->],
        #[fold(expr)]
        return_expr: Expr,
    },
    DeleteState {
        delete_token: kw::delete,
        state_token: kw::state,
        at_token: kw::at,
        #[fold(expr)]
        address_expr: Expr,
    },
    DeleteStates {
        delete_token: kw::delete,
        states_token: kw::states,
        #[fold(expr_array)]
        addresses: ExprArray,
    },
    Verify {
        verify_token: kw::verify,
        arrow_token: Token![->],
        #[fold(expr)]
        return_expr: Expr,
    },
}

impl Parse for Expectation {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let lookahead = input.lookahead1();
        if lookahead.peek(kw::get) {
            let get_token: kw::get = input.parse()?;
            let lookahead = input.lookahead1();
            if lookahead.peek(kw::balance) {
                let balance_token = input.parse()?;
                let at_token = input.parse()?;
                let address_expr = input.parse()?;
                let arrow_token = input.parse()?;
                let return_expr = input.parse()?;
                Ok(Expectation::GetBalance {
                    get_token,
                    balance_token,
                    at_token,
                    address_expr,
                    arrow_token,
                    return_expr,
                })
            } else if lookahead.peek(kw::state) {
                let state_token = input.parse()?;
                let at_token = input.parse()?;
                let address_expr = input.parse()?;
                let arrow_token = input.parse()?;
                let return_expr = input.parse()?;
                Ok(Expectation::GetState {
                    get_token,
                    state_token,
                    at_token,
                    address_expr,
                    arrow_token,
                    return_expr,
                })
            } else {
                Err(lookahead.error())
            }
        } else if lookahead.peek(kw::set) {
            let set_token = input.parse()?;
            let lookahead = input.lookahead1();
            if lookahead.peek(kw::state) {
                let state_token = input.parse()?;
                let at_token = input.parse()?;
                let address_expr = input.parse()?;
                let arrow_token = input.parse()?;
                let return_expr = input.parse()?;
                Ok(Expectation::SetState {
                    set_token,
                    state_token,
                    at_token,
                    address_expr,
                    arrow_token,
                    return_expr,
                })
            } else if lookahead.peek(kw::states) {
                let states_token = input.parse()?;
                let mapping = input.parse()?;
                Ok(Expectation::SetStates {
                    set_token,
                    states_token,
                    mapping,
                })
            } else {
                Err(lookahead.error())
            }
        } else if lookahead.peek(kw::delete) {
            let delete_token = input.parse()?;
            let lookahead = input.lookahead1();
            if lookahead.peek(kw::state) {
                let state_token = input.parse()?;
                let at_token = input.parse()?;
                let address_expr = input.parse()?;
                Ok(Expectation::DeleteState {
                    delete_token,
                    state_token,
                    at_token,
                    address_expr,
                })
            } else if lookahead.peek(kw::states) {
                let states_token = input.parse()?;
                let addresses = input.parse()?;
                Ok(Expectation::DeleteStates {
                    delete_token,
                    states_token,
                    addresses,
                })
            } else {
                Err(lookahead.error())
            }
        } else if lookahead.peek(kw::guid) {
            let guid_token = input.parse()?;
            let arrow_token = input.parse()?;
            let return_expr = input.parse()?;
            Ok(Expectation::GetGuid {
                guid_token,
                arrow_token,
                return_expr,
            })
        } else if lookahead.peek(kw::sighash) {
            let sighash_token = input.parse()?;
            let arrow_token = input.parse()?;
            let return_expr = input.parse()?;
            Ok(Expectation::GetSigHash {
                sighash_token,
                arrow_token,
                return_expr,
            })
        } else if lookahead.peek(kw::verify) {
            let verify_token = input.parse()?;
            let arrow_token = input.parse()?;
            let return_expr = input.parse()?;
            Ok(Expectation::Verify {
                verify_token,
                arrow_token,
                return_expr,
            })
        } else {
            Err(lookahead.error())
        }
    }
}

#[derive(Debug, Parse, Foldable, Clone, PartialEq)]
pub(crate) struct StateMapping {
    #[brace]
    pub(crate) brace_token: token::Brace,
    #[fold_iter(field_value)]
    #[inside(brace_token)]
    #[call(Punctuated::parse_terminated)]
    pub(crate) fields: Punctuated<FieldValue, Token![,]>,
    pub(crate) dot2_token: Option<Token![..]>,
    #[fold(expr)]
    #[parse_if(dot2_token.is_some())]
    pub(crate) rest: Option<Expr>,
}

#[derive(Debug, Parse, Foldable, Clone, PartialEq)]
pub(crate) enum Requirement {
    #[peek(kw::Wallet, name = "Wallet requirement")]
    Wallet {
        wallet_token: kw::Wallet,
        for_token: Token![for],
        #[fold(ident)]
        ident: Ident,
        with_token: kw::with,
        amount_token: kw::amount,
        eq_token: Token![=],
        #[fold(expr)]
        expr: Expr,
    },
    #[peek(kw::send, name = "SendTx")]
    SendTx {
        send_token: kw::send,
        tx_token: kw::transaction,
        eq_token: Option<Token![=]>,
        tx: Expr,
        with_token: kw::with,
        signer_token: kw::signer,
        eq_token2: Option<Token![=]>,
        signer: Expr,
        #[peek(Token![,])]
        guid: Option<Prefixed<GuidMeta, Token![,]>>,
    },
}

#[derive(Debug)]
pub(crate) struct Prefixed<T, Prefix> {
    pub(crate) prefix: Prefix,
    pub(crate) root: T,
}

impl<T, Prefix> Foldable for Prefixed<T, Prefix>
where
    T: Foldable,
    Prefix: Foldable,
{
    fn fold_with<F>(self, folder: &mut F) -> Self
    where
        F: Fold + ?Sized,
    {
        let Self { prefix, root } = self;
        Self {
            prefix: prefix.fold_with(folder),
            root: root.fold_with(folder),
        }
    }
}

impl<T, Prefix> Parse for Prefixed<T, Prefix>
where
    T: Parse,
    Prefix: Parse,
{
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(Self {
            prefix: input.parse()?,
            root: input.parse()?,
        })
    }
}

impl<T, Prefix> Clone for Prefixed<T, Prefix>
where
    T: Clone,
    Prefix: Clone,
{
    fn clone(&self) -> Self {
        Self {
            prefix: self.prefix.clone(),
            root: self.root.clone(),
        }
    }
}

impl<T, Prefix> PartialEq for Prefixed<T, Prefix>
where
    T: PartialEq,
    Prefix: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        self.prefix == other.prefix && self.root == other.root
    }
}

#[derive(Debug, Parse, Foldable, Clone, PartialEq)]
pub(crate) struct ExpectStmt {
    expect_token: kw::expect,
    #[paren]
    paren_token: token::Paren,
    #[foldable_iter]
    #[inside(paren_token)]
    #[call(Punctuated::parse_terminated)]
    expectations: Punctuated<Expectation, Token![,]>,
}

#[derive(Debug, Parse, Foldable, Clone, PartialEq)]
pub(crate) struct RequireStmt {
    pub(crate) require_token: kw::require,
    #[paren]
    pub(crate) paren_token: token::Paren,
    #[foldable_iter]
    #[inside(paren_token)]
    #[call(Punctuated::parse_terminated)]
    pub(crate) requirements: Punctuated<Requirement, Token![,]>,
}

#[derive(Debug, Parse, Foldable, Clone, PartialEq)]
pub(crate) struct MetaItem<Name: Parse> {
    pub(crate) name_token: Name,
    pub(crate) equals_token: token::Eq,
    #[fold(expr)]
    pub(crate) value: Expr,
    pub(crate) semi: Option<token::Semi>,
}

#[derive(Debug, Parse, Foldable, Clone, PartialEq)]
pub(crate) struct TestNameMeta {
    pub(crate) name_token: kw::name,
    pub(crate) equals_token: Token![=],
    #[fold(lit_str)]
    pub(crate) value: LitStr,
}

#[derive(Debug, Parse, Foldable, Clone, PartialEq)]
pub(crate) struct SigHashesMeta {
    pub(crate) name_token: kw::sighashes,
    pub(crate) equals_token: Token![=],
    #[bracket]
    pub(crate) bracket_token: Bracket,
    #[fold_iter(ident)]
    #[inside(bracket_token)]
    #[call(Punctuated::parse_terminated)]
    pub(crate) idents: Punctuated<Ident, Token![,]>,
}

#[derive(Debug, Parse, Foldable, Clone, PartialEq)]
pub(crate) struct CommandMeta(MetaItem<kw::command>);

#[derive(Debug, Parse, Foldable, Clone, PartialEq)]
pub(crate) struct SignerMeta(MetaItem<kw::signer>);

#[derive(Debug, Parse, Foldable, Clone, PartialEq)]
pub(crate) struct TxFeeMeta(MetaItem<kw::tx_fee>);

#[derive(Debug, Parse, Foldable, Clone, PartialEq)]
pub(crate) struct RequestMeta(MetaItem<kw::request>);

#[derive(Debug, Parse, Foldable, Clone, PartialEq)]
pub(crate) struct GuidMeta(#[foldable] MetaItem<kw::guid>);

#[derive(Debug, Parse, Foldable, Clone, PartialEq)]
pub(crate) enum PassFail {
    #[peek(kw::pass, name = "pass")]
    Pass,
    #[peek(kw::fail, name = "fail")]
    Fail {
        err_token: kw::err,
        equals_token: Token![=],
        #[fold(expr)]
        err: Expr,
    },
}

#[derive(Debug, Parse, Foldable, Clone, PartialEq)]
pub(crate) struct TestTypeMeta {
    pub(crate) result_token: kw::result,
    pub(crate) equals_token: Token![=],
    #[foldable]
    pub(crate) value: PassFail,
}

pub(crate) trait Foldable {
    fn fold_with<F>(self, folder: &mut F) -> Self
    where
        F: Fold + ?Sized;
}
