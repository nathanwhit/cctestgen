#![allow(unused)]

use std::ops::Range;

use cctestgen_internal_macros::{Foldable, Visitable};
use derive_syn_parse::Parse;
use proc_macro2::LineColumn;
use quote::IdentFragment;
use syn::{
    braced, bracketed,
    fold::{fold_ident, Fold},
    parse::{Parse, ParseStream},
    punctuated::Punctuated,
    spanned::Spanned,
    token::{self, Bracket},
    visit::Visit,
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

pub(crate) struct Parser<'a> {
    contents: &'a str,
}

impl<'a> Parser<'a> {
    pub(crate) fn parse(contents: &'a str) -> color_eyre::Result<Descriptors> {
        let f: proc_macro2::TokenStream = std::str::FromStr::from_str(contents)
            .map_err(|e| color_eyre::eyre::eyre!("{:?}", e))?;
        let result: syn::Result<Descriptors> = syn::parse2(f);
        match result {
            Ok(descriptors) => Ok(descriptors),
            Err(e) => {
                let parser_span = Span::from_span(&contents, e.span());
                ariadne::Report::build(ariadne::ReportKind::Error, (), parser_span.lo)
                    .with_message(e)
                    .with_label(
                        ariadne::Label::new(parser_span.lo..parser_span.hi)
                            .with_message("At this location"),
                    )
                    .finish()
                    .eprint(ariadne::Source::from(&contents))?;
                Err(color_eyre::eyre::eyre!("Error occurred during parsing"))
            }
        }
    }
}

#[derive(Debug, Foldable, Visitable)]
pub(crate) struct Descriptors {
    #[ast_iter]
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

#[derive(Debug)]
pub(crate) struct Descriptor {
    body: Vec<MetaOrStatement>,
}

impl Visitable for Descriptor {
    fn visit_with<'ast, V>(&'ast self, visitor: &mut V)
    where
        V: Visit<'ast> + AstVisit<'ast> + ?Sized,
    {
        todo!()
    }
}

impl Foldable for Descriptor {
    fn fold_with<F>(self, folder: &mut F) -> Self
    where
        F: Fold + ?Sized,
    {
        todo!()
    }
}

impl<'a, T> Visitable for &'a T
where
    T: Visitable,
{
    fn visit_with<'ast, V>(&'ast self, visitor: &mut V)
    where
        V: Visit<'ast> + AstVisit<'ast> + ?Sized,
    {
        (*self).visit_with(visitor)
    }
}

impl Descriptor {
    pub(crate) fn meta(&self) -> Vec<Meta> {
        self.body
            .iter()
            .filter_map(|s| match s {
                MetaOrStatement::Meta(meta) => Some(meta.clone()),
                _ => None,
            })
            .collect()
    }
    pub(crate) fn statements(&self) -> Vec<Statement> {
        self.body
            .iter()
            .filter_map(|s| match s {
                MetaOrStatement::Statement(statement) => Some(statement.clone()),
                _ => None,
            })
            .collect()
    }
}

impl Parse for Descriptor {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut statements = Vec::new();
        while !(input.is_empty() || input.peek(TripleDash)) {
            let stmt = input.parse()?;
            log::info!("Parsed statement: {:?}", stmt);
            statements.push(stmt);
        }
        Ok(Self { body: statements })
    }
}

#[derive(Debug, Parse, Foldable, Visitable, Clone, PartialEq)]
pub(crate) struct RustBlock {
    pub(crate) rust_token: kw::rust,
    #[syn(block)]
    pub(crate) block: Block,
}

#[derive(Debug, Foldable, Visitable, Clone)]
pub(crate) struct StmtBlock {
    pub(crate) brace_token: token::Brace,
    #[ast_iter(statement)]
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

#[derive(Debug, Clone, Foldable, Visitable)]
pub(crate) enum MetaOrStatement {
    #[ast]
    Meta(Meta),
    #[ast(statement)]
    Statement(Statement),
}

impl Parse for MetaOrStatement {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let lookahead = input.lookahead1();
        if lookahead.peek(kw::meta) {
            Ok(MetaOrStatement::Meta(input.parse()?))
        } else {
            Ok(MetaOrStatement::Statement(input.parse()?))
        }
    }
}

#[derive(Debug, Foldable, Visitable, Clone)]
pub(crate) enum Statement {
    #[ast]
    Expect(ExpectStmt),
    #[ast]
    Verbatim(RustBlock),
    #[ast]
    Integration(StmtBlock),
    #[ast]
    Unit(StmtBlock),
    #[ast]
    Require(RequireStmt),
    #[syn(stmt)]
    Rust(Stmt),
}

impl Parse for Statement {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        log::info!("Parsing statement: {}", input);
        let lookahead = input.lookahead1();
        let stmt = if lookahead.peek(kw::expect) {
            Ok(Statement::Expect(input.parse()?))
        } else if lookahead.peek(kw::rust) {
            Ok(Statement::Verbatim(input.parse()?))
        } else if lookahead.peek(kw::integration) {
            Ok(Statement::Integration(input.parse()?))
        } else if lookahead.peek(kw::unit) {
            Ok(Statement::Unit(input.parse()?))
        } else if lookahead.peek(kw::require) {
            Ok(Statement::Require(input.parse()?))
        } else {
            Ok(Statement::Rust(input.parse()?))
        };
        if input.peek(token::Semi) {
            let _semi: token::Semi = input.parse()?;
        }
        stmt
    }
}

#[derive(Debug, Parse, Foldable, Visitable, Clone, PartialEq)]
pub(crate) enum Meta {
    #[ast]
    #[peek(kw::command, name = "Command")]
    Command(CommandMeta),
    #[ast]
    #[peek(kw::guid, name = "Guid")]
    Guid(GuidMeta),
    #[ast]
    #[peek(kw::tx_fee, name = "Transaction fee")]
    TxFee(TxFeeMeta),
    #[ast]
    #[peek(kw::result, name = "Test type")]
    TestType(TestTypeMeta),
    #[ast]
    #[peek(kw::request, name = "Request")]
    Request(RequestMeta),
    #[ast]
    #[peek(kw::signer, name = "Signer")]
    Signer(SignerMeta),
    #[ast]
    #[peek(kw::sighashes, name = "SigHashes")]
    SigHashes(SigHashesMeta),
    #[peek(kw::name, name = "Test name")]
    TestName(TestNameMeta),
}

#[derive(Debug, Foldable, Visitable, Clone, PartialEq)]
pub(crate) enum Expectation {
    GetBalance {
        get_token: kw::get,
        balance_token: kw::balance,
        at_token: kw::at,
        #[syn(expr)]
        address_expr: Expr,
        arrow_token: Token![=>],
        #[syn(expr)]
        return_expr: Expr,
    },
    GetState {
        get_token: kw::get,
        state_token: kw::state,
        at_token: kw::at,
        #[syn(expr)]
        address_expr: Expr,
        arrow_token: Token![=>],
        #[syn(expr)]
        return_expr: Expr,
    },
    SetState {
        set_token: kw::set,
        state_token: kw::state,
        at_token: kw::at,
        #[syn(expr)]
        address_expr: Expr,
        arrow_token: Token![=>],
        #[syn(expr)]
        return_expr: Expr,
    },
    SetStates {
        set_token: kw::set,
        states_token: kw::states,
        #[ast]
        mapping: StateMapping,
    },
    GetSigHash {
        sighash_token: kw::sighash,
        arrow_token: Token![=>],
        #[syn(expr)]
        return_expr: Expr,
    },
    GetGuid {
        guid_token: kw::guid,
        arrow_token: Token![=>],
        #[syn(expr)]
        return_expr: Expr,
    },
    DeleteState {
        delete_token: kw::delete,
        state_token: kw::state,
        at_token: kw::at,
        #[syn(expr)]
        address_expr: Expr,
    },
    DeleteStates {
        delete_token: kw::delete,
        states_token: kw::states,
        #[syn(expr_array)]
        addresses: ExprArray,
    },
    Verify {
        verify_token: kw::verify,
        arrow_token: Token![=>],
        #[syn(expr)]
        return_expr: Expr,
    },
}

impl Parse for Expectation {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        log::debug!("Parsing expectation: {}", input);
        let lookahead = input.lookahead1();
        if lookahead.peek(kw::get) {
            let get_token: kw::get = input.parse()?;
            let lookahead = input.lookahead1();
            if lookahead.peek(kw::balance) {
                let balance_token = input.parse()?;
                let at_token = input.parse()?;
                log::debug!("Parsed to at: {}", input);
                let address_expr = input.parse()?;
                log::trace!("Parsed address expr: {:?}", address_expr);
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
                log::trace!("Parsing SetStates : {}", input);
                let states_token: kw::states = input.parse()?;
                log::trace!("Parsing mapping : {:?}", states_token);
                log::trace!("{:?}", states_token.span());
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

#[derive(Debug, Parse, Foldable, Visitable, Clone, PartialEq)]
pub(crate) struct StateMapping {
    #[brace]
    pub(crate) brace_token: token::Brace,
    #[ast_iter]
    #[inside(brace_token)]
    #[call(Punctuated::parse_terminated)]
    pub(crate) fields: Punctuated<StateEntry, Token![,]>,
    pub(crate) dot2_token: Option<Token![..]>,
    #[syn(expr)]
    #[parse_if(dot2_token.is_some())]
    pub(crate) rest: Option<Expr>,
}

#[derive(Debug, Parse, Foldable, Visitable, Clone, PartialEq)]
pub(crate) struct StateEntry {
    #[syn(expr)]
    key: Expr,
    arrow_token: Option<Token![=>]>,
    #[syn(expr)]
    #[parse_if(arrow_token.is_some())]
    value: Option<Expr>,
}

#[derive(Debug, Parse, Foldable, Visitable, Clone, PartialEq)]
pub(crate) enum Requirement {
    #[peek(kw::Wallet, name = "Wallet requirement")]
    Wallet {
        wallet_token: kw::Wallet,
        for_token: Token![for],
        #[syn(ident)]
        ident: Ident,
        with_token: kw::with,
        amount_token: kw::amount,
        eq_token: Token![=],
        #[syn(expr)]
        expr: Expr,
    },
    #[peek(kw::send, name = "SendTx")]
    SendTx {
        send_token: kw::send,
        tx_token: kw::transaction,
        eq_token: Option<Token![=]>,
        #[syn(expr)]
        tx: Expr,
        with_token: kw::with,
        signer_token: kw::signer,
        eq_token2: Option<Token![=]>,
        #[syn(expr)]
        signer: Expr,
        #[ast]
        #[peek(Token![,])]
        guid: Option<CommaThenGuid>,
    },
}

#[derive(Debug, Parse, Foldable, Visitable, Clone, PartialEq)]
pub(crate) struct CommaThenGuid {
    comma: Token![,],
    #[ast]
    guid: GuidMeta,
}

#[derive(Debug, Parse, Foldable, Visitable, Clone, PartialEq)]
pub(crate) struct ExpectStmt {
    expect_token: kw::expect,
    #[paren]
    paren_token: token::Paren,
    #[ast_iter(expectation)]
    #[inside(paren_token)]
    #[call(Punctuated::parse_terminated)]
    expectations: Punctuated<Expectation, Token![,]>,
}

#[derive(Debug, Parse, Foldable, Visitable, Clone, PartialEq)]
pub(crate) struct RequireStmt {
    pub(crate) require_token: kw::require,
    #[paren]
    pub(crate) paren_token: token::Paren,
    #[ast_iter(requirement)]
    #[inside(paren_token)]
    #[call(Punctuated::parse_terminated)]
    pub(crate) requirements: Punctuated<Requirement, Token![,]>,
}

#[derive(Debug, Parse, Foldable, Visitable, Clone, PartialEq)]
pub(crate) struct MetaItem<Name: Parse> {
    pub(crate) name_token: Name,
    pub(crate) equals_token: token::Eq,
    #[syn(expr)]
    pub(crate) value: Expr,
    pub(crate) semi: Option<token::Semi>,
}

#[derive(Debug, Parse, Foldable, Visitable, Clone, PartialEq)]
pub(crate) struct TestNameMeta {
    pub(crate) name_token: kw::name,
    pub(crate) equals_token: Token![=],
    #[syn(lit_str)]
    pub(crate) value: LitStr,
}

#[derive(Debug, Parse, Foldable, Visitable, Clone, PartialEq)]
pub(crate) struct SigHashesMeta {
    pub(crate) name_token: kw::sighashes,
    pub(crate) equals_token: Token![=],
    #[bracket]
    pub(crate) bracket_token: Bracket,
    #[syn_iter(ident)]
    #[inside(bracket_token)]
    #[call(Punctuated::parse_terminated)]
    pub(crate) idents: Punctuated<Ident, Token![,]>,
}

#[derive(Debug, Parse, Foldable, Visitable, Clone, PartialEq)]
pub(crate) struct CommandMeta(pub(crate) MetaItem<kw::command>);

#[derive(Debug, Parse, Foldable, Visitable, Clone, PartialEq)]
pub(crate) struct SignerMeta(pub(crate) MetaItem<kw::signer>);

#[derive(Debug, Parse, Foldable, Visitable, Clone, PartialEq)]
pub(crate) struct TxFeeMeta(pub(crate) MetaItem<kw::tx_fee>);

#[derive(Debug, Parse, Foldable, Visitable, Clone, PartialEq)]
pub(crate) struct RequestMeta(pub(crate) MetaItem<kw::request>);

#[derive(Debug, Parse, Foldable, Visitable, Clone, PartialEq)]
pub(crate) struct GuidMeta(pub(crate) MetaItem<kw::guid>);

#[derive(Debug, Parse, Foldable, Visitable, Clone, PartialEq)]
pub(crate) enum PassFail {
    #[peek(kw::pass, name = "pass")]
    Pass { pass_token: kw::pass },
    #[peek(kw::fail, name = "fail")]
    Fail {
        fail_token: kw::fail,
        err_token: kw::err,
        equals_token: Token![=],
        #[syn(expr)]
        err: Expr,
    },
}

#[derive(Debug, Parse, Foldable, Visitable, Clone, PartialEq)]
pub(crate) struct TestTypeMeta {
    pub(crate) result_token: kw::result,
    pub(crate) equals_token: Token![=],
    #[ast]
    pub(crate) value: PassFail,
}

pub(crate) trait Foldable {
    fn fold_with<F>(self, folder: &mut F) -> Self
    where
        F: Fold + ?Sized;
}

pub(crate) trait Visitable {
    fn visit_with<'ast, V>(&'ast self, visitor: &mut V)
    where
        V: Visit<'ast> + AstVisit<'ast> + ?Sized;
}

impl<T> Visitable for Vec<T>
where
    T: Visitable,
{
    fn visit_with<'ast, V>(&'ast self, visitor: &mut V)
    where
        V: Visit<'ast> + AstVisit<'ast> + ?Sized,
    {
        self.iter().for_each(|node| node.visit_with(visitor))
    }
}

pub(crate) trait AstVisit<'ast>: Visit<'ast> {
    fn visit_statement(&mut self, statement: &'ast Statement) {
        statement.visit_with(self);
    }
    fn visit_expectation(&mut self, expectation: &'ast Expectation) {
        expectation.visit_with(self);
    }
    fn visit_requirement(&mut self, requirement: &'ast Requirement) {
        requirement.visit_with(self);
    }
}

pub(crate) trait HasSpan {
    fn span(&self) -> Span;
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub(crate) struct Span {
    pub(crate) lo: usize,
    pub(crate) hi: usize,
}

fn find_offset(input: &str, line_no: usize, idx: usize, line_col: LineColumn) -> (usize, usize) {
    let mut idx = idx;
    let mut line_no = line_no;
    while line_no < line_col.line {
        if &input[idx..idx + 1] == "\n" {
            line_no += 1;
        }
        idx += 1;
    }
    (line_no, idx + line_col.column)
}

impl Span {
    pub(crate) fn from_span(input: &str, span: proc_macro2::Span) -> Self {
        let (line_no, start) = find_offset(input, 1, 0, span.start());
        let (line_no, end) = find_offset(input, line_no, start - span.start().column, span.end());
        Self { lo: start, hi: end }
    }
}

impl From<Span> for Range<usize> {
    fn from(span: Span) -> Self {
        span.lo..span.hi
    }
}
