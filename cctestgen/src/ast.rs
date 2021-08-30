// use std::{convert::TryFrom, fmt::Display, str::FromStr};

// use crate::{gen::Mode, SOURCE};

// use super::parse::Rule;

// use ariadne::Label;
// use color_eyre::{eyre::eyre, Result};

// use pest::iterators::Pair;

// use proc_macro2::{Ident, Literal};

// use derive_more::TryInto;
// use quote::{format_ident, ToTokens};
// use syn::{LitBool, Path};

// #[derive(Debug, Clone)]
// pub(crate) enum TestKind {
//     Pass,
//     Fail(Expr),
// }

// #[derive(Debug, Clone)]
// pub(crate) struct Descriptor {
//     pub(crate) name: String,
//     pub(crate) sighashes: Vec<String>,
//     pub(crate) signer: Expr,
//     pub(crate) tx_fee: Option<Expr>,
//     pub(crate) pass_fail: TestKind,
//     pub(crate) stmts: Vec<Stmt>,
//     pub(crate) request: Option<Expr>,
// }

// pub(crate) trait PairExt<Inner = Self> {
//     fn expecting(self, rule: Rule) -> Result<Inner>;

//     fn expecting_one_of(self, rules: &[Rule]) -> Result<Inner>;
// }

// impl PairExt for Pair<'_, Rule> {
//     fn expecting_one_of(self, rules: &[Rule]) -> Result<Self> {
//         for rule in rules {
//             if self.as_rule() == *rule {
//                 return Ok(self);
//             }
//         }
//         Err(eyre!(
//             "expected one of {:?} but found {:?}",
//             rules,
//             self.as_rule()
//         ))
//     }
//     fn expecting(self, rule: Rule) -> Result<Self> {
//         if self.as_rule() == rule {
//             Ok(self)
//         } else {
//             Err(eyre!("expected {:?} but found {:?}", &rule, self.as_rule()))
//         }
//     }
// }

// impl<'a> PairExt<Pair<'a, Rule>> for Option<Pair<'a, Rule>> {
//     fn expecting(self, rule: Rule) -> Result<Pair<'a, Rule>> {
//         match self {
//             Some(p) => {
//                 if p.as_rule() == rule {
//                     Ok(p)
//                 } else {
//                     Err(eyre!("expected {:?} but found {:?}", &rule, p.as_rule()))
//                 }
//             }
//             None => Err(eyre!("expected a pair but found None")),
//         }
//     }
//     fn expecting_one_of(self, rules: &[Rule]) -> Result<Pair<'a, Rule>> {
//         match self {
//             Some(p) => {
//                 for rule in rules {
//                     if p.as_rule() == *rule {
//                         return Ok(p);
//                     }
//                 }
//                 Err(eyre!(
//                     "expected one of {:?} but found {:?}",
//                     rules,
//                     p.as_rule()
//                 ))
//             }
//             None => Err(eyre!("expected a pair but found None")),
//         }
//     }
// }

// type Fields = Vec<StructEntry>;

// #[derive(Debug, Clone)]
// pub(crate) struct Field {
//     pub(crate) name: Ident,
//     pub(crate) value: Expr,
// }

// #[derive(Debug, Clone)]
// pub(crate) enum StructEntry {
//     Pair(Field),
//     Single(Expr),
//     Update(Expr),
// }

// #[derive(Debug, Clone)]
// pub(crate) enum ResultExpr {
//     Ok(Box<Expr>),
//     Err(Box<Expr>),
// }

// #[derive(Clone, Copy, Debug, PartialEq)]
// pub(crate) enum RefKind {
//     Mut,
//     Immut,
// }

// #[derive(Debug, Clone, TryInto)]
// pub(crate) enum Expr {
//     RustCode(String),
//     Default,
//     Ident(Ident),
//     Construction {
//         name: Path,
//         fields: Fields,
//     },
//     Array(Vec<Expr>),
//     Literal(Literal),
//     Option(Option<Box<Expr>>),
//     Result(ResultExpr),
//     Tuple(Vec<Expr>),
//     Postfix {
//         value: Box<Expr>,
//         postfix: Vec<Postfix>,
//     },
//     FnCall {
//         func: Path,
//         args: Vec<Expr>,
//     },
//     PseudoMacro(PseudoMacro),
//     Ref {
//         kind: RefKind,
//         expr: Box<Expr>,
//     },
// }

// #[derive(Clone, Debug)]
// pub(crate) enum Postfix {
//     FieldAccess(FieldAccess),
//     MethodCall(MethodCall),
// }

// #[derive(Clone, Debug)]
// pub(crate) struct FieldAccess {
//     pub(crate) field: Ident,
// }

// #[derive(Clone, Debug)]
// pub(crate) struct MethodCall {
//     pub(crate) method: Ident,
//     pub(crate) args: Vec<Expr>,
// }

// #[derive(Debug, Clone)]
// pub(crate) enum PseudoMacro {
//     SigHash(Ident),
//     WalletId(Ident),
//     Guid(Option<Ident>),
//     Signer(Ident),
// }

// fn from_one_arg<I>(args: Vec<Expr>) -> Result<I>
// where
//     I: TryFrom<Expr>,
//     <I as TryFrom<Expr>>::Error: Display,
// {
//     if args.len() != 1 {
//         return Err(eyre!("expected one argument: found {:?}", args));
//     }
//     I::try_from(args.into_iter().next().unwrap())
//         .map_err(|e| eyre!("argument was of incorrect type: {}", e))
// }

// impl ParseAst for PseudoMacro {
//     fn parse<'a>(pair: impl PairExt<Pair<'a, Rule>>) -> Result<Self> {
//         let mac = pair.expecting(Rule::pseudo_macro)?;
//         let mut inner = mac.into_inner();
//         let ident = Ident::parse(inner.next())?;
//         let mut args: Vec<Expr> = Vec::new();
//         for arg in inner {
//             args.push(Expr::parse(arg)?);
//         }

//         if ident == "SigHash" {
//             let id = from_one_arg(args)?;
//             Ok(PseudoMacro::SigHash(id))
//         } else if ident == "WalletId" {
//             let id = from_one_arg(args)?;
//             Ok(PseudoMacro::WalletId(id))
//         } else if ident == "Guid" {
//             if args.is_empty() {
//                 Ok(PseudoMacro::Guid(None))
//             } else {
//                 let id = from_one_arg(args)?;
//                 Ok(PseudoMacro::Guid(Some(id)))
//             }
//         } else if ident == "Signer" {
//             let id = from_one_arg(args)?;
//             Ok(PseudoMacro::Signer(id))
//         } else {
//             Err(eyre!("Unrecognized macro: {:?}", ident))
//         }
//     }
// }

// #[derive(Debug, Clone)]
// pub(crate) enum MapEntry {
//     Pair(Expr, Expr),
//     Single(Expr),
// }
// pub(crate) type Mapping = Vec<MapEntry>;

// #[derive(Debug, Clone)]
// pub(crate) struct Sighash(pub(crate) String);

// impl Default for Expr {
//     fn default() -> Self {
//         Self::Default
//     }
// }

// #[derive(Debug, Clone)]
// pub(crate) enum Requirement {
//     Wallet {
//         sighash: Ident,
//         amount: Expr,
//     },
//     Guid {
//         id: Ident,
//     },
//     SendTx {
//         tx: Expr,
//         signer: Expr,
//         guid: Option<Expr>,
//     },
// }

// #[derive(Debug, Clone)]
// pub(crate) enum Expectation {
//     GetBalance { id: Expr, ret: Expr },
//     GetStateEntry { id: Expr, ret: Expr },
//     SetStateEntry { id: Expr, value: Expr },
//     SetStateEntries { values: Mapping },
//     DeleteStateEntry { id: Expr },
//     DeleteStateEntries { values: Vec<Expr> },
//     GetSighash { sig: Expr },
//     GetGuid { guid: Expr },
//     Verify(Expr),
// }

// #[derive(Debug, Clone)]
// pub(crate) enum Stmt {
//     Binding { lhs: Pat, value: Expr },
//     Require { requirements: Vec<Requirement> },
//     Expect { expectations: Vec<Expectation> },
//     RustCode(String),
//     ModeSpecific { mode: Mode, stmts: Vec<Stmt> },
// }

// pub(crate) trait ParseAst: Sized {
//     fn parse<'a>(pair: impl PairExt<Pair<'a, Rule>>) -> Result<Self>;
// }

// impl ParseAst for Ident {
//     fn parse<'a>(pair: impl PairExt<Pair<'a, Rule>>) -> Result<Self> {
//         let expr = pair.expecting(Rule::ident)?;
//         match syn::parse_str(expr.as_str()) {
//             Ok(ident) => Ok(ident),
//             Err(e) => {
//                 ariadne::Report::build(ariadne::ReportKind::Error, (), expr.as_span().start())
//                     .with_message(&e)
//                     .with_label(ariadne::Label::new(
//                         expr.as_span().start()..expr.as_span().end(),
//                     ))
//                     .finish()
//                     .eprint(&mut *crate::SOURCE.get().unwrap().lock().unwrap())
//                     .unwrap();
//                 Err(e.into())
//             }
//         }
//     }
// }

// impl ParseAst for MethodCall {
//     fn parse<'a>(pair: impl PairExt<Pair<'a, Rule>>) -> Result<Self> {
//         let mut call = pair.expecting(Rule::call)?.into_inner();
//         let method = Ident::parse(call.next())?;
//         let mut args = Vec::new();
//         for arg in call {
//             args.push(Expr::parse(arg)?);
//         }
//         Ok(MethodCall { method, args })
//     }
// }

// impl ParseAst for FieldAccess {
//     fn parse<'a>(pair: impl PairExt<Pair<'a, Rule>>) -> Result<Self> {
//         let pair = pair.expecting(Rule::field_access)?;
//         let field = Ident::parse(pair.into_inner().next())?;

//         Ok(FieldAccess { field })
//     }
// }

// impl ParseAst for Postfix {
//     fn parse<'a>(pair: impl PairExt<Pair<'a, Rule>>) -> Result<Self> {
//         let pair = pair.expecting_one_of(&[Rule::call, Rule::field_access])?;
//         match pair.as_rule() {
//             Rule::call => Ok(Postfix::MethodCall(MethodCall::parse(pair)?)),
//             Rule::field_access => Ok(Postfix::FieldAccess(FieldAccess::parse(pair)?)),
//             other => unreachable!(
//                 "Unexpected rule {:?} when parsing postfix: {:?}",
//                 other, pair
//             ),
//         }
//     }
// }

// impl ParseAst for Expr {
//     fn parse<'a>(expr: impl PairExt<Pair<'a, Rule>>) -> Result<Expr> {
//         let expr = expr
//             .expecting_one_of(&[Rule::expr, Rule::non_method_expr])?
//             .into_inner()
//             .next()
//             .unwrap();
//         match expr.as_rule() {
//             Rule::code => {
//                 let code = expr.as_str();

//                 Ok(Expr::RustCode(code.into()))
//             }
//             Rule::literal => {
//                 let next = expr.into_inner().next().unwrap();
//                 match next.as_rule() {
//                     Rule::string | Rule::number => Ok(Expr::Literal(
//                         Literal::from_str(next.as_str())
//                             .map_err(|e| eyre!("Invalid literal {:?}", e))?,
//                     )),
//                     Rule::array => {
//                         let mut elements = Vec::new();
//                         for pair in next.into_inner() {
//                             elements.push(Expr::parse(pair)?);
//                         }
//                         Ok(Expr::Array(elements))
//                     }
//                     Rule::mapping => {
//                         todo!()
//                     }
//                     Rule::option => {
//                         let inner = next.into_inner().next();
//                         if let Some(expr) = inner {
//                             Ok(Expr::Option(Some(Box::new(Expr::parse(expr)?))))
//                         } else {
//                             Ok(Expr::Option(None))
//                         }
//                     }
//                     Rule::owned_string => {
//                         let mut inner = next.into_inner();

//                         let lit = Literal::from_str(inner.next().expecting(Rule::string)?.as_str())
//                             .map_err(|e| eyre!("Invalid literal {:?}", e))?;

//                         let ts = quote::quote! {
//                             String::from(#lit)
//                         };
//                         Ok(Expr::RustCode(ts.to_string()))
//                     }
//                     Rule::bool => {
//                         let lit: LitBool = syn::parse_str(next.as_str())?;

//                         Ok(Expr::RustCode(lit.into_token_stream().to_string()))
//                     }
//                     Rule::tuple => {
//                         let inner = next.into_inner();

//                         let mut elements = Vec::new();
//                         for pair in inner {
//                             elements.push(Expr::parse(pair)?);
//                         }

//                         Ok(Expr::Tuple(elements))
//                     }
//                     Rule::result => {
//                         let next = next.into_inner().next().unwrap();
//                         match next.as_rule() {
//                             Rule::ok => Ok(Expr::Result(ResultExpr::Ok(Box::new(Expr::parse(
//                                 next.into_inner().next(),
//                             )?)))),
//                             Rule::err => Ok(Expr::Result(ResultExpr::Err(Box::new(Expr::parse(
//                                 next.into_inner().next(),
//                             )?)))),
//                             other => unreachable!("unexpected rule {:?}", other),
//                         }
//                     }
//                     _ => unreachable!(),
//                 }
//             }
//             Rule::constructor => {
//                 let mut inner = expr.into_inner();
//                 let name: syn::Path = syn::parse_str(inner.next().expecting(Rule::path)?.as_str())?;
//                 let fields = Fields::parse(inner.next())?;
//                 Ok(Expr::Construction { name, fields })
//             }
//             Rule::ident => match syn::parse_str(expr.as_str()) {
//                 Ok(ident) => Ok(Expr::Ident(ident)),
//                 Err(e) => {
//                     ariadne::Report::build(ariadne::ReportKind::Error, (), expr.as_span().start())
//                         .with_message(&e)
//                         .with_label(ariadne::Label::new(
//                             expr.as_span().start()..expr.as_span().end(),
//                         ))
//                         .finish()
//                         .eprint(&mut *crate::SOURCE.get().unwrap().lock().unwrap())
//                         .unwrap();
//                     Err(e.into())
//                 }
//             },
//             Rule::default => Ok(Expr::Default),
//             Rule::method_call => {
//                 let mut inner = expr.into_inner();
//                 let value = Box::new(Expr::parse(inner.next())?);
//                 let calls = inner.next().expecting(Rule::calls)?;
//                 let mut postfix = Vec::new();
//                 for call in calls.into_inner() {
//                     postfix.push(Postfix::parse(call)?);
//                 }
//                 Ok(Expr::Postfix { value, postfix })
//             }
//             Rule::expr_block => Ok(Expr::parse(expr.into_inner().next())?),
//             Rule::pseudo_macro => Ok(Expr::PseudoMacro(PseudoMacro::parse(expr)?)),
//             Rule::fn_call => {
//                 let mut inner = expr.into_inner();
//                 let func = syn::parse_str(inner.next().expecting(Rule::path)?.as_str())?;
//                 let mut args = Vec::new();
//                 for arg in inner {
//                     args.push(Expr::parse(arg)?);
//                 }
//                 Ok(Expr::FnCall { func, args })
//             }
//             Rule::reference => {
//                 let mut inner = expr.into_inner();
//                 let kind = RefKind::parse(inner.next())?;
//                 let expr = Box::new(Expr::parse(inner.next())?);
//                 Ok(Expr::Ref { kind, expr })
//             }
//             other => unreachable!("Bad, we got a {:?}: {:?}", other, expr.as_str()),
//         }
//     }
// }

// impl ParseAst for RefKind {
//     fn parse<'a>(pair: impl PairExt<Pair<'a, Rule>>) -> Result<Self> {
//         let pair = pair.expecting_one_of(&[Rule::immut_ref, Rule::mut_ref])?;

//         match pair.as_rule() {
//             Rule::immut_ref => Ok(RefKind::Immut),
//             Rule::mut_ref => Ok(RefKind::Mut),
//             other => unreachable!("shouldn't be reachable here {:?}", other),
//         }
//     }
// }

// impl ParseAst for StructEntry {
//     fn parse<'a>(pair: impl PairExt<Pair<'a, Rule>>) -> Result<Self> {
//         let pair = pair.expecting(Rule::struct_pair)?;
//         let mut inner = pair.into_inner();
//         let first = inner.next().unwrap();
//         match first.as_rule() {
//             Rule::field_pair => {
//                 let mut parts = first.into_inner();
//                 let name = parts.next().expecting(Rule::ident)?;
//                 let value = Expr::parse(parts.next())?;
//                 Ok(Self::Pair(Field {
//                     name: syn::parse_str(name.as_str())?,
//                     value,
//                 }))
//             }
//             Rule::update => {
//                 let value = Expr::parse(first.into_inner().next())?;
//                 Ok(Self::Update(value))
//             }
//             Rule::short => {
//                 let value = Expr::parse(first.into_inner().next())?;
//                 Ok(Self::Single(value))
//             }
//             other => unreachable!("bad rule in struct entry: {:?}", other),
//         }
//     }
// }

// impl ParseAst for Fields {
//     fn parse<'a>(pair: impl PairExt<Pair<'a, Rule>>) -> Result<Self> {
//         let mapping = pair.expecting(Rule::struct_map)?;
//         let mut fields = vec![];
//         for pair in mapping.into_inner() {
//             fields.push(StructEntry::parse(pair)?);
//         }
//         Ok(fields)
//     }
// }

// impl ParseAst for Mapping {
//     fn parse<'a>(pair: impl PairExt<Pair<'a, Rule>>) -> Result<Self> {
//         let mapping = pair.expecting(Rule::mapping)?;
//         let mut items = Mapping::new();
//         for pair in mapping.into_inner() {
//             items.push(MapEntry::parse(pair)?);
//         }
//         Ok(items)
//     }
// }

// impl ParseAst for MapEntry {
//     fn parse<'a>(pair: impl PairExt<Pair<'a, Rule>>) -> Result<Self> {
//         let entry = pair.expecting(Rule::pair)?;
//         let mut inner = entry.into_inner();
//         let e = inner.next().ok_or_else(|| eyre!("expected item"))?;
//         match e.as_rule() {
//             Rule::expr => {
//                 let key = Expr::parse(e)?;
//                 let value = Expr::parse(inner.next())?;
//                 Ok(Self::Pair(key, value))
//             }
//             Rule::short => {
//                 let value = Expr::parse(e.into_inner().next())?;
//                 Ok(Self::Single(value))
//             }
//             other => unreachable!("Unexpected rule {:?}", other),
//         }
//     }
// }

// impl ParseAst for Requirement {
//     fn parse<'a>(pair: impl PairExt<Pair<'a, Rule>>) -> Result<Self> {
//         let req = pair
//             .expecting(Rule::requirement)?
//             .into_inner()
//             .next()
//             .expect("Requirement should have an inner rule");

//         match req.as_rule() {
//             Rule::wallet => {
//                 let mut inner = req.into_inner();
//                 let sighash = Ident::parse(inner.next())?;
//                 let amount = Expr::parse(inner.next())?;
//                 Ok(Requirement::Wallet { sighash, amount })
//             }
//             Rule::guid => {
//                 let mut inner = req.into_inner();
//                 let id = syn::parse_str(inner.next().expecting(Rule::ident)?.as_str())?;
//                 Ok(Requirement::Guid { id })
//             }
//             Rule::send_tx => {
//                 let mut inner = req.into_inner();
//                 let tx = Expr::parse(inner.next())?;
//                 let signer = Expr::parse(inner.next())?;
//                 let guid = inner.next().map(Expr::parse).transpose()?;
//                 Ok(Requirement::SendTx { tx, signer, guid })
//             }
//             _ => unreachable!(),
//         }
//     }
// }

// impl ParseAst for Vec<Expr> {
//     fn parse<'a>(pair: impl PairExt<Pair<'a, Rule>>) -> Result<Self> {
//         let pair = pair.expecting(Rule::array)?;

//         let mut exprs = Vec::new();
//         for expr in pair.into_inner() {
//             exprs.push(Expr::parse(expr)?);
//         }

//         Ok(exprs)
//     }
// }

// impl ParseAst for Expectation {
//     fn parse<'a>(pair: impl PairExt<Pair<'a, Rule>>) -> Result<Self> {
//         let exp = pair.expecting(Rule::expectation)?;
//         let inner = exp.into_inner().next().unwrap();

//         match inner.as_rule() {
//             Rule::get_balance => {
//                 let mut inner = inner.into_inner();
//                 let id = Expr::parse(inner.next())?;
//                 let ret = Expr::parse(inner.next())?;
//                 Ok(Expectation::GetBalance { id, ret })
//             }
//             Rule::get_state => {
//                 let mut inner = inner.into_inner();
//                 let id = Expr::parse(inner.next())?;
//                 let ret_pair = inner
//                     .next()
//                     .expecting_one_of(&[Rule::expr, Rule::non_method_expr])?;
//                 if ret_pair.as_str().contains("to_bytes") {
//                     ariadne::Report::build(
//                         ariadne::ReportKind::Warning,
//                         (),
//                         ret_pair.as_span().start(),
//                     )
//                     .with_message("passing bytes as the expectation's return value")
//                     .with_note("the return value is serialized automatically")
//                     .with_label(
//                         Label::new(ret_pair.as_span().start()..ret_pair.as_span().end())
//                             .with_color(ariadne::Color::Yellow)
//                             .with_message("consider removing the `to_bytes` call"),
//                     )
//                     .finish()
//                     .eprint(&mut *SOURCE.get().unwrap().lock().unwrap())
//                     .unwrap();
//                 }
//                 let ret = Expr::parse(ret_pair)?;
//                 Ok(Expectation::GetStateEntry { id, ret })
//             }
//             Rule::set_state => {
//                 let mut inner = inner.into_inner();
//                 let id = Expr::parse(inner.next())?;
//                 let value = Expr::parse(inner.next())?;
//                 Ok(Expectation::SetStateEntry { id, value })
//             }
//             Rule::set_states => {
//                 let mut inner = inner.into_inner();
//                 let values = Mapping::parse(inner.next())?;
//                 Ok(Expectation::SetStateEntries { values })
//             }
//             Rule::delete_state => {
//                 let mut inner = inner.into_inner();
//                 let id = Expr::parse(inner.next())?;
//                 Ok(Expectation::DeleteStateEntry { id })
//             }
//             Rule::delete_states => {
//                 let values = Vec::parse(inner.into_inner().next())?;
//                 Ok(Expectation::DeleteStateEntries { values })
//             }
//             Rule::get_sighash => {
//                 let sig = Expr::parse(inner.into_inner().next())?;
//                 Ok(Expectation::GetSighash { sig })
//             }
//             Rule::get_guid => {
//                 let guid = Expr::parse(inner.into_inner().next())?;
//                 Ok(Expectation::GetGuid { guid })
//             }
//             Rule::verify => {
//                 let ret = Expr::parse(inner.into_inner().next())?;
//                 Ok(Expectation::Verify(ret))
//             }
//             bad => unreachable!("Did not expect to find {:?}", bad),
//         }
//     }
// }

// #[derive(Clone, Debug)]
// pub(crate) enum Pat {
//     Ident(Ident),
//     Tuple(Vec<Ident>),
// }

// impl ParseAst for Pat {
//     fn parse<'a>(pair: impl PairExt<Pair<'a, Rule>>) -> Result<Self> {
//         let pat = pair.expecting(Rule::pat)?;
//         let pat_ = pat.clone();
//         let inner = pat.into_inner().next().unwrap();
//         match inner.as_rule() {
//             Rule::ident_pat => {
//                 let id = inner.into_inner().next();
//                 Ok(Pat::Ident(Ident::parse(id)?))
//             }
//             Rule::tuple_pat => {
//                 let mut idents = Vec::new();
//                 for p in inner.into_inner() {
//                     let p = p.expecting(Rule::ident_pat)?;
//                     let id = p.into_inner().next();
//                     idents.push(Ident::parse(id)?);
//                 }
//                 Ok(Pat::Tuple(idents))
//             }
//             other => unreachable!("unexpectedly found {:?} parsing Pat {:?}", other, pat_),
//         }
//     }
// }

// impl ParseAst for Stmt {
//     fn parse<'a>(pair: impl PairExt<Pair<'a, Rule>>) -> Result<Self> {
//         let stmt = pair.expecting(Rule::statement)?;

//         let next = stmt.into_inner().next().unwrap();
//         match next.as_rule() {
//             Rule::binding => {
//                 let mut inner = next.into_inner();
//                 let lhs = Pat::parse(inner.next())?;
//                 let value = Expr::parse(inner.next())?;
//                 Ok(Stmt::Binding { lhs, value })
//             }
//             Rule::require => {
//                 let mut requirements = Vec::new();
//                 for pair in next.into_inner() {
//                     requirements.push(Requirement::parse(pair)?);
//                 }
//                 Ok(Stmt::Require { requirements })
//             }
//             Rule::expect => {
//                 let mut expectations = Vec::new();
//                 for pair in next.into_inner() {
//                     expectations.push(Expectation::parse(pair)?);
//                 }
//                 Ok(Stmt::Expect { expectations })
//             }
//             Rule::code => {
//                 let code = next.as_str();
//                 Ok(Stmt::RustCode(code.into()))
//             }
//             Rule::integration => {
//                 let inner = next.into_inner();
//                 let mut stmts = Vec::new();
//                 for pair in inner {
//                     stmts.push(Stmt::parse(pair)?);
//                 }
//                 Ok(Stmt::ModeSpecific {
//                     mode: Mode::Integration,
//                     stmts,
//                 })
//             }
//             Rule::unit => {
//                 let inner = next.into_inner();
//                 let mut stmts = Vec::new();
//                 for pair in inner {
//                     stmts.push(Stmt::parse(pair)?);
//                 }
//                 Ok(Stmt::ModeSpecific {
//                     mode: Mode::Unit,
//                     stmts,
//                 })
//             }
//             _ => unreachable!(),
//         }
//     }
// }

// impl ParseAst for Descriptor {
//     fn parse<'a>(pair: impl PairExt<Pair<'a, Rule>>) -> Result<Self> {
//         let pair = pair.expecting(Rule::descriptor)?;
//         let pairs = pair.into_inner();

//         let mut name = None;
//         let mut sighashes = vec![];
//         let mut tx_fee = None;
//         let mut pass_fail = TestKind::Pass;
//         let mut stmts = Vec::new();
//         let mut request = None;
//         let mut signer = None;

//         for pair in pairs {
//             if let Rule::statement = pair.as_rule() {
//                 stmts.push(Stmt::parse(pair)?);
//             } else if let Rule::EOI = pair.as_rule() {
//                 break;
//             } else if let Rule::meta = pair.as_rule() {
//                 for m in pair.into_inner() {
//                     match m.as_rule() {
//                         Rule::testname => {
//                             let mut inner = m.into_inner();
//                             let value = inner.next().expecting(Rule::string)?;
//                             let value = value.into_inner().next().expecting(Rule::inner)?;
//                             name = Some(value.as_str().to_owned());
//                         }
//                         Rule::sighashes => {
//                             let arr = m.into_inner();
//                             for id in arr {
//                                 let id = id.expecting(Rule::ident)?;
//                                 sighashes.push(id.as_str().to_owned());
//                             }
//                         }
//                         Rule::command => {
//                             let value = Expr::parse(m.into_inner().next())?;
//                             stmts.push(Stmt::Binding {
//                                 lhs: Pat::Ident(format_ident!("command")),
//                                 value,
//                             });
//                             stmts.push(Stmt::Require {
//                                 requirements: vec![Requirement::Guid {
//                                     id: format_ident!("{}", "command"),
//                                 }],
//                             })
//                         }
//                         Rule::tx_fee => {
//                             let expr = m.into_inner().next().expecting(Rule::expr)?;
//                             let value = Expr::parse(expr)?;
//                             tx_fee = Some(value);
//                         }
//                         Rule::passfail => {
//                             let mut inner = m.into_inner();
//                             match inner.next() {
//                                 Some(pair) => {
//                                     let expected = pair.expecting(Rule::expr)?;
//                                     let expr = Expr::parse(expected)?;
//                                     pass_fail = TestKind::Fail(expr);
//                                 }
//                                 None => {
//                                     pass_fail = TestKind::Pass;
//                                 }
//                             }
//                         }
//                         Rule::request => {
//                             let expr = Expr::parse(m.into_inner().next())?;
//                             request = Some(expr);
//                         }
//                         Rule::signer => {
//                             let expr = Expr::parse(m.into_inner().next())?;
//                             signer = Some(expr);
//                         }
//                         other => panic!("Unexpected rule {:?}", other),
//                     }
//                 }
//             } else {
//                 panic!("Bad {:?}", pair);
//             }
//         }
//         Ok(Descriptor {
//             name: name.ok_or_else(|| eyre!("Must specify the name of the test"))?,
//             sighashes,
//             signer: signer.ok_or_else(|| eyre!("Must specify the signer of the command"))?,
//             tx_fee,
//             pass_fail,
//             stmts,
//             request,
//         })
//     }
// }
