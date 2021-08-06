use std::{fs, str::FromStr};

use color_eyre::{eyre::eyre, Result};
use inflector::Inflector;
use itertools::Itertools;
use pest::{
    iterators::{Pair, Pairs},
    Parser,
};
use pest_derive::Parser;
use proc_macro2::{Ident, Literal, TokenStream};
use quote::{format_ident, quote, IdentFragment};
use strum_macros::{AsRefStr, EnumString, IntoStaticStr};

#[derive(Parser)]
#[grammar = "descriptor.pest"]
struct DescriptorParser;

type Fields = Vec<Field>;

#[derive(Debug, Clone, Copy, AsRefStr, IntoStaticStr, EnumString)]

pub enum CommandKind {
    SendFunds,
    RegisterAddress,
    RegisterTransfer,
    AddAskOrder,
    AddBidOrder,
    AddOffer,
    AddDealOrder,
    CompleteDealOrder,
    LockDealOrder,
    CloseDealOrder,
    Exempt,
    AddRepaymentOrder,
    CompleteRepaymentOrder,
    CloseRepaymentOrder,
    CollectCoins,
    Housekeeping,
}

#[derive(Debug, Clone)]
pub struct Command {
    kind: CommandKind,
    fields: Fields,
}

#[derive(Debug, Clone)]
pub enum TestKind {
    Pass,
    Fail(Expr),
}

#[derive(Debug, Clone)]
pub struct Descriptor {
    name: String,
    sighashes: Vec<String>,
    command: Command,
    tx_fee: Option<Expr>,
    pass_fail: TestKind,
    stmts: Vec<Stmt>,
    request: Option<Expr>,
}

pub trait PairExt<Inner = Self> {
    fn expecting(self, rule: Rule) -> Result<Inner>;
}

impl PairExt for Pair<'_, Rule> {
    fn expecting(self, rule: Rule) -> Result<Self> {
        if self.as_rule() == rule {
            Ok(self)
        } else {
            Err(eyre!("expected {:?} but found {:?}", &rule, self.as_rule()))
        }
    }
}

impl<'a> PairExt<Pair<'a, Rule>> for Option<Pair<'a, Rule>> {
    fn expecting(self, rule: Rule) -> Result<Pair<'a, Rule>> {
        match self {
            Some(p) => {
                if p.as_rule() == rule {
                    Ok(p)
                } else {
                    Err(eyre!("expected {:?} but found {:?}", &rule, p.as_rule()))
                }
            }
            None => Err(eyre!("expected a pair but found None")),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Field {
    name: Ident,
    value: Box<Expr>,
}

impl ToRust for Field {
    fn to_rust(self, mode: Mode) -> Result<TokenStream> {
        let Self { name, value } = self;
        let value = value.to_rust(mode)?;
        Ok(quote! {
            #name : #value
        })
    }
}

#[derive(Debug, Clone)]
pub enum Expr {
    RustCode(String),
    Sighash(Sighash),
    Default,
    Ident(Ident),
    Construction { name: Ident, fields: Fields },
    Array(Vec<Expr>),
    Mapping(Mapping),
    Literal(Literal),
    WalletId(Ident),
    GuidFor(Ident),
    Option(Option<Box<Expr>>),
}

pub type Mapping = Vec<(Expr, Expr)>;

#[derive(Debug, Clone)]
pub struct Sighash(String);

impl Default for Expr {
    fn default() -> Self {
        Self::Default
    }
}

#[derive(Debug, Clone)]
pub enum Requirement {
    Wallet { sighash: Sighash, amount: Expr },
    Guid { id: Ident },
}

#[derive(Debug, Clone)]
pub enum Expectation {
    GetBalance { id: Expr, ret: Expr },
    GetStateEntry { id: Expr, ret: Expr },
    SetStateEntry { id: Expr, value: Expr },
    SetStateEntries { values: Mapping },
    DeleteStateEntry { id: Expr },
    GetSighash { sig: Expr },
    GetGuid { guid: Expr },
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Binding { name: String, value: Expr },
    Require { requirements: Vec<Requirement> },
    Expect { expectations: Vec<Expectation> },
    RustCode(String),
}

pub trait ParseAst: Sized {
    fn parse<'a>(pair: impl PairExt<Pair<'a, Rule>>) -> Result<Self>;
}

impl ParseAst for Expr {
    fn parse<'a>(expr: impl PairExt<Pair<'a, Rule>>) -> Result<Expr> {
        let expr = expr.expecting(Rule::expr)?.into_inner().next().unwrap();

        match expr.as_rule() {
            Rule::code => {
                let code = expr.as_str();

                Ok(Expr::RustCode(code.into()))
            }
            Rule::sighash => Ok(Expr::Sighash(Sighash::parse(expr)?)),
            Rule::literal => {
                let next = expr.into_inner().next().unwrap();
                match next.as_rule() {
                    Rule::string | Rule::number => Ok(Expr::Literal(
                        Literal::from_str(next.as_str())
                            .map_err(|e| eyre!("Invalid literal {:?}", e))?,
                    )),
                    Rule::array => {
                        let mut elements = Vec::new();
                        for pair in next.into_inner() {
                            elements.push(Expr::parse(pair)?);
                        }
                        Ok(Expr::Array(elements))
                    }
                    Rule::mapping => {
                        todo!()
                    }
                    Rule::option => {
                        let inner = next.into_inner().next();
                        if let Some(expr) = inner {
                            Ok(Expr::Option(Some(Box::new(Expr::parse(expr)?))))
                        } else {
                            Ok(Expr::Option(None))
                        }
                    }
                    _ => unreachable!(),
                }
            }
            Rule::constructor => {
                let mut inner = expr.into_inner();
                let name = syn::parse_str(inner.next().expecting(Rule::ident)?.as_str())?;
                let fields = Fields::parse(inner.next())?;
                Ok(Expr::Construction { name, fields })
            }
            Rule::ident => Ok(Expr::Ident(syn::parse_str(expr.as_str())?)),
            Rule::walletid => Ok(Expr::WalletId(syn::parse_str(
                expr.into_inner().next().expecting(Rule::ident)?.as_str(),
            )?)),
            Rule::guid_for => Ok(Expr::GuidFor(syn::parse_str(
                expr.into_inner().next().expecting(Rule::ident)?.as_str(),
            )?)),
            Rule::default => Ok(Expr::Default),
            foo => unreachable!("Bad, we got a {:?}", foo),
        }
    }
}

impl ParseAst for Fields {
    fn parse<'a>(pair: impl PairExt<Pair<'a, Rule>>) -> Result<Self> {
        let mapping = pair.expecting(Rule::struct_map)?;
        let mut fields = vec![];
        for pair in mapping.into_inner() {
            let pair = pair.expecting(Rule::struct_pair)?;
            let mut parts = pair.into_inner();
            let name = parts.next().expecting(Rule::ident)?;
            let value = Expr::parse(parts.next())?;
            fields.push(Field {
                name: syn::parse_str(name.as_str())?,
                value: Box::new(value),
            });
        }
        Ok(fields)
    }
}

impl ParseAst for Mapping {
    fn parse<'a>(pair: impl PairExt<Pair<'a, Rule>>) -> Result<Self> {
        let mapping = pair.expecting(Rule::mapping)?;
        let mut items = Mapping::new();
        for pair in mapping.into_inner() {
            let pair = pair.expecting(Rule::pair)?;
            let mut parts = pair.into_inner();
            let key = Expr::parse(parts.next())?;
            let value = Expr::parse(parts.next())?;
            items.push((key, value));
        }
        Ok(items)
    }
}

impl ParseAst for Sighash {
    fn parse<'a>(pair: impl PairExt<Pair<'a, Rule>>) -> Result<Self> {
        let sig = pair.expecting(Rule::sighash)?;
        let id = sig
            .into_inner()
            .next()
            .expecting(Rule::ident)?
            .as_str()
            .to_owned();
        Ok(Sighash(id))
    }
}

impl ParseAst for Requirement {
    fn parse<'a>(pair: impl PairExt<Pair<'a, Rule>>) -> Result<Self> {
        let req = pair
            .expecting(Rule::requirement)?
            .into_inner()
            .next()
            .expect("Requirement should have an inner rule");

        match req.as_rule() {
            Rule::wallet => {
                let mut inner = req.into_inner();
                let sighash = Sighash::parse(inner.next())?;
                let amount = Expr::parse(inner.next())?;
                Ok(Requirement::Wallet { sighash, amount })
            }
            Rule::guid => {
                let mut inner = req.into_inner();
                let id = syn::parse_str(inner.next().expecting(Rule::ident)?.as_str())?;
                Ok(Requirement::Guid { id })
            }
            _ => unreachable!(),
        }
    }
}

impl ParseAst for Expectation {
    fn parse<'a>(pair: impl PairExt<Pair<'a, Rule>>) -> Result<Self> {
        let exp = pair.expecting(Rule::expectation)?;
        let inner = exp.into_inner().next().unwrap();

        match inner.as_rule() {
            Rule::get_balance => {
                let mut inner = inner.into_inner();
                let id = Expr::parse(inner.next())?;
                let ret = Expr::parse(inner.next())?;
                Ok(Expectation::GetBalance { id, ret })
            }
            Rule::get_state => {
                let mut inner = inner.into_inner();
                let id = Expr::parse(inner.next())?;
                let ret = Expr::parse(inner.next())?;
                Ok(Expectation::GetStateEntry { id, ret })
            }
            Rule::set_state => {
                let mut inner = inner.into_inner();
                let id = Expr::parse(inner.next())?;
                let value = Expr::parse(inner.next())?;
                Ok(Expectation::SetStateEntry { id, value })
            }
            Rule::set_states => {
                let mut inner = inner.into_inner();
                let values = Mapping::parse(inner.next())?;
                Ok(Expectation::SetStateEntries { values })
            }
            Rule::delete_state => {
                let mut inner = inner.into_inner();
                let id = Expr::parse(inner.next())?;
                Ok(Expectation::DeleteStateEntry { id })
            }
            Rule::get_sighash => {
                let sig = Expr::parse(inner.into_inner().next())?;
                Ok(Expectation::GetSighash { sig })
            }
            Rule::get_guid => {
                let guid = Expr::parse(inner.into_inner().next())?;
                Ok(Expectation::GetGuid { guid })
            }
            bad => unreachable!("Did not expect to find {:?}", bad),
        }
    }
}

impl ParseAst for Stmt {
    fn parse<'a>(pair: impl PairExt<Pair<'a, Rule>>) -> Result<Self> {
        let stmt = pair.expecting(Rule::statement)?;

        let next = stmt.into_inner().next().unwrap();
        match next.as_rule() {
            Rule::binding => {
                let mut inner = next.into_inner();
                let id = inner.next().expecting(Rule::ident)?;
                let value = Expr::parse(inner.next())?;
                Ok(Stmt::Binding {
                    name: id.as_str().to_owned(),
                    value,
                })
            }
            Rule::require => {
                let mut requirements = Vec::new();
                for pair in next.into_inner() {
                    requirements.push(Requirement::parse(pair)?);
                }
                Ok(Stmt::Require { requirements })
            }
            Rule::expect => {
                let mut expectations = Vec::new();
                for pair in next.into_inner() {
                    expectations.push(Expectation::parse(pair)?);
                }
                Ok(Stmt::Expect { expectations })
            }
            Rule::code => {
                let code = next.as_str();
                Ok(Stmt::RustCode(code.into()))
            }
            _ => unreachable!(),
        }
    }
}

impl ToRust for Command {
    fn to_rust(self, mode: Mode) -> Result<TokenStream> {
        let Command { kind, fields } = self;
        let kind = format_ident!("{}", kind.as_ref());
        let fields = fields
            .into_iter()
            .map(|f| ToRust::to_rust(f, mode))
            .collect::<Result<Vec<_>>>()?;

        Ok(quote! {
            #kind {
                #(
                    #fields
                ),*
            }
        })
    }
}

fn parse_descriptor(mut pairs: Pairs<Rule>) -> Result<Descriptor> {
    let mut name = String::new();
    let mut sighashes = vec![];
    let mut command = None;
    let mut tx_fee = None;
    let mut pass_fail = TestKind::Pass;
    let mut stmts = Vec::new();
    let mut request = None;

    let meta = pairs.next().unwrap();
    for m in meta.into_inner() {
        match m.as_rule() {
            Rule::testname => {
                let mut inner = m.into_inner();
                let value = inner.next().expecting(Rule::string)?;
                let value = value.into_inner().next().expecting(Rule::inner)?;
                name = value.as_str().to_owned();
            }
            Rule::sighashes => {
                let arr = m.into_inner();
                for id in arr {
                    let id = id.expecting(Rule::ident)?;
                    sighashes.push(id.as_str().to_owned());
                }
            }
            Rule::command => {
                let construct = m.into_inner().next().expecting(Rule::constructor)?;
                let mut parts = construct.into_inner();
                let name = parts.next().expecting(Rule::ident)?;
                let mapping = parts.next().expecting(Rule::struct_map)?;
                let fields = Fields::parse(mapping)?;
                let kind = name.as_str().parse::<CommandKind>()?;
                command = Some(Command { fields, kind })
            }
            Rule::tx_fee => {
                let expr = m.into_inner().next().expecting(Rule::expr)?;
                let value = Expr::parse(expr)?;
                tx_fee = Some(value);
            }
            Rule::passfail => {
                let mut inner = m.into_inner();
                match inner.next() {
                    Some(pair) => {
                        let expected = pair.expecting(Rule::expr)?;
                        let expr = Expr::parse(expected)?;
                        pass_fail = TestKind::Fail(expr);
                    }
                    None => {
                        pass_fail = TestKind::Pass;
                    }
                }
            }
            Rule::request => {
                let expr = Expr::parse(m.into_inner().next())?;
                request = Some(expr);
            }
            foo => panic!("Unexpected rule {:?}", foo),
        }
    }
    for pair in pairs {
        if let Rule::statement = pair.as_rule() {
            stmts.push(Stmt::parse(pair)?);
        } else if let Rule::EOI = pair.as_rule() {
            break;
        } else {
            panic!("Bad {:?}", pair)
        }
    }
    Ok(Descriptor {
        name,
        sighashes,
        command: command.unwrap(),
        tx_fee,
        pass_fail,
        stmts,
        request,
    })
}

#[derive(Clone, Copy, Debug)]
pub enum Mode {
    Integration,
    Unit,
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
                let ts = TokenStream::from_str(&code).unwrap();
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
                    let id = sig_to_walletid(&sig);
                    let sig = format_ident!("{}", sig);
                    let amount = amount.to_rust(mode)?;
                    quote! {
                        {
                            let collect_coins = ccprocessor_rust::handler::CollectCoins {
                                amount: { #amount.into() },
                                eth_address: "dummy".into(),
                                blockchain_tx_id: "setup".into(),
                            };
                            let response = send_command(collect_coins, ports, None);
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
                                tx_ctx,
                                #id,
                                #ret,
                                None,
                            );
                        }
                    }
                }
                Expectation::SetStateEntry { id, value } => todo!(),
                Expectation::DeleteStateEntry { id } => todo!(),
                Expectation::SetStateEntries { values } => {
                    let (keys, values): (Vec<_>, Vec<_>) = values
                        .into_iter()
                        .map(|(k, v)| (k.to_rust(mode), v.to_rust(mode)))
                        .unzip();

                    let keys: Vec<_> = keys.into_iter().try_collect()?;
                    let values: Vec<_> = values.into_iter().try_collect()?;

                    quote! {
                        expect_set_state_entries(
                            &mut tx_ctx,
                            vec![
                                #(
                                    (#keys.to_string(), #values.into())
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
                        {
                            expect_set_state_entry(#id.to_string(), #value.into()).unwrap();
                        }
                    }
                }
                Expectation::SetStateEntries { values } => {
                    let (keys, values): (Vec<_>, Vec<_>) = values
                        .into_iter()
                        .map(|(k, v)| (k.to_rust(mode), v.to_rust(mode)))
                        .unzip();

                    let keys: Vec<_> = keys.into_iter().try_collect()?;
                    let values: Vec<_> = values.into_iter().try_collect()?;

                    quote! {
                        {
                            expect_set_state_entries(
                                ports,
                                vec![
                                    #(
                                        (#keys.to_string(), #values.into())
                                    ),*
                                ]
                            ).unwrap();
                        }
                    }
                }
                Expectation::DeleteStateEntry { id } => todo!(),
                Expectation::GetBalance { .. }
                | Expectation::GetStateEntry { .. }
                | Expectation::GetSighash { .. }
                | Expectation::GetGuid { .. } => quote! {},
            },
        })
    }
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

            let sighash_decls = quote! {
                #(
                    let #sighash_ids = SigHash::from(#sighashes);
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

            let fns = quote! {
                fn wallet_with(balance: Option<impl Into<Integer> + Clone>) -> Option<Vec<u8>> {
                    balance.map(|b| {
                        let wallet = ccprocessor_rust::protos::Wallet {
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

            let sighash_decls = quote! {
                #(
                    let #sighash_ids = SigHash::from(#sighashes);
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

            let stmts: Vec<_> = descriptor
                .stmts
                .into_iter()
                .map(|f| ToRust::to_rust(f, mode))
                .try_collect()?;

            let cmd = "command";
            let guid = command_to_guid(cmd);
            let execute = if let TestKind::Fail(err) = descriptor.pass_fail {
                let err = err.to_rust(mode)?;
                quote! {
                    execute_failure(command, #err, Some(Nonce::from(#guid)));
                }
            } else {
                quote! {
                    execute_success(command, ports, Some(Nonce::from(#guid)));
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
            println!("\n{}", decl);

            Ok(decl)
        }
    }
}

fn main() -> Result<()> {
    color_eyre::install()?;
    let contents = fs::read_to_string("test-descriptions")?;
    let result = DescriptorParser::parse(Rule::descriptor, &contents)?;
    let descriptor = parse_descriptor(result)?;
    // println!("{:?}", descriptor);
    let rust = descriptor.to_rust(Mode::Integration)?;
    Ok(())
}
