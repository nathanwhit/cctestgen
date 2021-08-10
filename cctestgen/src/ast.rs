use std::str::FromStr;

use super::parse::Rule;

use color_eyre::{eyre::eyre, Result};

use pest::iterators::Pair;

use proc_macro2::{Ident, Literal};

use strum_macros::{AsRefStr, EnumString, IntoStaticStr};

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
    pub kind: CommandKind,
    pub fields: Fields,
}

#[derive(Debug, Clone)]
pub enum TestKind {
    Pass,
    Fail(Expr),
}

#[derive(Debug, Clone)]
pub struct Descriptor {
    pub name: String,
    pub sighashes: Vec<String>,
    pub command: Command,
    pub signer: Expr,
    pub tx_fee: Option<Expr>,
    pub pass_fail: TestKind,
    pub stmts: Vec<Stmt>,
    pub request: Option<Expr>,
}

pub trait PairExt<Inner = Self> {
    fn expecting(self, rule: Rule) -> Result<Inner>;

    fn expecting_one_of(self, rules: &[Rule]) -> Result<Inner>;
}

impl PairExt for Pair<'_, Rule> {
    fn expecting_one_of(self, rules: &[Rule]) -> Result<Self> {
        for rule in rules {
            if self.as_rule() == *rule {
                return Ok(self);
            }
        }
        Err(eyre!(
            "expected one of {:?} but found {:?}",
            rules,
            self.as_rule()
        ))
    }
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
    fn expecting_one_of(self, rules: &[Rule]) -> Result<Pair<'a, Rule>> {
        match self {
            Some(p) => {
                for rule in rules {
                    if p.as_rule() == *rule {
                        return Ok(p);
                    }
                }
                Err(eyre!(
                    "expected one of {:?} but found {:?}",
                    rules,
                    p.as_rule()
                ))
            }
            None => Err(eyre!("expected a pair but found None")),
        }
    }
}

type Fields = Vec<StructEntry>;

#[derive(Debug, Clone)]
pub struct Field {
    pub name: Ident,
    pub value: Expr,
}

#[derive(Debug, Clone)]
pub enum StructEntry {
    Pair(Field),
    Single(Expr),
    Update(Expr),
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
    SignerFor(Ident),
    Option(Option<Box<Expr>>),
    MethodCall { value: Box<Expr>, methods: String },
}

#[derive(Debug, Clone)]
pub enum MapEntry {
    Pair(Expr, Expr),
    Single(Expr),
}
pub type Mapping = Vec<MapEntry>;

#[derive(Debug, Clone)]
pub struct Sighash(pub String);

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
        let expr = expr
            .expecting_one_of(&[Rule::expr, Rule::non_method_expr])?
            .into_inner()
            .next()
            .unwrap();

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
            Rule::signer_for => Ok(Expr::SignerFor(syn::parse_str(
                expr.into_inner().next().expecting(Rule::ident)?.as_str(),
            )?)),
            Rule::default => Ok(Expr::Default),
            Rule::method_call => {
                let mut inner = expr.into_inner();
                let value = Box::new(Expr::parse(inner.next())?);
                let methods = inner.next().expecting(Rule::calls)?.as_str().into();
                Ok(Expr::MethodCall { value, methods })
            }
            foo => unreachable!("Bad, we got a {:?}", foo),
        }
    }
}

impl ParseAst for StructEntry {
    fn parse<'a>(pair: impl PairExt<Pair<'a, Rule>>) -> Result<Self> {
        let pair = pair.expecting(Rule::struct_pair)?;
        let mut inner = pair.into_inner();
        let first = inner.next().unwrap();
        match first.as_rule() {
            Rule::field_pair => {
                let mut parts = first.into_inner();
                let name = parts.next().expecting(Rule::ident)?;
                let value = Expr::parse(parts.next())?;
                Ok(Self::Pair(Field {
                    name: syn::parse_str(name.as_str())?,
                    value,
                }))
            }
            Rule::update => {
                let value = Expr::parse(first.into_inner().next())?;
                Ok(Self::Update(value))
            }
            Rule::short => {
                let value = Expr::parse(first.into_inner().next())?;
                Ok(Self::Single(value))
            }
            other => unreachable!("bad rule in struct entry: {:?}", other),
        }
    }
}

impl ParseAst for Fields {
    fn parse<'a>(pair: impl PairExt<Pair<'a, Rule>>) -> Result<Self> {
        let mapping = pair.expecting(Rule::struct_map)?;
        let mut fields = vec![];
        for pair in mapping.into_inner() {
            fields.push(StructEntry::parse(pair)?);
        }
        Ok(fields)
    }
}

impl ParseAst for Mapping {
    fn parse<'a>(pair: impl PairExt<Pair<'a, Rule>>) -> Result<Self> {
        let mapping = pair.expecting(Rule::mapping)?;
        let mut items = Mapping::new();
        for pair in mapping.into_inner() {
            items.push(MapEntry::parse(pair)?);
        }
        Ok(items)
    }
}

impl ParseAst for MapEntry {
    fn parse<'a>(pair: impl PairExt<Pair<'a, Rule>>) -> Result<Self> {
        let entry = pair.expecting(Rule::pair)?;
        let mut inner = entry.into_inner();
        let e = inner.next().ok_or_else(|| eyre!("expected item"))?;
        match e.as_rule() {
            Rule::expr => {
                let key = Expr::parse(e)?;
                let value = Expr::parse(inner.next())?;
                Ok(Self::Pair(key, value))
            }
            Rule::short => {
                let value = Expr::parse(e.into_inner().next())?;
                Ok(Self::Single(value))
            }
            other => unreachable!("Unexpected rule {:?}", other),
        }
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

impl ParseAst for Descriptor {
    fn parse<'a>(pair: impl PairExt<Pair<'a, Rule>>) -> Result<Self> {
        let pair = pair.expecting(Rule::descriptor)?;
        let mut pairs = pair.into_inner();

        let mut name = String::new();
        let mut sighashes = vec![];
        let mut command = None;
        let mut tx_fee = None;
        let mut pass_fail = TestKind::Pass;
        let mut stmts = Vec::new();
        let mut request = None;
        let mut signer = None;

        let meta = pairs.next().expecting(Rule::meta)?;
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
                Rule::signer => {
                    let expr = Expr::parse(m.into_inner().next())?;
                    signer = Some(expr);
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
            signer: signer.ok_or_else(|| eyre!("Must specify the signer of the command"))?,
            tx_fee,
            pass_fail,
            stmts,
            request,
        })
    }
}
