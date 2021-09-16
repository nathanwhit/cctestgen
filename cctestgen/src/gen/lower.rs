use color_eyre::eyre::eyre;
use derive_more::{Deref, DerefMut};
use inflector::Inflector;
use itertools::Itertools;
use quote::{format_ident, quote, IdentFragment, ToTokens};
use rand::SeedableRng;
use rand_chacha::ChaCha12Rng;
use syn::{parse_quote, visit::Visit, Expr, Ident};
use tap::{Pipe, Tap, TapFallible};

use super::*;
use crate::parser::{self, AstVisit, Descriptor, PassFail, Requirement, Statement, Visitable};

pub(crate) struct WalletCounter {
    pub(crate) count: u64,
}

impl WalletCounter {
    pub(crate) fn new() -> Self {
        Self { count: 0 }
    }
}
impl<'ast> Visit<'ast> for WalletCounter {}
impl<'ast> AstVisit<'ast> for WalletCounter {
    fn visit_requirement(&mut self, requirement: &'ast crate::parser::Requirement) {
        if let Requirement::Wallet { .. } = requirement {
            self.count += 1;
        }
    }
}

pub(crate) struct FindLocal {
    in_local: bool,
    found: bool,
    kind: FindCommand,
}

pub(crate) enum FindCommand {
    Name(Ident),
}

impl FindLocal {
    pub(crate) fn named(name: impl IdentFragment) -> Self {
        Self {
            in_local: false,
            found: false,
            kind: FindCommand::Name(format_ident!("{}", name)),
        }
    }
    pub(crate) fn in_ast(&mut self, ast: impl Visitable) -> bool {
        self.in_local = false;
        ast.visit_with(self);
        self.found
    }
}

impl<'ast> Visit<'ast> for FindLocal {
    fn visit_local(&mut self, i: &'ast syn::Local) {
        self.in_local = true;
        self.visit_pat(&i.pat);
        self.in_local = false;
    }
    fn visit_pat_ident(&mut self, i: &'ast syn::PatIdent) {
        match &self.kind {
            FindCommand::Name(name) => {
                if i.ident == *name {
                    self.found = true;
                }
            }
        }
    }
}

pub(crate) struct Cloner;

fn ident_from_path(path: &syn::Path) -> Option<Ident> {
    if path.segments.len() != 1 {
        return None;
    }
    let seg = &path.segments[0];
    Some(seg.ident.clone())
}

impl syn::fold::Fold for Cloner {
    fn fold_expr(&mut self, expr: Expr) -> Expr {
        match expr.clone() {
            Expr::Path(path) => {
                log::info!("Found path = {:?}", path);
                let id = match ident_from_path(&path.path) {
                    Some(id) => id,
                    None => return Expr::Path(path),
                };
                parse_quote! {
                    #id .clone()
                }
            }
            Expr::Field(field) => match *field.base {
                Expr::Path(path) => {
                    let id = match ident_from_path(&path.path) {
                        Some(id) => id,
                        None => return expr,
                    };
                    parse_quote! {
                        #id .clone()
                    }
                }
                expr => return expr,
            },
            expr => expr,
        }
    }
}

pub(crate) struct Mutifier {
    in_local: bool,
}

impl Mutifier {
    #[allow(dead_code)]
    pub(crate) fn new() -> Self {
        Self { in_local: false }
    }
}

impl syn::fold::Fold for Mutifier {
    fn fold_local(&mut self, i: syn::Local) -> syn::Local {
        self.in_local = true;
        let folded = syn::Local {
            pat: self.fold_pat(i.pat),
            ..i
        };
        self.in_local = false;
        folded
    }
    fn fold_pat_ident(&mut self, i: syn::PatIdent) -> syn::PatIdent {
        if self.in_local {
            syn::PatIdent {
                mutability: Some(<syn::Token![mut]>::default()),
                ..i
            }
        } else {
            i
        }
    }
}

pub(crate) struct TestMetaData {
    pub(crate) name: String,
    pub(crate) command: Expr,
    pub(crate) guid: Option<Expr>,
    pub(crate) tx_fee: Option<Expr>,
    pub(crate) pass_fail: PassFail,
    pub(crate) request: Option<Expr>,
    pub(crate) signer: Expr,
    pub(crate) sighashes: Vec<Ident>,
}

fn duplicate_meta(meta: &parser::Meta) {
    // ariadne::Report::build(ariadne::ReportKind::Error, (), )
}

impl TryFrom<Vec<parser::Meta>> for TestMetaData {
    type Error = color_eyre::Report;

    fn try_from(value: Vec<parser::Meta>) -> Result<Self, Self::Error> {
        let mut name = None;
        let mut command = None;
        let mut guid = None;
        let mut tx_fee = None;
        let mut test_kind = None;
        let mut request = None;
        let mut signer = None;
        let mut sighashes = None;
        for meta in value {
            match meta {
                parser::Meta::Command(c) => {
                    command = Some(c.0.value);
                }
                parser::Meta::Guid(g) => {
                    guid = Some(g.0.value);
                }
                parser::Meta::TxFee(fee) => {
                    tx_fee = Some(fee.0.value);
                }
                parser::Meta::TestType(pf) => {
                    test_kind = Some(pf.value);
                }
                parser::Meta::Request(req) => {
                    request = Some(req.0.value);
                }
                parser::Meta::Signer(s) => {
                    signer = Some(s.0.value);
                }
                parser::Meta::SigHashes(sigs) => {
                    sighashes = Some(sigs.idents.into_iter().collect_vec());
                }
                parser::Meta::TestName(t) => {
                    name = Some(t.value);
                }
            }
        }
        let name = name
            .ok_or(eyre!("missing test name from metadata"))?
            .value();
        let command = command.ok_or(eyre!("missing command from metadata"))?;

        let test_kind = test_kind.ok_or(eyre!("missing test result (pass/fail) from metadata"))?;
        let signer = signer.ok_or(eyre!("missing signer from metadata"))?;
        let sighashes = sighashes.ok_or(eyre!("missing list of sighashes from test metadata"))?;
        Ok(TestMetaData {
            name,
            command,
            guid,
            tx_fee,
            pass_fail: test_kind,
            request,
            signer,
            sighashes,
        })
    }
}

impl<'ast> AstVisit<'ast> for FindLocal {}
