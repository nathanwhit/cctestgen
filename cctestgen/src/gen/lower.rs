use std::collections::HashSet;

use color_eyre::eyre::eyre;

use itertools::Itertools;
use quote::{format_ident, IdentFragment};

use syn::{fold::Fold, parse_quote, visit::Visit, Expr, ExprCall, Ident};

use super::*;
use crate::parser::{self, AstVisit, PassFail, Requirement, Visitable};

pub(crate) struct WalletCounter {
    pub(crate) count: u64,
    seen: HashSet<Requirement>,
}

impl WalletCounter {
    pub(crate) fn new() -> Self {
        Self {
            count: 0,
            seen: HashSet::new(),
        }
    }
}
impl<'ast> Visit<'ast> for WalletCounter {}
impl<'ast> AstVisit<'ast> for WalletCounter {
    fn visit_requirement(&mut self, requirement: &'ast crate::parser::Requirement) {
        if let Requirement::Wallet { .. } = requirement {
            if self.seen.insert(requirement.clone()) {
                self.count += 1;
            }
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
        // println!("{:?}", expr);
        match expr.clone() {
            Expr::Path(path) => {
                if let Some(id) = ident_from_path(&path.path) {
                    if id != "tse" && !id.to_string().as_str().ends_with("signer") {
                        // println!("CLONING {:?}", expr);
                        return parse_quote!( #id.clone() );
                    }
                }
            }
            Expr::Field(field) => match *field.base {
                Expr::Path(path) => {
                    if let Some(id) = ident_from_path(&path.path) {
                        return parse_quote!( #id.clone() );
                    }
                }
                _ => {}
            },
            Expr::Call(call) => {
                return Expr::Call(ExprCall {
                    args: call.args.into_iter().map(|a| self.fold_expr(a)).collect(),
                    ..call
                });
            }
            _ => {}
        }
        syn::fold::fold_expr(self, expr)
    }

    fn fold_field_value(&mut self, i: syn::FieldValue) -> syn::FieldValue {
        let mut folded = syn::fold::fold_field_value(self, i.clone());
        if folded != i {
            folded.colon_token = Some(syn::token::Colon::default());
        }
        folded
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
            pat: self.fold_pat(i.pat.clone()),
            ..syn::fold::fold_local(self, i.clone())
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

pub(crate) struct MacroDesugarer;

impl Fold for MacroDesugarer {
    fn fold_expr(&mut self, i: Expr) -> Expr {
        if let Expr::Macro(macro_expr) = i.clone() {
            if let Some(ident) = macro_expr.mac.path.get_ident() {
                match ident.to_string().as_str() {
                    "Guid" => {
                        if macro_expr.mac.tokens.is_empty() {
                            return parse_quote!(Guid::new());
                        }
                        if let Ok(id) = syn::parse2::<Ident>(macro_expr.mac.tokens) {
                            let guid = super::to_rust::command_to_guid(id);
                            return parse_quote!(#guid);
                        }
                    }
                    "SigHash" => {
                        if let Ok(id) = syn::parse2::<Ident>(macro_expr.mac.tokens) {
                            return parse_quote!(#id);
                        }
                    }
                    "WalletId" => {
                        if let Ok(id) = syn::parse2::<Ident>(macro_expr.mac.tokens) {
                            let wallet_id = super::to_rust::sig_to_walletid(id);
                            return parse_quote!(#wallet_id);
                        }
                    }
                    "Signer" => {
                        if let Ok(id) = syn::parse2::<Ident>(macro_expr.mac.tokens) {
                            let signer = super::to_rust::sig_to_signer(id);
                            return parse_quote!(#signer);
                        }
                    }
                    _ => {}
                }
            }
        }
        syn::fold::fold_expr(self, i)
    }
}

pub(crate) fn transform(descriptors: Descriptors) -> Descriptors {
    let mut mutifier = Mutifier::new();
    descriptors
        .fold_with(&mut MacroDesugarer)
        .fold_with(&mut Cloner)
        .fold_with(&mut mutifier)
}

pub(crate) struct TestMetaData {
    pub(crate) name: String,
    #[allow(dead_code)]
    pub(crate) command: Expr,
    pub(crate) guid: Option<Expr>,
    pub(crate) tx_fee: Option<Expr>,
    pub(crate) pass_fail: PassFail,
    pub(crate) request: Option<Expr>,
    pub(crate) signer: Expr,
    pub(crate) sighashes: Vec<Ident>,
}

#[allow(dead_code)]
fn duplicate_meta(_meta: &parser::Meta) {
    // ariadne::Report::build(ariadne::ReportKind::Error, (), )
}

impl TryFrom<Vec<parser::MetaDatum>> for TestMetaData {
    type Error = color_eyre::Report;

    fn try_from(value: Vec<parser::MetaDatum>) -> Result<Self, Self::Error> {
        let mut name = None;
        let mut command = None;
        let mut guid = None;
        let mut tx_fee = None;
        let mut test_kind = None;
        let mut request = None;
        let mut signer = None;
        let mut sighashes = None;
        for datum in value {
            match datum.meta {
                parser::Meta::Command(c) => {
                    command = Some(c.value().clone());
                }
                parser::Meta::Guid(g) => {
                    guid = Some(g.value().clone());
                }
                parser::Meta::TxFee(fee) => {
                    tx_fee = Some(fee.value().clone());
                }
                parser::Meta::TestType(pf) => {
                    test_kind = Some(pf.value);
                }
                parser::Meta::Request(req) => {
                    request = Some(req.value().clone());
                }
                parser::Meta::Signer(s) => {
                    signer = Some(s.value().clone());
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
            .ok_or_else(|| eyre!("missing test name from metadata"))?
            .value();
        let command = command.ok_or_else(|| eyre!("missing command from metadata"))?;

        let test_kind =
            test_kind.ok_or_else(|| eyre!("missing test result (pass/fail) from metadata"))?;
        let signer = signer.ok_or_else(|| eyre!("missing signer from metadata"))?;
        let sighashes =
            sighashes.ok_or_else(|| eyre!("missing list of sighashes from test metadata"))?;
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
