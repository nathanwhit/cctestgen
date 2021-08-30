#![allow(unused)]

use cctestgen_internal_macros::Foldable;
use derive_syn_parse::Parse;
use quote::IdentFragment;
use syn::{
    braced, bracketed,
    fold::Fold,
    parse::{Parse, ParseStream},
    punctuated::Punctuated,
    token::{self, Bracket},
    Block, Expr, ExprArray, Ident, LitStr, Stmt, Token,
};

mod kw {
    syn::custom_keyword!(rust);
    syn::custom_keyword!(signer);
    syn::custom_keyword!(tx_fee);
    syn::custom_keyword!(request);
    syn::custom_keyword!(command);
    syn::custom_keyword!(name);
    syn::custom_keyword!(result);
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
    syn::custom_keyword!(balance);
    syn::custom_keyword!(state);
    syn::custom_keyword!(states);
    syn::custom_keyword!(sighash);
    syn::custom_keyword!(sighashes);
    syn::custom_keyword!(verify);
}

#[derive(Parse)]
struct RustBlock {
    rust_token: kw::rust,
    block: Block,
}

#[derive(Foldable, Parse)]
struct MetaItem<Name: Parse> {
    name_token: Name,
    equals_token: Token![=],
    #[fold(expr)]
    value: Expr,
    semi: Option<Token![;]>,
}

// impl<Name: Parse> Parse for MetaItem<Name> {
//     fn parse(input: ParseStream) -> syn::Result<Self> {
//         Ok(Self {
//             name_token: input.parse()?,
//             equals_token: input.parse()?,
//             value: input.parse()?,
//         })
//     }
// }

#[derive(Parse, Foldable)]
struct TestNameMeta {
    name_token: kw::name,
    equals_token: Token![=],
    #[fold(lit_str)]
    value: LitStr,
}

#[derive(Foldable)]
struct SigHashesMeta {
    name_token: kw::sighashes,
    equals_token: Token![=],
    bracket_token: Bracket,
    idents: Punctuated<Ident, Token![,]>,
}

impl Parse for SigHashesMeta {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let name_token = input.parse()?;
        let equals_token = input.parse()?;
        let content;
        let bracket_token = bracketed!(content in input);
        let idents = Punctuated::parse_terminated(input)?;
        Ok(Self {
            name_token,
            equals_token,
            bracket_token,
            idents,
        })
    }
}

#[derive(Parse, Foldable)]
struct CommandMeta(MetaItem<kw::command>);

#[derive(Parse, Foldable)]
struct SignerMeta(MetaItem<kw::signer>);

#[derive(Parse, Foldable)]
struct TxFeeMeta(MetaItem<kw::tx_fee>);

#[derive(Parse, Foldable)]
struct RequestMeta(MetaItem<kw::request>);

#[derive(Parse)]
enum PassFail {
    #[peek(kw::pass, name = "pass")]
    Pass,
    #[peek(kw::fail, name = "fail")]
    Fail {
        err_token: kw::err,
        equals_token: Token![=],
        err: Expr,
    },
}

#[derive(Parse)]
struct TestTypeMeta {
    result_token: kw::result,
    equals_token: Token![=],
    value: PassFail,
}

trait Foldable {
    fn fold_with<F>(self, folder: &mut F) -> Self
    where
        F: Fold + ?Sized;
}
