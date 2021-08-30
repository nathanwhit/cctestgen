use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::{
    parenthesized, parse_quote, DeriveInput, ExprField, ExprParen, Field, Fields, Ident, Index,
    Member,
};

enum FoldAttr {
    Fold(Ident),
    Foldable,
}

fn fold_field(
    field: Field,
    index: Option<&mut usize>,
) -> syn::Result<Option<proc_macro2::TokenStream>> {
    if field.attrs.is_empty() {
        return Ok(None);
    }
    let attr = field
        .attrs
        .into_iter()
        .find_map(|attr| {
            attr.path.get_ident().map(|id| {
                if id == "fold" {
                    let expr_paren: ExprParen = syn::parse2(attr.tokens.clone()).unwrap();
                    let fold_func_suffix = match *expr_paren.expr {
                        syn::Expr::Path(p) => p.path.get_ident().unwrap().clone(),
                        _ => return None,
                    };
                    println!("{}", fold_func_suffix);
                    Some(FoldAttr::Fold(format_ident!("fold_{}", fold_func_suffix)))
                } else if id == "foldable" {
                    Some(FoldAttr::Foldable)
                } else {
                    None
                }
            })
        })
        .flatten();

    let attr = match attr {
        Some(a) => a,
        None => return Ok(None),
    };

    if let Some(ident) = field.ident {
        let ty = field.ty;
        match attr {
            FoldAttr::Fold(func) => Ok(Some(quote! {
                #ident : <F as syn::fold::Fold>::#func(folder, self.#ident),
            })),
            FoldAttr::Foldable => Ok(Some(quote! {
                #ident : <#ty as Foldable>::fold_with(self.#ident, folder),
            })),
        }
    } else {
        match attr {
            FoldAttr::Fold(func) => {
                let index = index.unwrap();
                let member = Member::Unnamed(Index::from(*index));
                *index += 1;
                Ok(Some(quote! {
                    <F as syn::fold::Fold>::#func(folder, self.)
                }))
            }
            FoldAttr::Foldable => todo!(),
        }
    }
}

#[proc_macro_derive(Foldable, attributes(fold, foldable))]
pub fn derive_foldable(input: TokenStream) -> TokenStream {
    let DeriveInput {
        attrs: _,
        vis: _,
        ident,
        generics,
        data,
    } = syn::parse_macro_input!(input);

    let imp = match data {
        syn::Data::Struct(s) => {
            let tuple_struct = matches!(s.fields, Fields::Unnamed(_));
            if s.fields.len() == 1 {
                let field = s.fields.into_iter().next().unwrap();
                let ty = field.ty;
                if tuple_struct {
                    quote! {
                        Self(<#ty as Foldable>::fold_with(self.0, folder))
                    }
                } else {
                    let id = field.ident.unwrap();
                    quote! {
                        Self {
                            #id : <#ty as Foldable>::fold_with(self.#id, folder)
                        }
                    }
                }
            } else {
                let mut fields = proc_macro2::TokenStream::new();
                let mut count = 0;
                for field in s.fields {
                    if let Ok(Some(ts)) = fold_field(field, tuple_struct.then(|| &mut count)) {
                        fields.extend(ts);
                    }
                }
                let rest = quote! { ..self };
                if tuple_struct {
                    quote! {
                        Self( #fields #rest )
                    }
                } else {
                    quote! {
                        Self {
                            #fields #rest
                        }
                    }
                }
            }
        }
        syn::Data::Enum(_) => todo!(),
        syn::Data::Union(_) => todo!(),
    };
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();
    let out = quote! {
        impl #impl_generics Foldable for #ident #ty_generics #where_clause {
            fn fold_with<F>(self, folder: &mut F) -> Self
            where
                F: Fold + ?Sized,
            {
                #imp
            }
        }
    };
    println!("{}", out);
    out.into()
}
