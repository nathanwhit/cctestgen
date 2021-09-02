use proc_macro::TokenStream;
use quote::{format_ident, quote, ToTokens};
use syn::{Attribute, DeriveInput, ExprParen, Field, Fields, Ident, Index, Member, Pat, Type, Variant, parse_quote};

#[derive(Clone)]
enum FoldAttr {
    Fold(Ident),
    FoldIter(Ident),
    Foldable,
    FoldableIter,
}

fn fold_func_from(tokens: proc_macro2::TokenStream) -> Option<Ident> {
    let expr_paren: ExprParen = syn::parse2(tokens).unwrap();
    let fold_func_suffix = match *expr_paren.expr {
        syn::Expr::Path(p) => p.path.get_ident().unwrap().clone(),
        _ => return None,
    };
    Some(format_ident!("fold_{}", fold_func_suffix))
}

fn find_fold_attr(attrs: &[Attribute]) -> Option<FoldAttr> {
    let attr = attrs
        .into_iter()
        .find_map(|attr| {
            attr.path.get_ident().map(|id| {
                if id == "fold" {
                    Some(FoldAttr::Fold(fold_func_from(attr.tokens.clone())?))
                } else if id == "foldable" {
                    Some(FoldAttr::Foldable)
                } else if id == "fold_iter" {
                    Some(FoldAttr::FoldIter(fold_func_from(attr.tokens.clone())?))
                } else if id == "foldable_iter" {
                    Some(FoldAttr::FoldableIter)
                } else {
                    None
                }
            })
        })
        .flatten();
    attr
}

#[derive(Debug, Clone, Copy)]
enum AdtKind {
    Struct,
    Enum,
}

fn fold_field(
    field: Field,
    attr: Option<FoldAttr>,
    adt_kind: AdtKind,
    index: &mut usize,
) -> syn::Result<proc_macro2::TokenStream> {
    let attr = if attr.is_none() {
        find_fold_attr(&field.attrs)
    } else {
        attr
    };

    let ty = field.ty.clone();
    let mut option = false;
    if let Type::Path(pth) = &ty {
        if let Some(segment) = pth.path.segments.first() {
            if segment.ident == "Option" {
                option = true;
            }
        }
    }
    let pat = match adt_kind.clone() {
        AdtKind::Enum => field_to_pat(&field, *index).to_token_stream(),
        AdtKind::Struct => {
            let member = if let Some(ident) = field.ident.clone() {
                Member::Named(ident)
            } else {
                let m = Member::Unnamed(Index::from(*index));
                *index += 1;
                m
            };
            if option && attr.is_some() {
                quote! { #member }
            } else {
                quote! {
                    self.#member
                }
            }
        }
    };
    let member_pat = if let AdtKind::Struct = adt_kind {
        quote! { self.#pat }
    } else {
        quote! { #pat }
    };
   
    let mut rhs = match attr.clone() {
        Some(FoldAttr::Fold(func)) => quote! {
            <F as syn::fold::Fold>::#func(folder, #pat)
        },
        Some(FoldAttr::Foldable) =>quote! {
            <#ty as Foldable>::fold_with(#pat, folder)
        },
        Some(FoldAttr::FoldIter(func)) => quote! {
            #pat.into_iter().map(|el| <F as syn::fold::Fold>::#func(folder, el)).collect()
        },
        Some(FoldAttr::FoldableIter) => quote! {
            #pat.into_iter().map(|el| el.fold_with(folder)).collect()
        },
        None => quote! {
            #pat
        },
    };

    if option && attr.is_some()  {
        rhs = quote! { #member_pat.map(|#pat| #rhs) };
    }
    
    if let Some(ident) = field.ident {
        Ok(quote! {
            #ident : #rhs,
        })
    } else {
        Ok(quote! {
            #rhs,
        })
    }
}

fn field_to_pat(field: &Field, index: usize) -> Pat {
    let pat_name = if let Some(ident) = &field.ident {
        ident.clone()
    } else {
        format_ident!("__field_{}", index)
    };

    parse_quote! {
        #pat_name
    }
}

fn variant_to_pat(variant: &Variant) -> Pat {
    let mut index = 0;
    let mut fields = Vec::new();
    let mut tuple_struct = false;
    for field in &variant.fields {
        if field.ident.is_none() {
            tuple_struct = true;
        }
        fields.push(field_to_pat(field, index));
        index += 1;
    }
    let var_name = variant.ident.clone();
    let body = if tuple_struct {
        quote! { ( #(#fields),* ) }
    } else {
        quote! { { #(#fields),* } }
    };
    parse_quote! {
        Self::#var_name #body
    }
}

#[proc_macro_derive(Foldable, attributes(fold, foldable, fold_iter, foldable_iter))]
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
            if s.fields.len() == 1 && tuple_struct {
                let field = s.fields.into_iter().next().unwrap();
                let ty = field.ty;
                quote! {
                    Self(<#ty as Foldable>::fold_with(self.0, folder))
                }
                
            } else {
                let mut fields = proc_macro2::TokenStream::new();
                let mut count = 0;
                for field in s.fields {
                    if let Ok(ts) = fold_field(field, None, AdtKind::Struct, &mut count) {
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
        syn::Data::Enum(e) => {
            let mut arms = proc_macro2::TokenStream::new();
            for variant in e.variants {
                let pat = variant_to_pat(&variant);
                let var_name = variant.ident;
                let mut tuple_struct = false;
                let fields = if variant.fields.len() == 1 {
                    let field = variant.fields.into_iter().next().unwrap();
                    tuple_struct = field.ident.is_none();
                    let var_attr = find_fold_attr(&variant.attrs);
                    let field_attr = find_fold_attr(&field.attrs);
                    let attr = match (var_attr, field_attr) {
                        (Some(_), Some(attr2)) => attr2,
                        (Some(attr), None) => attr,
                        (None, Some(attr)) => attr,
                        (None, None) => FoldAttr::Foldable,
                    };
                    let field = fold_field(field, Some(attr), AdtKind::Enum, &mut 0).unwrap();
                    field
                } else {
                    let mut fields = proc_macro2::TokenStream::new();
                    let mut index = 0;
                    for field in variant.fields {
                        tuple_struct = field.ident.is_none();
                        fields.extend(fold_field(field, None, AdtKind::Enum, &mut index).unwrap());
                    }

                    fields
                };
                let rhs = if tuple_struct {
                    quote! {
                        Self::#var_name( #fields )
                    }
                } else {
                    quote! {
                        Self::#var_name { #fields }
                    }
                };
                arms.extend(quote! {
                    #pat => #rhs,
                });
            }
            quote! {
                match self {
                    #arms
                }
            }
        }
        syn::Data::Union(_) => unimplemented!(),
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
