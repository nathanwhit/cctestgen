use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::{format_ident, quote, ToTokens};
use syn::{
    parse_quote, Attribute, DeriveInput, ExprParen, Field, Fields, Ident, Index, Member, Pat, Type,
    Variant,
};

#[derive(Clone)]
enum DeriveAttr {
    Syn(Ident),
    SynIter(Ident),
    Ast(Option<Ident>),
    AstIter(Option<Ident>),
}

#[derive(Clone, Copy, PartialEq)]
enum DeriveKind {
    Fold,
    Visit,
}

fn func_from(tokens: TokenStream2, derive_kind: DeriveKind) -> Option<Ident> {
    let expr_paren: ExprParen = syn::parse2(tokens).ok()?;
    let func_suffix = match *expr_paren.expr {
        syn::Expr::Path(p) => p.path.get_ident().unwrap().clone(),
        _ => return None,
    };
    match derive_kind {
        DeriveKind::Fold => Some(format_ident!("fold_{}", func_suffix)),
        DeriveKind::Visit => Some(format_ident!("visit_{}", func_suffix)),
    }
}

fn find_derive_attr(attrs: &[Attribute], derive_kind: DeriveKind) -> Option<DeriveAttr> {
    let attr = attrs
        .iter()
        .find_map(|attr| {
            attr.path.get_ident().map(|id| {
                if id == "syn" {
                    Some(DeriveAttr::Syn(func_from(
                        attr.tokens.clone(),
                        derive_kind,
                    )?))
                } else if id == "ast" {
                    Some(DeriveAttr::Ast(func_from(attr.tokens.clone(), derive_kind)))
                } else if id == "syn_iter" {
                    Some(DeriveAttr::SynIter(func_from(
                        attr.tokens.clone(),
                        derive_kind,
                    )?))
                } else if id == "ast_iter" {
                    Some(DeriveAttr::AstIter(func_from(
                        attr.tokens.clone(),
                        derive_kind,
                    )))
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

// a crude approximation of whether a type matches a name
// only looks at the outermost type, even if nested
fn type_matches(ty: &Type, name: &str) -> bool {
    if let Type::Path(pth) = &ty {
        if let Some(segment) = pth.path.segments.first() {
            if segment.ident == name {
                return true;
            }
        }
    }
    false
}

// enum FieldKind {
//     Option(Box<FieldKind>),
//     Box(Box<FieldKind>),
//     Plain,
// }

fn handle_field(
    field: Field,
    attr: Option<DeriveAttr>,
    adt_kind: AdtKind,
    derive_kind: DeriveKind,
    index: &mut usize,
) -> syn::Result<TokenStream2> {
    let attr = if attr.is_none() {
        find_derive_attr(&field.attrs, derive_kind)
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
    let boxed = type_matches(&ty, "Box");
    let pat = match adt_kind {
        AdtKind::Enum => {
            let p = field_to_pat(&field, *index);
            if boxed {
                quote! { (*#p) }
            } else {
                p.to_token_stream()
            }
        }
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
            } else if boxed {
                quote! { (*self.#member) }
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

    let mut rhs = match derive_kind {
        DeriveKind::Fold => match attr.clone() {
            Some(DeriveAttr::Syn(func)) => quote! {
                folder.#func(#pat)
            },
            Some(DeriveAttr::Ast(_)) => quote! {
                #pat.fold_with(folder)
            },
            Some(DeriveAttr::SynIter(func)) => quote! {
                #pat.into_iter().map(|el| <F as syn::fold::Fold>::#func(folder, el)).collect()
            },
            Some(DeriveAttr::AstIter(_)) => quote! {
                #pat.into_iter().map(|el| el.fold_with(folder)).collect()
            },
            None => pat.clone(),
        },
        DeriveKind::Visit => match attr.clone() {
            Some(DeriveAttr::Syn(func)) => quote! {
                <V as syn::visit::Visit>::#func(visitor, &#pat)
            },
            Some(DeriveAttr::Ast(Some(func))) => quote! {
                <V as AstVisit>::#func(visitor, &#pat)
            },
            Some(DeriveAttr::Ast(None)) => quote! {
                #pat.visit_with(visitor)
            },
            Some(DeriveAttr::SynIter(func)) => quote! {
                #pat.iter().for_each(|el| <V as syn::visit::Visit>::#func(visitor, &el))
            },
            Some(DeriveAttr::AstIter(opt)) => {
                let visit = match opt {
                    Some(func) => quote! { <V as AstVisit>::#func(visitor, &el)},
                    None => quote! { el.visit_with(visitor) },
                };
                quote! {
                    #pat.iter().for_each(|el| #visit)
                }
            }
            None => quote! { () },
        },
    };

    if option && attr.is_some() {
        rhs = match derive_kind {
            DeriveKind::Fold => quote! { #member_pat.map(|#pat| #rhs) },
            DeriveKind::Visit => quote! { #member_pat.as_ref().map(|#pat| #rhs) },
        }
    } else if boxed && attr.is_some() {
        rhs = quote! { ::std::boxed::Box::new(#rhs) };
    }

    match derive_kind {
        DeriveKind::Fold => {
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
        DeriveKind::Visit => Ok(quote! { #rhs; }),
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
    let mut fields = Vec::new();
    let mut tuple_struct = false;
    for (index, field) in variant.fields.iter().enumerate() {
        if field.ident.is_none() {
            tuple_struct = true;
        }
        fields.push(field_to_pat(field, index));
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

fn handle_derive_data(data: syn::Data, derive_kind: DeriveKind) -> TokenStream2 {
    match data {
        syn::Data::Struct(s) => {
            let tuple_struct = matches!(s.fields, Fields::Unnamed(_));
            if s.fields.len() == 1 && tuple_struct {
                match derive_kind {
                    DeriveKind::Fold => quote! {
                        Self(self.0.fold_with(folder))
                    },
                    DeriveKind::Visit => quote! {
                        self.0.visit_with(visitor);
                    },
                }
            } else {
                let mut fields = TokenStream2::new();
                let mut count = 0;
                for field in s.fields {
                    if let Ok(ts) =
                        handle_field(field, None, AdtKind::Struct, derive_kind, &mut count)
                    {
                        fields.extend(ts);
                    }
                }
                match derive_kind {
                    DeriveKind::Fold => {
                        let rest = quote! {};
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
                    DeriveKind::Visit => quote! { #fields },
                }
            }
        }
        syn::Data::Enum(e) => {
            let mut arms = TokenStream2::new();
            for variant in e.variants {
                let pat = variant_to_pat(&variant);
                let var_name = variant.ident;
                let mut tuple_struct = false;
                let fields = if variant.fields.len() == 1 {
                    let field = variant.fields.into_iter().next().unwrap();
                    tuple_struct = field.ident.is_none();
                    let var_attr = find_derive_attr(&variant.attrs, derive_kind);
                    let field_attr = find_derive_attr(&field.attrs, derive_kind);
                    let attr = match (var_attr, field_attr) {
                        (Some(_), Some(attr2)) => Some(attr2),
                        (Some(attr), None) => Some(attr),
                        (None, Some(attr)) => Some(attr),
                        (None, None) if tuple_struct => Some(DeriveAttr::Ast(None)),
                        (None, None) => None,
                    };

                    handle_field(field, attr, AdtKind::Enum, derive_kind, &mut 0).unwrap()
                } else {
                    let mut fields = TokenStream2::new();
                    let mut index = 0;
                    for field in variant.fields {
                        tuple_struct = field.ident.is_none();
                        fields.extend(
                            handle_field(field, None, AdtKind::Enum, derive_kind, &mut index)
                                .unwrap(),
                        );
                    }

                    fields
                };
                let rhs = match derive_kind {
                    DeriveKind::Fold => {
                        if tuple_struct {
                            quote! {
                                Self::#var_name( #fields )
                            }
                        } else {
                            quote! {
                                Self::#var_name { #fields }
                            }
                        }
                    }
                    DeriveKind::Visit => quote! { { #fields } },
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
    }
}

#[proc_macro_derive(Foldable, attributes(syn, ast, syn_iter, ast_iter))]
pub fn derive_foldable(input: TokenStream) -> TokenStream {
    let DeriveInput {
        attrs: _,
        vis: _,
        ident,
        generics,
        data,
    } = syn::parse_macro_input!(input);

    let imp = handle_derive_data(data, DeriveKind::Fold);
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
    out.into()
}

#[proc_macro_derive(Visitable, attributes(ast, syn, ast_iter, syn_iter))]
pub fn derive_visitable(input: TokenStream) -> TokenStream {
    let DeriveInput {
        attrs: _,
        vis: _,
        ident,
        generics,
        data,
    } = syn::parse_macro_input!(input);

    let imp = handle_derive_data(data, DeriveKind::Visit);
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();
    let out = quote! {
        impl #impl_generics Visitable for #ident #ty_generics #where_clause {
            fn visit_with<'ast, V>(&'ast self, visitor: &mut V)
            where
                V: Visit<'ast> + AstVisit<'ast> + ?Sized,
            {
                #imp
            }
        }
    };
    out.into()
}

// #[proc_macro_derive(HasSpan, attributes(span))]
// pub fn derive_has_span(input: TokenStream) -> TokenStream {
//     let DeriveInput {
//         attrs,
//         vis,
//         ident,
//         generics,
//         data,
//     } = syn::parse_macro_input!(input);

//     todo!()
// }
