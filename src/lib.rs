use proc_macro as pm1;
use proc_macro2::{Delimiter, Literal, TokenStream, TokenTree};
use quote::quote;

#[proc_macro]
pub fn formats(input: pm1::TokenStream) -> pm1::TokenStream {
    let inner = parse(input.into());
    quote! { format!(#inner) }.into()
}
#[proc_macro]
pub fn printlns(input: pm1::TokenStream) -> pm1::TokenStream {
    let inner = parse(input.into());
    quote! { println!(#inner) }.into()
}
#[proc_macro]
pub fn prints(input: pm1::TokenStream) -> pm1::TokenStream {
    let inner = parse(input.into());
    quote! { print!(#inner) }.into()
}

fn parse(input: TokenStream) -> TokenStream {
    let mut format = Vec::new();

    let mut tokens = input.into_iter();
    while let Some(token) = tokens.next() {
        match token {
            TokenTree::Literal(literal) => {
                format.push(literal);
            }
            TokenTree::Group(group) if group.delimiter() == Delimiter::Brace => {
                let mut tokens = group.stream().into_iter();
                match tokens.next() {
                    Some(TokenTree::Ident(ident)) => match ident.to_string().as_str() {
                        "fg" => {
                            match tokens.next() {
                                Some(TokenTree::Punct(punct)) if punct.to_string() == ":" => (),
                                _ => panic!("Expected `:` after identifier `{}`", ident),
                            }
                            match tokens.next() {
                                Some(TokenTree::Ident(ident)) => match ident.to_string().as_str() {
                                    "red" => format.push(Literal::string("\x1b[31m")),
                                    _ => panic!("Unexpected identifier `{}`", ident),
                                },
                                _ => panic!("Expected idenfier after `:` (after `{}`)", ident),
                            }
                        }
                        "reset" => format.push(Literal::string("\x1b[0m")),
                        _ => panic!("Unexpected identifier `{}`", ident),
                    },
                    Some(other) => panic!("Unexpected token `{}`", other),
                    None => panic!("Expected identifier in braces"),
                }
            }
            TokenTree::Punct(punct) if punct.to_string() == "," => break,
            other => panic!("Unexpected token `{}`", other),
        }
    }
    let rest: TokenStream = tokens.collect();

    quote! {
        concat!(#(
            #format,
        )*),
        #rest
    }
}
