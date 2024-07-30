#![doc(
    html_logo_url = "https://raw.githubusercontent.com/soqb/vermouth/trunk/assets/logo-small.png"
)]
#![doc(
    html_favicon_url = "https://raw.githubusercontent.com/soqb/vermouth/trunk/assets/logo-icon.png"
)]
#![cfg_attr(docsrs, feature(doc_cfg))]
#![allow(clippy::toplevel_ref_arg)]
//! _Fortification against [sin][`syn`]._
//! A new kind of parser for procedural macros.
//!
//! Opposing [`syn`],
//! this crate is designed around the philosophy that malformed input
//! should be handled gracefully by procedural macros.
//!
//! See the methods on the [`Parser`] type for structural documentation.
//!
//! [`syn`]: https://crates.io/crates/syn

#[cfg(not(feature = "proc-macro2"))]
extern crate proc_macro;
#[cfg(feature = "proc-macro2")]
extern crate proc_macro2 as proc_macro;

#[doc(hidden)]
#[macro_export]
macro_rules! ඞ_declare_test {
    () => {
        // this... is unfortunate.
        // we don't want to enable `cfg(feature = "proc-macro2")` for r-a,
        // but we also don't want to disable `cfg(test)` and thus,
        // we enable *another* feature to silence the error.
        #[cfg(not(any(feature = "proc-macro2", feature = "rust-analyzer-hack")))]
        compile_error!(
            "`vermouth` tests must be run with the `proc-macro2` feature.\n\
            `proc-macro` doesn't support execution outside the rustc harness"
        );
        // even if we just spat out a compile error,
        // we still import a (non-functional) crate
        // to suppress errors about bad imports.
        #[cfg(not(feature = "proc-macro2"))]
        extern crate proc_macro;
        #[cfg(feature = "proc-macro2")]
        extern crate proc_macro2 as proc_macro;
    };
}

mod error;
mod ext;
mod parser;
mod pat;
mod span;

pub use self::{error::*, ext::*, parser::*, pat::*, span::*};

#[cfg(feature = "attributes")]
#[cfg_attr(docsrs, doc(cfg(feature = "attributes")))]
pub mod attributes;
pub mod path;
pub mod punctuated;

#[cfg(test)]
mod tests {
    use proc_macro::{Span, TokenTree};

    use crate::{
        attributes::Attribute, punct_pat, Expected, Parse, Parser, ParserPos, Result, Spanned,
    };

    ඞ_declare_test!();

    macro_rules! quote_single {
        (#) => {
            [
                proc_macro::TokenTree::from(proc_macro::Punct::new('#', proc_macro::Spacing::Alone)),
            ]
        };
        (!) => {
            [
                proc_macro::TokenTree::from(proc_macro::Punct::new('!', proc_macro::Spacing::Alone)),
            ]
        };
        (+) => {
            [
                proc_macro::TokenTree::from(proc_macro::Punct::new('+', proc_macro::Spacing::Alone)),
            ]
        };
        (==) => {
            [
                proc_macro::TokenTree::from(proc_macro::Punct::new('=', proc_macro::Spacing::Joint)),
                proc_macro::TokenTree::from(proc_macro::Punct::new('=', proc_macro::Spacing::Alone)),
            ]
        };
        ($i:ident) => {
            [
                proc_macro::TokenTree::from(
                    proc_macro::Ident::new(stringify!($i), proc_macro::Span::call_site()),
                ),
            ]
        };
        ({ $($c:tt)* }) => {
            [
                proc_macro::TokenTree::from(
                    proc_macro::Group::new(proc_macro::Delimiter::Brace, quote!($($c)*)),
                ),
            ]
        };
        ([ $($c:tt)* ]) => {
            [
                proc_macro::TokenTree::from(
                    proc_macro::Group::new(proc_macro::Delimiter::Bracket, quote!($($c)*)),
                ),
            ]
        };
        (( $($c:tt)* )) => {
            [
                proc_macro::TokenTree::from(
                    proc_macro::Group::new(proc_macro::Delimiter::Parenthesis, quote!($($c)*)),
                ),
            ]
        };
    }

    macro_rules! quote {
        ($($t:tt)*) => {{
            let mut buf = proc_macro::TokenStream::new();
            $(
                buf.extend(quote_single!($t));
            )*
            buf
        }};
    }

    #[test]
    fn parsing() {
        let tokens = quote! { a + b == c };
        let ref mut cx = Parser::new(tokens, Span::call_site());
        assert_eq!(
            cx.eat_ident().map(Spanned::from).map(|s| s == "a"),
            Ok(true),
        );
        assert_eq!(cx.eat(punct_pat!(+)).map(drop), Ok(()));
        assert_eq!(
            cx.eat_ident().map(Spanned::from).map(|s| s == "b"),
            Ok(true),
        );
        assert_eq!(cx.eat(punct_pat!(==)).map(drop), Ok(()));
        assert_eq!(
            cx.eat_ident().map(Spanned::from).map(|s| s == "c"),
            Ok(true),
        );
    }

    #[test]
    fn error_reporting() {
        let exp = Expected::nothing(ParserPos::arbitrary())
            .or_lit("foo")
            .or_noun("a bar")
            .or_lit("baz");
        assert_eq!(format!("{exp}"), "expected `foo`, a bar, or `baz`");
    }

    #[test]
    #[cfg(feature = "attributes")]
    fn attributes() {
        let tokens = quote! { #[foo] #![bar] };
        let ref mut cx = Parser::new(tokens, Span::call_site());

        struct Foo;
        struct Bar;

        impl Parse for Foo {
            type Args<'a> = ();
            fn parse_with(cx: &mut Parser, _args: Self::Args<'_>) -> Result<Foo> {
                cx.eat_expectantly(
                    |tok| match tok {
                        TokenTree::Ident(id) if id.to_string() == "foo" => Some(Foo),
                        _ => None,
                    },
                    |pos| Expected::lit(pos, "foo"),
                )
            }
        }

        impl Parse for Bar {
            type Args<'a> = ();
            fn parse_with(cx: &mut Parser, _args: Self::Args<'_>) -> Result<Bar> {
                cx.eat_expectantly(
                    |tok| match tok {
                        TokenTree::Ident(id) if id.to_string() == "bar" => Some(Bar),
                        _ => None,
                    },
                    |pos| Expected::lit(pos, "bar"),
                )
            }
        }

        assert!(matches!(
            <Attribute<Foo, Bar>>::parse(cx),
            Ok(Attribute::Outer { contents: Foo }),
        ));
        assert!(matches!(
            <Attribute<Foo, Bar>>::parse(cx),
            Ok(Attribute::Inner { contents: Bar, .. }),
        ));
    }
}
