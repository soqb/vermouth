#![doc(
    html_logo_url = "https://raw.githubusercontent.com/soqb/vermouth/trunk/assets/logo-small.png"
)]
#![doc(
    html_favicon_url = "https://raw.githubusercontent.com/soqb/vermouth/trunk/assets/logo-icon.png"
)]
#![cfg_attr(
    feature = "unstable-diagnostics-backend",
    feature(
        // the `vermouth` feature `"unstable-diagnostics-backend"`
        // requires a nightly toolchain.
        proc_macro_diagnostic,
    )
)]
#![cfg_attr(docsrs, feature(doc_cfg))]
#![warn(missing_docs, clippy::missing_docs_in_private_items)]
#![forbid(unsafe_code)]

//! _Fortification against [sin][`syn`]._
//! A new kind of parser for procedural macros.
//!
//! This crate aims to be a (not-quite) drop-in replacement for
//! [David Tolnay]'s [`proc-macro2`] and [`quote`] crates,
//! and an opinionated alternative to [`syn`].
//!
//! Opposing [`syn`],
//! this crate is designed around the philosophy that malformed input
//! should be handled gracefully by procedural macros.
//!
//! See [our `quote` macro](crate::quote!) and the [`TokenQueue`] type
//! for stream-building operations.
//! See the [`Parser`] type for structural documentation on parsing.
//!
//! [David Tolnay]: https://github.com/dtolnay/
//! [`proc-macro2`]: https://crates.io/crates/proc-macro2
//! [`quote`]: https://crates.io/crates/quote
//! [`syn`]: https://crates.io/crates/syn
//!
#![cfg_attr(
    feature = "internal-document-features",
    doc = document_features::document_features!(
        feature_label = r##"<a class="stab portability" id="feature-{feature}" href="#feature-{feature}"><code>{feature}</code></a>"##
    ),
)]

#[cfg(not(feature = "proc-macro2"))]
extern crate proc_macro;
#[cfg(feature = "proc-macro2")]
extern crate proc_macro2 as proc_macro;

#[cfg(all(feature = "proc-macro2", feature = "unstable-diagnostics-backend"))]
compile_error!(
    "`vermouth` does not support enabling both the `proc-macro2` and `unstable-diagnostics-backend` features \
    due to limitations in the implementation of `proc-macro2`"
);

// /// A hygiene-exploiting hack to allow macros to be exported at non-root paths.
// macro_rules! export_macro {
//     (#[unused_name($unused:ident)] $(#[$attr:meta])* macro_rules! $name:ident { $($t:tt)* }) => {
//         $(#[$attr])*
//         #[macro_export]
//         #[doc(hidden)]
//         macro_rules! $unused {
//             $($t)*
//         }

//         #[doc(inline)]
//         pub use $unused as $name;
//     };
// }

/// Imports either
#[cfg_attr(not(feature = "proc-macro2"), doc = "[`proc_macro`] or `proc_macro2`")]
#[cfg_attr(
    feature = "proc-macro2",
    doc = "`proc_macro` or [`proc_macro2`](::proc_macro2)"
)]
/// depending on feature flags.
///
/// Required as a macro for doc-tests, where crate-private items are not accessible.
#[doc(hidden)]
#[macro_export]
macro_rules! ඞ_declare_test {
    () => {
        // // this... is unfortunate.
        // // we don't want to enable `cfg(feature = "proc-macro2")` for r-a,
        // // but we also don't want to disable `cfg(test)` and thus,
        // // we enable *another* feature to silence the error.
        // #[cfg(not(any(feature = "proc-macro2", feature = "rust-analyzer-hack")))]
        // compile_error!(
        //     "`vermouth` tests must be run with the `proc-macro2` feature.\n\
        //     `proc-macro` doesn't support execution outside the rustc harness"
        // );

        // even if we just spat out a compile error,
        // we still import a (non-functional) crate
        // to suppress errors about bad imports.
        #[cfg(not(feature = "proc-macro2"))]
        extern crate proc_macro;
        #[cfg(feature = "proc-macro2")]
        extern crate proc_macro2 as proc_macro;
    };
}

#[macro_use]
mod ctfe;

#[cfg(feature = "parse")]
mod error;
mod ext;
#[cfg(feature = "parse")]
mod parser;
#[cfg(feature = "parse")]
mod pat;
mod punct;
mod queue;
#[cfg(feature = "quote")]
mod quote;
mod span;
mod to_tokens;

#[cfg(feature = "quote")]
pub use quote::*;

#[cfg(feature = "parse")]
pub use self::{error::*, parser::*, pat::*};

pub use self::{ext::*, queue::*, span::*, to_tokens::*};

#[doc(hidden)]
#[path = "macro_exports/mod.rs"]
pub mod ඞ_macro_exports;

#[cfg(feature = "attributes")]
pub mod attributes;

#[cfg(all(test, feature = "attributes", feature = "quote"))]
mod tests {
    use proc_macro::{Ident, Literal, Span, TokenStream, TokenTree};

    use crate::{
        Expected, Parse, Parser, ParserPos, Result, Spanned, attributes::Attribute, punct_pat,
        quote,
    };

    ඞ_declare_test!();

    #[test]
    fn parsing() {
        let tokens = quote! { a + b == c }.into();
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
    fn quote_interpolate() {
        let a = ();
        let _tokens: TokenStream = quote! { $a }.into();
    }

    #[test]
    fn error_reporting() {
        let exp = Expected::nothing(ParserPos::arbitrary())
            .or_lit("foo")
            .or_noun("a bar")
            .or_lit("baz");
        assert_eq!(exp.to_string(), "expected `foo`, a bar, or `baz`");
    }

    #[test]
    fn parser_indices() {
        #[track_caller]
        fn is_at(cx: &mut Parser, idx: u32) {
            assert_eq!(
                cx.raw_pos().into_raw(),
                idx + 1,
                "is_at: parser indices mismatch"
            );
        }

        #[track_caller]
        fn nibbles_to(cx: &mut Parser, idx: Option<u32>, v: char) {
            let (tt, pos) = cx.nibble();
            assert_eq!(
                tt.and_then(|tt| match tt {
                    TokenTree::Ident(id) => Some(id.to_string()),
                    _ => None,
                }),
                Some(v.to_string()),
            );

            let pos = pos.into_raw();
            let pos = (pos > 0).then_some(pos);

            assert_eq!(pos, idx, "nibbles_to: parser indices mismatch");

            if let Some(idx) = idx {
                is_at(cx, idx);
            }
        }

        let tokens: TokenStream = ('A'..='Z')
            .map(|c| Ident::new(&c.to_string(), Span::call_site()))
            .map(TokenTree::from)
            .collect();
        let ref mut cx = Parser::new(tokens, Span::call_site());

        nibbles_to(cx, Some(1), 'A');
        nibbles_to(cx, Some(2), 'B');

        let ckp = cx.save();
        nibbles_to(cx, Some(3), 'C');
        nibbles_to(cx, Some(4), 'D');
        nibbles_to(cx, Some(5), 'E');

        cx.gag(3);
        nibbles_to(cx, Some(3), 'C');
        nibbles_to(cx, Some(4), 'D');
        nibbles_to(cx, Some(5), 'E');

        cx.restore(&ckp);
        nibbles_to(cx, Some(3), 'C');

        is_at(cx, 3);

        let exp = cx
            .eat_expectantly(|_| <Option<()>>::None, Expected::nothing)
            .unwrap_err();

        nibbles_to(cx, Some(5), 'E');
        nibbles_to(cx, Some(6), 'F');

        exp.recover(cx);
        is_at(cx, 3);
        nibbles_to(cx, Some(4), 'D');
    }

    #[track_caller]
    fn assert_streams_match(real_a: TokenStream, real_b: TokenStream) {
        #[track_caller]
        fn assert_tts_match(a: TokenTree, b: TokenTree) {
            match (&a, &b) {
                (TokenTree::Group(a), TokenTree::Group(b)) => {
                    assert_eq!(a.delimiter(), b.delimiter());
                    assert_streams_match(a.stream(), b.stream());
                }
                (TokenTree::Ident(_), TokenTree::Ident(_))
                | (TokenTree::Punct(_), TokenTree::Punct(_))
                | (TokenTree::Literal(_), TokenTree::Literal(_)) => {
                    assert_eq!(a.to_string(), b.to_string())
                }
                _ => panic!("{a:?} and {b:?} did not match"),
            }
        }

        let (mut a_len, mut b_len) = (0, 0);
        let mut a = real_a.clone().into_iter();
        let mut b = real_b.clone().into_iter();
        loop {
            let (a, b) = (a.next(), b.next());
            a_len += a.is_some() as usize;
            b_len += b.is_some() as usize;
            match (a, b) {
                (None, None) => return,
                (Some(a), Some(b)) => assert_tts_match(a, b),
                _ => {
                    panic!("stream len mismatch. {real_a:?} has {a_len} but {real_b:?} has {b_len}")
                }
            }
        }
    }

    /// An inference guide.
    fn tt(tt: impl Into<TokenTree>) -> TokenTree {
        tt.into()
    }

    #[test]
    fn quote_literals() {
        let quoted = quote! {
            144
            12u8
            7f64
            7.
            "foobar"
            r#"forbar"#
            c"coobar"
            cr#"corbar"#
            b"boobar"
            br#"borbar"#
            'x'
            b'y'
        }
        .into();
        let manual = [
            tt(Literal::u8_unsuffixed(144)),
            tt(Literal::u8_suffixed(12)),
            tt(Literal::f64_suffixed(7.)),
            tt(Literal::f32_unsuffixed(7.)),
            tt(Literal::string("foobar")),
            tt(Literal::string("forbar")),
            tt(Literal::c_string(c"coobar")),
            tt(Literal::c_string(c"corbar")),
            tt(Literal::byte_string(b"boobar")),
            tt(Literal::byte_string(b"borbar")),
            tt(Literal::character('x')),
            tt(Literal::byte_character(b'y')),
        ]
        .into_iter()
        .collect();
        assert_streams_match(quoted, manual);
    }

    #[test]
    #[cfg(feature = "attributes")]
    fn attributes() {
        let tokens = quote! { #[foo] #![bar] }.into();
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
