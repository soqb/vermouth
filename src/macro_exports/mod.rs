mod lit;

use proc_macro::{Delimiter, Group, Ident, Punct, Spacing, Span, TokenStream, TokenTree};

use crate::{ToTokens, TokensExtend, ctfe};

pub use crate::quote::quote_tt_impl;

pub use proc_macro;
pub use std::iter::Extend as StdExtend;

pub fn assert_tokens_extend(_: &mut impl TokensExtend) {}

pub fn new_punct(c: char, spacing: Spacing) -> TokenTree {
    TokenTree::from(Punct::new(c, spacing))
}

pub fn new_group(stream: TokenStream, delimiter: Delimiter) -> TokenTree {
    TokenTree::from(Group::new(delimiter, stream))
}

pub const fn parse_ident<'a>(str: &'a str) -> Option<impl Into<TokenTree> + 'a> {
    enum IdentParse<'a> {
        Raw(&'a str),
        Notraw(&'a str),
    }

    impl<'a> From<IdentParse<'a>> for TokenTree {
        fn from(value: IdentParse<'a>) -> Self {
            match value {
                IdentParse::Raw(s) => Ident::new_raw(s, Span::call_site()).into(),
                IdentParse::Notraw(s) => Ident::new(s, Span::call_site()).into(),
            }
        }
    }
    if let Some((prefix, raw)) = ctfe::split_around(str, b'#') {
        if ctfe::bytes_eq(prefix.as_bytes(), b"r#") {
            Some(IdentParse::Raw(raw))
        } else {
            None
        }
    } else {
        Some(IdentParse::Notraw(str))
    }
}

pub const fn new_lit(
    str: &'static str,
    data: impl lit::LitContents,
) -> Option<lit::DelayedLiteral<impl lit::LitContents>> {
    lit::new_lit(str, data)
}

pub trait SpecMut: Sized {
    type Mut;
    fn consume(self) -> Self::Mut {
        panic!()
    }
}

pub struct Spec<T>(pub *const T);

impl<'a, T> SpecMut for &'a Spec<T> {
    type Mut = &'a mut T;
}

impl<'a, T> SpecMut for Spec<&'a mut T> {
    type Mut = &'a mut T;
}

pub struct TokenF<F>(pub F);

impl<F: Fn(&mut TokenStream)> ToTokens for TokenF<F> {
    fn extend_tokens(&self, buf: &mut TokenStream) {
        (&self.0)(buf)
    }
}
