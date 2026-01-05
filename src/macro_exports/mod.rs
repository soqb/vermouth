//! The domain-specific library for this crate's macros (especially [`quote`](crate::quote)).

use proc_macro::{Delimiter, Group, Ident, Punct, Spacing, Span, TokenStream};

use crate::{ReparseError, TokenQueue, TryIntoTokens, TtError, TtResult, ctfe};
use std::{convert::Infallible, str::FromStr};

pub use core;
pub use proc_macro;

mod lit;
mod spec;

pub use spec::*;

pub fn assert_tokens_extend(_: &mut TokenStream) {}

#[inline]
pub fn err<T, A, B: From<A>>(e: TtError<A>) -> TtResult<T, B> {
    Err(match e {
        TtError::Reparse(rp) => TtError::Reparse(rp),
        TtError::Misc(m) => TtError::Misc(m.into()),
    })
}

#[inline]
pub fn ok<T, E>(x: T) -> TtResult<T, E> {
    Ok(x)
}

#[inline]
pub fn try_extend_tokens<T: TryIntoTokens>(buf: &mut TokenQueue, t: T) -> TtResult<(), T::Error> {
    t.try_extend_tokens(buf)?;
    Ok(())
}

#[inline]
pub fn try_to_tokens<T: TryIntoTokens>(t: T) -> TtResult<TokenQueue, T::Error> {
    t.try_into_tokens()
}

pub fn push_underscore(q: &mut TokenQueue) {
    q.push(Ident::new("_", Span::call_site()));
}

pub fn push_empty_group(q: &mut TokenQueue, delim: Delimiter) {
    q.push(Group::new(delim, TokenStream::new()));
}

#[derive(Debug, Clone, Copy)]
pub struct Verbatim(pub &'static str);

impl TryIntoTokens for Verbatim {
    type Error = Infallible;

    fn try_extend_tokens(self, q: &mut TokenQueue) -> TtResult<()> {
        let tt = TokenStream::from_str(self.0)
            .map_err(move |lex| ReparseError::from_ident(lex, self.0))?;
        q.push(tt);
        Ok(())
    }
}

pub const fn parse_lifetime(str: &'static str) -> impl TryIntoTokens<Error = Infallible> + Copy {
    #[derive(Clone, Copy)]
    struct Lifetime<T>(T);

    impl<T: TryIntoTokens> TryIntoTokens for Lifetime<T> {
        type Error = T::Error;

        fn try_extend_tokens(self, q: &mut TokenQueue) -> TtResult<(), T::Error> {
            q.push(Punct::new('\'', Spacing::Joint));
            self.0.try_extend_tokens(q)
        }
    }

    // NB: no assert_eq bc const.
    let (a, b) = str.split_at(1);
    assert!(matches!(a.as_bytes(), b"\'"));
    Lifetime(parse_ident(b))
}

#[inline(never)]
pub fn push_punct(q: &mut TokenQueue, chars: &[char]) {
    let Some((&last, rest)) = chars.split_last() else {
        return;
    };

    for &c in rest {
        q.push(Punct::new(c, Spacing::Joint));
    }

    q.push(Punct::new(last, Spacing::Alone));
}

pub const fn parse_ident(s: &'static str) -> impl TryIntoTokens<Error = Infallible> + Copy {
    #[derive(Clone, Copy)]
    enum IdentParse {
        Raw(&'static str),
        Notraw(&'static str),
        Fallback(&'static str),
    }

    impl TryIntoTokens for IdentParse {
        type Error = Infallible;

        #[inline(always)]
        fn try_extend_tokens(self, q: &mut TokenQueue) -> TtResult<()> {
            match self {
                IdentParse::Raw(s) => q.push(Ident::new_raw(s, Span::call_site())),
                IdentParse::Notraw(s) => q.push(Ident::new(s, Span::call_site())),
                IdentParse::Fallback(s) => q.try_extend_from(Verbatim(s))?,
            }
            Ok(())
        }
    }

    if ctfe::bytes_any(s.as_bytes(), b'#') {
        if let Some((prefix, raw)) = s.split_at_checked(2)
            && let b"r#" = prefix.as_bytes()
        {
            IdentParse::Raw(raw)
        } else {
            IdentParse::Fallback(s)
        }
    } else {
        IdentParse::Notraw(s)
    }
}

pub const fn parse_lit_regime(str: &'static str) -> u8 {
    lit::Regime::recognize(str) as u8
}
