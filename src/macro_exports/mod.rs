//! The domain-specific library for this crate's macros (especially [`try_quote`](crate::try_quote)).

use proc_macro::{Delimiter, Group, Ident, Punct, Spacing, Span, TokenStream};

use crate::{ReparseError, TokenQueue, TryToTokens, TtError, TtResult, ctfe};
use std::{convert::Infallible, error::Error, str::FromStr};

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
pub fn try_extend_tokens<T: TryToTokens>(buf: &mut TokenQueue, t: T) -> TtResult<(), T::Error> {
    buf.reserve(t.queue_size_hint().0);
    t.try_extend_tokens(buf)?;
    Ok(())
}

#[inline]
pub fn try_to_tokens<T: TryToTokens>(t: T) -> TtResult<TokenQueue, T::Error> {
    t.try_to_tokens()
}

pub fn push_underscore(q: &mut TokenQueue) {
    q.push(Ident::new("_", Span::call_site()));
}

pub fn push_empty_group(q: &mut TokenQueue, delim: Delimiter) {
    q.push(Group::new(delim, TokenStream::new()));
}

pub const fn parse_lifetime(str: &'static str) -> impl TryToTokens<Error = Infallible> + Copy {
    #[derive(Clone, Copy)]
    struct Lifetime<T>(T);

    impl<T: TryToTokens> TryToTokens for Lifetime<T> {
        type Error = T::Error;

        fn try_extend_tokens(&self, q: &mut TokenQueue) -> TtResult<(), T::Error> {
            q.push(Punct::new('\'', Spacing::Joint));
            self.0.try_extend_tokens(q)
        }
    }

    // NB: no assert_eq bc const.
    assert!(str.as_bytes()[0] == b'\'');
    Lifetime(parse_ident(str.split_at(1).1))
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

pub const fn parse_ident(str: &'static str) -> impl TryToTokens<Error = Infallible> + Copy {
    #[derive(Clone, Copy)]
    enum IdentParse {
        Raw(&'static str),
        Notraw(&'static str),
        Fallback(&'static str),
    }

    impl TryToTokens for IdentParse {
        type Error = Infallible;

        #[inline(always)]
        fn try_extend_tokens(&self, q: &mut TokenQueue) -> TtResult<()> {
            match self {
                IdentParse::Raw(s) => q.push(Ident::new_raw(s, Span::call_site())),
                IdentParse::Notraw(s) => q.push(Ident::new(s, Span::call_site())),
                IdentParse::Fallback(s) => {
                    let tt = TokenStream::from_str(s)
                        .map_err(move |lex| ReparseError::from_ident(lex, s))?;
                    q.extend(tt);
                }
            }
            Ok(())
        }
    }

    if let Some((prefix, raw)) = ctfe::split_around(str, b'#') {
        if ctfe::bytes_eq(prefix.as_bytes(), b"r#") {
            IdentParse::Raw(raw)
        } else {
            IdentParse::Fallback(str)
        }
    } else {
        IdentParse::Notraw(str)
    }
}

pub const fn parse_lit_regime(str: &'static str) -> u8 {
    lit::Regime::recognize(str) as u8
}

#[derive(Clone, Copy)]
struct TokenF<F>(F);

impl<E: From<Infallible> + Error, F: Fn(&mut TokenQueue) -> TtResult<(), E>> TryToTokens
    for TokenF<F>
{
    type Error = E;
    fn try_extend_tokens(&self, buf: &mut TokenQueue) -> TtResult<(), E> {
        (&self.0)(buf)
    }
}

pub fn make_fn<E: From<Infallible> + Error, F: Fn(&mut TokenQueue) -> TtResult<(), E>>(
    f: F,
) -> impl TryToTokens<Error = E> {
    TokenF(f)
}
