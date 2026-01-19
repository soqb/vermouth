//! The domain-specific library for this crate's macros (especially [`quote`](crate::quote)).

use proc_macro::{Delimiter, Group, Ident, Punct, Spacing, Span, TokenStream};

use crate::{IntoTokens, PushToken, TokenQueue, Verbatim, VerbatimKind, ctfe};

pub use core;
pub use proc_macro;

mod lit;
mod spec;

pub use spec::*;

pub fn assert_tokens_extend(_: &mut TokenStream) {}

// #[inline]
// pub fn err<T, A, B: From<A>>(e: TtError<A>) -> TtResult<T, B> {
//     Err(e.into())
// }

// #[inline]
// pub fn ok<T, E>(x: T) -> TtResult<T, E> {
//     Ok(x)
// }

pub fn push_underscore(q: &mut TokenQueue) {
    q.push(Ident::new("_", Span::call_site()));
}

pub fn push_empty_group(q: &mut TokenQueue, delim: Delimiter) {
    q.push(Group::new(delim, TokenStream::new()));
}

#[inline]
pub fn push_punct(q: &mut TokenQueue, chars: &[char]) {
    let Some((&last, rest)) = chars.split_last() else {
        return;
    };

    for &c in rest {
        q.push(Punct::new(c, Spacing::Joint));
    }

    q.push(Punct::new(last, Spacing::Alone));
}

#[track_caller]
pub const fn parse_ident(s: &'static str) -> impl PushToken {
    parse_ident_like(VerbatimKind::Ident, s)
}

#[track_caller]
pub const fn parse_lifetime(s: &'static str) -> impl PushToken {
    #[derive(Clone, Copy)]
    struct Lifetime<T>(T);

    impl<T: PushToken> IntoTokens for Lifetime<T> {
        fn extend_tokens(self, q: &mut TokenQueue) {
            q.push(Punct::new('\'', Spacing::Joint));
            q.push(self.0);
        }
    }

    impl<T: PushToken> PushToken for Lifetime<T> {}

    // NB: no assert_eq bc const.
    let (f, s) = s.split_at(1);
    assert!(matches!(f.as_bytes(), b"\'"));
    Lifetime(parse_ident_like(VerbatimKind::Lifetime, s))
}

#[track_caller]
const fn parse_ident_like(kind: VerbatimKind, s: &'static str) -> impl PushToken {
    #[derive(Clone, Copy)]
    enum IdentParse {
        Raw(&'static str),
        Notraw(&'static str),
        Fallback(Verbatim),
    }

    impl IntoTokens for IdentParse {
        #[inline]
        fn extend_tokens(self, q: &mut TokenQueue) {
            match self {
                IdentParse::Raw(s) => q.push(Ident::new_raw(s, Span::call_site())),
                IdentParse::Notraw(s) => q.push(Ident::new(s, Span::call_site())),
                IdentParse::Fallback(v) => q.push(v),
            }
        }
    }
    impl PushToken for IdentParse {}

    if ctfe::bytes_any(s.as_bytes(), b'#') {
        if let Some((prefix, raw)) = s.split_at_checked(2)
            && let b"r#" = prefix.as_bytes()
        {
            IdentParse::Raw(raw)
        } else {
            IdentParse::Fallback(Verbatim::new(s, kind))
        }
    } else {
        IdentParse::Notraw(s)
    }
}

pub const fn parse_lit_regime(str: &'static str) -> u8 {
    lit::Regime::recognize(str) as u8
}
