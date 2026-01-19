use std::{fmt, panic::Location, str::FromStr};

use proc_macro::{Span, TokenStream};

use crate::{IntoTokens, PushToken, TokenQueue};

/// A verbatim token. See [the `verbatim` macro](verbatim).
#[derive(Debug, Clone, Copy)]
pub struct Verbatim {
    text: &'static str,
    kind: VerbatimKind,
    location: &'static Location<'static>,
    span: Option<Span>,
}

/// What sort of token this [`Verbatim`] represents.
#[derive(Debug, Clone, Copy)]
pub enum VerbatimKind {
    /// An identifier.
    Ident,
    /// A literal.
    Literal,
    /// A lifetime.
    Lifetime,
}

impl fmt::Display for VerbatimKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            VerbatimKind::Ident => "identifier",
            VerbatimKind::Literal => "literal",
            VerbatimKind::Lifetime => "lifetime",
        };
        f.write_str(s)
    }
}

impl Verbatim {
    #[doc(hidden)]
    #[track_caller]
    pub const fn new(text: &'static str, kind: VerbatimKind) -> Verbatim {
        Verbatim {
            text,
            kind,
            location: Location::caller(),
            span: None,
        }
    }

    /// See [`Transcriber::with_span`].
    pub const fn with_span(mut self, span: Span) -> Verbatim {
        self.span = Some(span);
        self
    }
}

impl IntoTokens for Verbatim {
    fn extend_tokens(self, q: &mut TokenQueue) {
        let Verbatim {
            text,
            kind,
            location,
            span,
        } = self;
        let ts = TokenStream::from_str(text).unwrap_or_else(move |lex| {
            panic!("failed to reparse {kind} {text:?} {location}: {lex}")
        });

        match span {
            None => q.push(ts),
            Some(span) => {
                // NB: may very well be more than one token (e.g. lifetimes, negative numerics)
                //   and the max is entirely unspecified.
                for mut tt in ts {
                    tt.set_span(span);
                    q.push(tt)
                }
            }
        };
    }
}

impl PushToken for Verbatim {}
