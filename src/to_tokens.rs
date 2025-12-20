use std::{convert::Infallible, error::Error, fmt};

use proc_macro::{Group, Ident, Literal, Punct, Spacing, TokenStream};

use crate::TokenBuf;

/// An error representing a failed reparse.
#[derive(Debug)]
pub struct ReparseError<'a> {
    source: proc_macro::LexError,
    kind: &'static str,
    text: &'a str,
}

impl<'a> ReparseError<'a> {
    pub fn from_ident(source: proc_macro::LexError, text: &'a str) -> ReparseError<'a> {
        ReparseError {
            source,
            text,
            kind: "identifier",
        }
    }
    pub fn from_lit(source: proc_macro::LexError, text: &'a str) -> ReparseError<'a> {
        ReparseError {
            source,
            text,
            kind: "literal",
        }
    }
}

impl<'a> From<Infallible> for ReparseError<'a> {
    fn from(value: Infallible) -> ReparseError<'a> {
        match value {}
    }
}

impl fmt::Display for ReparseError<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "failed to reparse {}: {:?}", self.kind, self.text)
    }
}

impl Error for ReparseError<'_> {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        Some(&self.source)
    }
}

#[non_exhaustive]
#[derive(Debug)]
pub enum TtError<R = Infallible> {
    Reparse(ReparseError<'static>),
    Misc(R),
}

pub type TtResult<T, E = Infallible> = core::result::Result<T, TtError<E>>;

impl<R> From<Infallible> for TtError<R> {
    fn from(value: Infallible) -> TtError<R> {
        match value {}
    }
}

impl<R> From<ReparseError<'static>> for TtError<R> {
    fn from(value: ReparseError<'static>) -> Self {
        TtError::Reparse(value)
    }
}

impl<R: fmt::Display> fmt::Display for TtError<R> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TtError::Reparse(rp) => fmt::Display::fmt(rp, f),
            TtError::Misc(m) => fmt::Display::fmt(m, f),
        }
    }
}

impl<R: Error + 'static> Error for TtError<R> {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            TtError::Reparse(rp) => Some(rp),
            TtError::Misc(m) => Some(m),
        }
    }
}

/// Methods for converting values into [`TokenStream`]s.
pub trait TryToTokens {
    /// The error type of the to-token conversion.
    ///
    /// Always used in the form [`TtError<Self::Error>`](TtError).
    type Error: From<Infallible> + Error;

    /// Analagous to [`Iterator::size_hint`].
    fn token_size_hint(&self) -> (usize, Option<usize>) {
        (0, None)
    }

    /// Extends an existing token buffer with the contents of a value.
    fn try_extend_tokens(&self, buf: &mut TokenBuf) -> TtResult<(), Self::Error>;

    /// Builds a [`TokenStream`] from a value.
    #[inline]
    fn try_to_tokens(&self) -> TtResult<TokenBuf, Self::Error> {
        let mut buf = TokenBuf::new();
        buf.reserve(self.token_size_hint().0);
        self.try_extend_tokens(&mut buf)?;
        Ok(buf)
    }
}

impl TryToTokens for Infallible {
    type Error = Infallible;

    fn try_extend_tokens(&self, _: &mut TokenBuf) -> TtResult<()> {
        match *self {}
    }
}

impl TryToTokens for TokenStream {
    type Error = Infallible;

    fn try_extend_tokens(&self, buf: &mut TokenBuf) -> TtResult<()> {
        buf.extend(self.clone());
        Ok(())
    }

    fn try_to_tokens(&self) -> TtResult<TokenBuf> {
        Ok(self.clone().into())
    }

    fn token_size_hint(&self) -> (usize, Option<usize>) {
        (!self.is_empty() as usize, None)
    }
}

impl TryToTokens for TokenBuf {
    type Error = Infallible;

    fn try_extend_tokens(&self, buf: &mut TokenBuf) -> TtResult<()> {
        buf.concat(self);
        Ok(())
    }

    fn try_to_tokens(&self) -> TtResult<TokenBuf> {
        Ok(self.clone())
    }

    fn token_size_hint(&self) -> (usize, Option<usize>) {
        self.size_hint()
    }
}

impl<T: TryToTokens + ?Sized> TryToTokens for &T {
    type Error = T::Error;

    #[inline]
    fn try_extend_tokens(&self, buf: &mut TokenBuf) -> TtResult<(), Self::Error> {
        T::try_extend_tokens(self, buf)
    }

    #[inline]
    fn try_to_tokens(&self) -> TtResult<TokenBuf, Self::Error> {
        T::try_to_tokens(self)
    }
}

impl<T: TryToTokens + ?Sized> TryToTokens for &mut T {
    type Error = T::Error;

    #[inline]
    fn try_extend_tokens(&self, buf: &mut TokenBuf) -> TtResult<(), Self::Error> {
        T::try_extend_tokens(self, buf)
    }

    #[inline]
    fn try_to_tokens(&self) -> TtResult<TokenBuf, Self::Error> {
        T::try_to_tokens(self)
    }
}

impl<T: TryToTokens> TryToTokens for [T] {
    type Error = T::Error;

    #[inline]
    fn try_extend_tokens(&self, buf: &mut TokenBuf) -> TtResult<(), Self::Error> {
        for tt in self {
            tt.try_extend_tokens(buf)?;
        }
        Ok(())
    }

    #[inline]
    fn try_to_tokens(&self) -> TtResult<TokenBuf, Self::Error> {
        if let [tt] = self {
            return tt.try_to_tokens();
        }

        let mut buf = TokenBuf::new();
        buf.reserve(self.iter().map(|tt| tt.token_size_hint().0).sum());
        for tt in self {
            tt.try_extend_tokens(&mut buf)?;
        }

        Ok(buf)
    }
}

impl<T: TryToTokens> TryToTokens for Option<T> {
    type Error = T::Error;

    fn try_extend_tokens(&self, buf: &mut TokenBuf) -> TtResult<(), T::Error> {
        if let Some(this) = self {
            this.try_extend_tokens(buf)?;
        }
        Ok(())
    }

    fn try_to_tokens(&self) -> TtResult<TokenBuf, Self::Error> {
        if let Some(this) = self {
            this.try_to_tokens()
        } else {
            Ok(TokenBuf::new())
        }
    }
}

/// Evaluates to `@`. Useful for escaping.
///
/// See [`quote`](crate::quote#escaping-) for use cases.
pub struct YouKnowWhatIMean;

impl TryToTokens for YouKnowWhatIMean {
    type Error = Infallible;

    fn try_extend_tokens(&self, buf: &mut TokenBuf) -> TtResult<()> {
        buf.push(Punct::new('@', Spacing::Alone));
        Ok(())
    }
}

macro_rules! impl_to_tokens_tt {
    ($($t:ty),*) => {
        $(
            impl TryToTokens for $t {
                type Error = Infallible;

                fn try_extend_tokens(&self, buf: &mut TokenBuf) -> TtResult<()> {
                    buf.push(self.clone());
                    Ok(())
                }
            }
        )*
    };
}

impl_to_tokens_tt!(Punct, Ident, Group, Literal);
