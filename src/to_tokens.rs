use std::{convert::Infallible, error::Error, fmt};

use proc_macro::{Group, Ident, Literal, Punct, Spacing, TokenStream};

use crate::TokenQueue;

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
    ///
    /// Owing to the lazy design of [`TokenQueue`],
    /// this is the number of "commands" to push, rather than the raw token count,
    /// which is estimated by [`TokenQueue::token_size_hint`].
    fn queue_size_hint(&self) -> (usize, Option<usize>) {
        (0, None)
    }

    /// Extends an existing token buffer with the contents of a value.
    fn try_extend_tokens(&self, q: &mut TokenQueue) -> TtResult<(), Self::Error>;

    /// Builds a [`TokenStream`] from a value.
    #[inline]
    fn try_to_tokens(&self) -> TtResult<TokenQueue, Self::Error> {
        let mut q = TokenQueue::with_capacity(self.queue_size_hint().0);
        self.try_extend_tokens(&mut q)?;
        Ok(q)
    }
}

impl TryToTokens for Infallible {
    type Error = Infallible;

    fn try_extend_tokens(&self, _: &mut TokenQueue) -> TtResult<()> {
        match *self {}
    }
}

impl TryToTokens for TokenStream {
    type Error = Infallible;

    fn try_extend_tokens(&self, buf: &mut TokenQueue) -> TtResult<()> {
        buf.extend(self.clone());
        Ok(())
    }

    fn try_to_tokens(&self) -> TtResult<TokenQueue> {
        Ok(self.clone().into())
    }

    fn queue_size_hint(&self) -> (usize, Option<usize>) {
        (!self.is_empty() as usize, None)
    }
}

impl TryToTokens for TokenQueue {
    type Error = Infallible;

    fn try_extend_tokens(&self, q: &mut TokenQueue) -> TtResult<()> {
        q.concat(self);
        Ok(())
    }

    fn try_to_tokens(&self) -> TtResult<TokenQueue> {
        Ok(self.clone())
    }

    fn queue_size_hint(&self) -> (usize, Option<usize>) {
        let n = self.len();
        (n, Some(n))
    }
}

impl<T: TryToTokens + ?Sized> TryToTokens for &T {
    type Error = T::Error;

    #[inline]
    fn try_extend_tokens(&self, q: &mut TokenQueue) -> TtResult<(), Self::Error> {
        T::try_extend_tokens(self, q)
    }

    #[inline]
    fn try_to_tokens(&self) -> TtResult<TokenQueue, Self::Error> {
        T::try_to_tokens(self)
    }
}

impl<T: TryToTokens + ?Sized> TryToTokens for &mut T {
    type Error = T::Error;

    #[inline]
    fn try_extend_tokens(&self, q: &mut TokenQueue) -> TtResult<(), Self::Error> {
        T::try_extend_tokens(self, q)
    }

    #[inline]
    fn try_to_tokens(&self) -> TtResult<TokenQueue, Self::Error> {
        T::try_to_tokens(self)
    }
}

impl<T: TryToTokens> TryToTokens for [T] {
    type Error = T::Error;

    #[inline]
    fn try_extend_tokens(&self, q: &mut TokenQueue) -> TtResult<(), Self::Error> {
        for tt in self {
            tt.try_extend_tokens(q)?;
        }
        Ok(())
    }

    #[inline]
    fn try_to_tokens(&self) -> TtResult<TokenQueue, Self::Error> {
        if let [tt] = self {
            return tt.try_to_tokens();
        }

        let mut q = TokenQueue::new();
        q.reserve(self.queue_size_hint().0);
        for tt in self {
            tt.try_extend_tokens(&mut q)?;
        }

        Ok(q)
    }

    fn queue_size_hint(&self) -> (usize, Option<usize>) {
        self.iter()
            .map(T::queue_size_hint)
            .fold((0, Some(0)), |(min, max), (a, b)| {
                (min + a, max.and_then(|max| b.map(|b| max + b)))
            })
    }
}

impl<T: TryToTokens> TryToTokens for Option<T> {
    type Error = T::Error;

    fn try_extend_tokens(&self, q: &mut TokenQueue) -> TtResult<(), T::Error> {
        if let Some(this) = self {
            this.try_extend_tokens(q)?;
        }
        Ok(())
    }

    fn try_to_tokens(&self) -> TtResult<TokenQueue, Self::Error> {
        if let Some(this) = self {
            this.try_to_tokens()
        } else {
            Ok(TokenQueue::new())
        }
    }

    fn queue_size_hint(&self) -> (usize, Option<usize>) {
        self.as_ref().map_or((0, None), T::queue_size_hint)
    }
}

/// Evaluates to `@`. Useful for escaping.
///
/// See [`quote`](crate::quote!#escaping-) for use cases.
pub struct YouKnowWhatIMean;

impl TryToTokens for YouKnowWhatIMean {
    type Error = Infallible;

    fn try_extend_tokens(&self, q: &mut TokenQueue) -> TtResult<()> {
        q.push(Punct::new('@', Spacing::Alone));
        Ok(())
    }

    fn queue_size_hint(&self) -> (usize, Option<usize>) {
        (1, Some(1))
    }
}

macro_rules! impl_to_tokens_tt {
    ($($t:ty),*) => {
        $(
            impl TryToTokens for $t {
                type Error = Infallible;

                fn try_extend_tokens(&self, q: &mut TokenQueue) -> TtResult<()> {
                    q.push(self.clone());
                    Ok(())
                }

                fn queue_size_hint(&self) -> (usize, Option<usize>) {
                    (1, Some(1))
                }
            }
        )*
    };
}

impl_to_tokens_tt! { Punct, Ident, Group, Literal }
