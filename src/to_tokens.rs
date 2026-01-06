use std::{convert::Infallible, error::Error, fmt};

use proc_macro::{Group, Ident, Literal, Punct, TokenStream};

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

/// Methods for converting by-value into [`TokenQueue`].
///
/// See also [`TryToTokens`], the analagous by-reference trait.
pub trait TryIntoTokens: Sized {
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
    fn try_extend_tokens(self, q: &mut TokenQueue) -> TtResult<(), Self::Error>;

    /// Converts into a [`TokenQueue`] from a value.
    #[inline]
    fn try_into_tokens(self) -> TtResult<TokenQueue, Self::Error> {
        let mut q = TokenQueue::with_capacity(self.queue_size_hint().0);
        self.try_extend_tokens(&mut q)?;
        Ok(q)
    }
}

/// Methods for converting by-reference into [`TokenQueue`].
///
/// See [`TryIntoTokens`] (the analagous by-value trait) for more.
pub trait TryToTokens {
    /// The error type of the to-token conversion.
    type Error: From<Infallible> + Error;

    /// Analagous to [`Iterator::size_hint`].
    fn queue_size_hint_ref(&self) -> (usize, Option<usize>) {
        (0, None)
    }

    /// Extends an existing token buffer, copying the contents of a value.
    fn try_extend_tokens_ref(&self, q: &mut TokenQueue) -> TtResult<(), Self::Error>;

    /// Converts into a [`TokenQueue`] from a reference to a value.
    fn try_to_tokens(&self) -> TtResult<TokenQueue, Self::Error> {
        let mut q = TokenQueue::with_capacity(self.queue_size_hint_ref().0);
        self.try_extend_tokens_ref(&mut q)?;
        Ok(q)
    }
}

impl<T: TryToTokens> TryIntoTokens for &T {
    type Error = T::Error;

    fn try_extend_tokens(self, q: &mut TokenQueue) -> TtResult<(), T::Error> {
        (*self).try_extend_tokens_ref(q)
    }

    fn queue_size_hint(&self) -> (usize, Option<usize>) {
        (*self).queue_size_hint_ref()
    }

    fn try_into_tokens(self) -> TtResult<TokenQueue, T::Error> {
        let mut q = TokenQueue::with_capacity(self.queue_size_hint_ref().0);
        self.try_extend_tokens(&mut q)?;
        Ok(q)
    }
}

impl<T: TryToTokens> TryToTokens for &T {
    type Error = T::Error;

    #[inline]
    fn try_extend_tokens_ref(&self, q: &mut TokenQueue) -> TtResult<(), T::Error> {
        (**self).try_extend_tokens_ref(q)
    }

    #[inline]
    fn queue_size_hint_ref(&self) -> (usize, Option<usize>) {
        (**self).queue_size_hint_ref()
    }

    #[inline]
    fn try_to_tokens(&self) -> TtResult<TokenQueue, Self::Error> {
        (**self).try_to_tokens()
    }
}

impl<T: TryToTokens> TryToTokens for &mut T {
    type Error = T::Error;

    #[inline]
    fn try_extend_tokens_ref(&self, q: &mut TokenQueue) -> TtResult<(), T::Error> {
        (**self).try_extend_tokens_ref(q)
    }

    #[inline]
    fn queue_size_hint_ref(&self) -> (usize, Option<usize>) {
        (**self).queue_size_hint_ref()
    }

    #[inline]
    fn try_to_tokens(&self) -> TtResult<TokenQueue, Self::Error> {
        (**self).try_to_tokens()
    }
}

impl TryIntoTokens for () {
    type Error = Infallible;

    fn try_extend_tokens(self, _: &mut TokenQueue) -> TtResult<()> {
        Ok(())
    }

    fn queue_size_hint(&self) -> (usize, Option<usize>) {
        (0, Some(0))
    }
}

impl TryIntoTokens for Infallible {
    type Error = Infallible;

    fn try_extend_tokens(self, _: &mut TokenQueue) -> TtResult<()> {
        match self {}
    }
}

impl TryIntoTokens for TokenStream {
    type Error = Infallible;

    fn try_extend_tokens(self, buf: &mut TokenQueue) -> TtResult<()> {
        buf.push(self);
        Ok(())
    }

    fn try_into_tokens(self) -> TtResult<TokenQueue> {
        Ok(self.into())
    }

    fn queue_size_hint(&self) -> (usize, Option<usize>) {
        (!self.is_empty() as usize, None)
    }
}

impl TryToTokens for TokenStream {
    type Error = Infallible;

    fn try_extend_tokens_ref(&self, buf: &mut TokenQueue) -> TtResult<()> {
        buf.push(self.clone());
        Ok(())
    }

    fn try_to_tokens(&self) -> TtResult<TokenQueue> {
        Ok(self.clone().into())
    }

    fn queue_size_hint_ref(&self) -> (usize, Option<usize>) {
        (!self.is_empty() as usize, None)
    }
}

impl<T: TryIntoTokens> TryIntoTokens for Option<T> {
    type Error = T::Error;

    fn try_extend_tokens(self, q: &mut TokenQueue) -> TtResult<(), T::Error> {
        if let Some(this) = self {
            this.try_extend_tokens(q)?;
        }
        Ok(())
    }

    fn try_into_tokens(self) -> TtResult<TokenQueue, Self::Error> {
        if let Some(this) = self {
            this.try_into_tokens()
        } else {
            Ok(TokenQueue::new())
        }
    }

    fn queue_size_hint(&self) -> (usize, Option<usize>) {
        self.as_ref().map_or((0, None), T::queue_size_hint)
    }
}

fn fold_size_hint(s: impl Iterator<Item = (usize, Option<usize>)>) -> (usize, Option<usize>) {
    s.reduce(|(a, b), (c, d)| (a + c, b.and_then(|b| d.map(|d| b + d))))
        .unwrap_or_else(|| (0, Some(0)))
}

macro_rules! impl_to_tokens_tt {
    ($($t:ty),*) => {
        $(
            impl TryIntoTokens for $t {
                type Error = Infallible;

                fn try_extend_tokens(self, q: &mut TokenQueue) -> TtResult<()> {
                    q.push(self);
                    Ok(())
                }

                fn queue_size_hint(&self) -> (usize, Option<usize>) {
                    (1, Some(1))
                }
            }

            impl TryToTokens for $t {
                type Error = Infallible;

                fn try_extend_tokens_ref(&self, q: &mut TokenQueue) -> TtResult<()> {
                    q.push(self.clone());
                    Ok(())
                }

                fn queue_size_hint_ref(&self) -> (usize, Option<usize>) {
                    (1, Some(1))
                }
            }
        )*
    };
}

impl_to_tokens_tt! { Punct, Ident, Group, Literal }
