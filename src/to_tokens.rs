//! The [`ToTokens`] and [`IntoTokens`] traits for idiomatic tokenization.

use std::convert::Infallible;

use proc_macro::TokenStream;

use crate::TokenQueue;

/// Methods for converting by-value into [`TokenQueue`].
///
/// See also [`IntoTokens`], the analagous by-reference trait.
pub trait IntoTokens: Sized {
    /// Analagous to [`Iterator::size_hint`].
    ///
    /// Owing to the lazy design of [`TokenQueue`],
    /// this is the number of "commands" to push, rather than the raw token count,
    /// which is estimated by [`TokenQueue::token_size_hint`].
    fn queue_size_hint(&self) -> (usize, Option<usize>) {
        (0, None)
    }

    /// Extends an existing token buffer with the contents of a value.
    fn extend_tokens(self, q: &mut TokenQueue);

    /// Converts into a [`TokenQueue`] from a value.
    #[inline]
    fn into_tokens(self) -> TokenQueue {
        let mut q = TokenQueue::with_capacity(self.queue_size_hint().0);
        self.extend_tokens(&mut q);
        q
    }
}

impl<T: IntoTokens + Clone> IntoTokens for &T {
    #[inline]
    fn queue_size_hint(&self) -> (usize, Option<usize>) {
        (*self).queue_size_hint()
    }

    #[inline]
    fn extend_tokens(self, q: &mut TokenQueue) {
        self.clone().extend_tokens(q);
    }

    #[inline]
    fn into_tokens(self) -> TokenQueue {
        self.clone().into_tokens()
    }
}

impl IntoTokens for () {
    fn extend_tokens(self, _: &mut TokenQueue) {}

    fn queue_size_hint(&self) -> (usize, Option<usize>) {
        (0, Some(0))
    }
}

impl IntoTokens for Infallible {
    fn extend_tokens(self, _: &mut TokenQueue) {
        match self {}
    }
}

impl IntoTokens for TokenStream {
    fn extend_tokens(self, buf: &mut TokenQueue) {
        buf.push(self);
    }

    fn into_tokens(self) -> TokenQueue {
        self.into()
    }

    fn queue_size_hint(&self) -> (usize, Option<usize>) {
        (!self.is_empty() as usize, None)
    }
}

impl<T: IntoTokens> IntoTokens for Option<T> {
    fn extend_tokens(self, q: &mut TokenQueue) {
        if let Some(this) = self {
            this.extend_tokens(q);
        }
    }

    fn into_tokens(self) -> TokenQueue {
        if let Some(this) = self {
            this.into_tokens()
        } else {
            TokenQueue::new()
        }
    }

    fn queue_size_hint(&self) -> (usize, Option<usize>) {
        self.as_ref().map_or((0, None), T::queue_size_hint)
    }
}
