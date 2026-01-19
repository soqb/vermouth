//! The [`IntoTokens`] traits for idiomatic tokenization.

use std::convert::Infallible;

use proc_macro::TokenStream;

use crate::TokenQueue;

/// Methods for converting by-value into [`TokenQueue`].
///
/// See also [`RefToTokens`], the analagous by-reference trait.
pub trait IntoTokens: Sized {
    /// Analagous to [`Iterator::size_hint`].
    ///
    /// # Interpretation
    ///
    /// Owing to the lazy design of [`TokenQueue`],
    /// this is the number of "commands" to push, rather than the raw token count,
    /// which is estimated by [`TokenQueue::token_size_hint`].
    ///
    /// Exactly what should be counted here is considered an implementation detail
    /// and therefore not stable, as it depends on some open performance-related questions,
    /// but currently:
    /// * [Pushing](TokenQueue::push) anything is exactly one command.
    fn queue_size_hint(&self) -> (usize, Option<usize>) {
        (0, None)
    }

    /// Extends an existing token queue by value.
    fn extend_tokens(self, q: &mut TokenQueue);

    /// Converts by-value to a [`TokenQueue`].
    #[inline]
    fn into_tokens(self) -> TokenQueue {
        let mut q = TokenQueue::with_capacity(self.queue_size_hint().0);
        self.extend_tokens(&mut q);
        q
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

/// Marks a type as cheaply-clonable and suitable for
/// [an automatic `&T: IntoTokens` implementation](IntoTokens#impl-IntoTokens-for-%26T).
// FIXME: name?
pub trait RefToTokens: IntoTokens + Clone {}

/// See [`RefToTokens`].
impl<T: RefToTokens> IntoTokens for &T {
    fn extend_tokens(self, q: &mut TokenQueue) {
        self.clone().extend_tokens(q)
    }

    fn queue_size_hint(&self) -> (usize, Option<usize>) {
        (*self).queue_size_hint()
    }

    fn into_tokens(self) -> TokenQueue {
        self.clone().into_tokens()
    }
}

impl<T: RefToTokens> RefToTokens for &T {}
impl RefToTokens for TokenStream {}
