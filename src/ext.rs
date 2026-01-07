//! Extension traits for token trees and streams.

use proc_macro::TokenTree;

/// A trait extending the behaviour of [`TokenTree`]s.
pub trait TokenTreeExt {
    /// Returns whether the contained token is a [`Punct`] of the specified [`char`].
    ///
    /// [`Punct`]: proc_macro::Punct
    fn is_punct(&self, c: char) -> bool;
}

impl TokenTreeExt for TokenTree {
    #[inline]
    fn is_punct(&self, c: char) -> bool {
        match self {
            TokenTree::Punct(p) => p.as_char() == c,
            _ => false,
        }
    }
}
