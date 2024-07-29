use proc_macro::{TokenStream, TokenTree};

/// An extension trait for manually building [`TokenStream`]s more ergonomically.
pub trait TokensExtend {
    /// Pushes a single token into a stream.
    fn push(&mut self, tok: impl Into<TokenTree>);
}

impl TokensExtend for TokenStream {
    #[inline]
    fn push(&mut self, tok: impl Into<TokenTree>) {
        self.extend(Some(tok.into()))
    }
}

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

/// Methods for converting values into [`TokenStream`]s.
pub trait ToTokens {
    /// Extends an existing [`TokenStream`] with the contents of a value.
    fn extend_tokens(&self, buf: &mut TokenStream);
    /// Builds a [`TokenStream`] from a value.
    #[inline]
    fn to_tokens(&self) -> TokenStream {
        let mut buf = TokenStream::new();
        self.extend_tokens(&mut buf);
        buf
    }
}

impl ToTokens for TokenStream {
    #[inline]
    fn to_tokens(&self) -> TokenStream {
        self.clone()
    }

    #[inline]
    fn extend_tokens(&self, buf: &mut TokenStream) {
        buf.extend(self.clone())
    }
}
