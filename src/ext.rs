use proc_macro::{Group, Ident, Literal, Punct, TokenStream, TokenTree};

/// An extension trait for manually building [`TokenStream`]s more ergonomically.
pub trait TokensExtend: Extend<TokenTree> {
    /// Pushes a single token into a stream.
    #[inline]
    fn push(&mut self, tok: impl Into<TokenTree>) {
        self.extend(Some(tok.into()))
    }
}

impl TokensExtend for TokenStream {}

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
    /// Extends an existing token buffer with the contents of a value.
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

impl<T: ToTokens> ToTokens for &T {
    fn extend_tokens(&self, buf: &mut TokenStream) {
        T::extend_tokens(self, buf)
    }
}

impl<T: ToTokens> ToTokens for Option<T> {
    fn extend_tokens(&self, buf: &mut TokenStream) {
        if let Some(this) = self {
            this.extend_tokens(buf);
        }
    }
}

macro_rules! impl_to_tokens_tt {
    ($($t:ty),*) => {
        $(
            impl ToTokens for $t {
                fn extend_tokens(&self, buf: &mut TokenStream) {
                    buf.push(self.clone());
                }
            }
        )*
    };
}

impl_to_tokens_tt!(Punct, Ident, Group, Literal);
