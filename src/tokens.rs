use proc_macro::{
    TokenStream,
    TokenTree::{self, Punct},
};

use crate::{Error, Result, Spanned};

pub trait TokensExtend {
    fn push(&mut self, tok: impl Into<TokenTree>);
}

impl TokensExtend for TokenStream {
    fn push(&mut self, tok: impl Into<TokenTree>) {
        self.extend(Some(tok.into()))
    }
}

pub trait TokenTreeExt {
    fn is_punct(&self, c: char) -> bool;
}

impl TokenTreeExt for TokenTree {
    fn is_punct(&self, c: char) -> bool {
        match self {
            Punct(p) => p.as_char() == c,
            _ => false,
        }
    }
}

pub fn expect_str(id: Spanned<&str>, p: &'static str) -> Result<()> {
    (&**id == p)
        .then_some(())
        .ok_or_else(|| Error::from_expected_noun(&id, p))
}

pub trait ToTokens {
    fn extend_tokens(&self, buf: &mut TokenStream);
    fn to_tokens(&self) -> TokenStream {
        let mut buf = TokenStream::new();
        self.extend_tokens(&mut buf);
        buf
    }
}

impl ToTokens for TokenStream {
    fn to_tokens(&self) -> TokenStream {
        self.clone()
    }

    fn extend_tokens(&self, buf: &mut TokenStream) {
        buf.extend(self.clone())
    }
}
