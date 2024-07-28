use proc_macro::{
    TokenStream,
    TokenTree::{self, Punct},
};

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

pub trait ToTokens {
    fn extend_tokens(&self, buf: &mut TokenStream);
    fn into_tokens(&self) -> TokenStream {
        let mut buf = TokenStream::new();
        self.extend_tokens(&mut buf);
        buf
    }
}

impl ToTokens for TokenStream {
    fn into_tokens(&self) -> TokenStream {
        self.clone()
    }

    fn extend_tokens(&self, buf: &mut TokenStream) {
        buf.extend(self.clone())
    }
}
