use proc_macro::{Group, Ident, Literal, Punct, TokenStream, TokenTree};

#[derive(Clone)]
enum BufStorage {
    Tokens(Vec<TokenTree>),
    Stream(TokenStream),
}

impl Default for BufStorage {
    fn default() -> BufStorage {
        BufStorage::Tokens(Vec::new())
    }
}

#[derive(Default, Clone)]
pub struct TokenBuf {
    inner: BufStorage,
}

impl TokenBuf {
    pub fn new() -> TokenBuf {
        TokenBuf::default()
    }

    pub fn with_capacity(n: usize) -> TokenBuf {
        let mut buf = TokenBuf::new();
        buf.reserve(n);
        buf
    }

    #[must_use]
    pub fn size_hint(&self) -> (usize, Option<usize>) {
        match &self.inner {
            BufStorage::Tokens(tts) => (tts.len(), Some(tts.len())),
            BufStorage::Stream(s) => (!s.is_empty() as usize, None),
        }
    }

    #[must_use]
    pub fn is_empty(&self) -> bool {
        match &self.inner {
            BufStorage::Tokens(tts) => tts.is_empty(),
            BufStorage::Stream(ts) => ts.is_empty(),
        }
    }

    pub fn reserve(&mut self, n: usize) {
        let tokens = self.upgrade();
        tokens.reserve(n);
    }

    fn upgrade(&mut self) -> &mut Vec<TokenTree> {
        match &mut self.inner {
            BufStorage::Tokens(tts) => tts,
            BufStorage::Stream(_) => {
                todo!()
            }
        }
    }

    pub fn push<T>(&mut self, t: T)
    where
        TokenBuf: Extend<T>,
    {
        self.extend(Some(t));
    }
}

impl Extend<TokenTree> for TokenBuf {
    fn extend<T: IntoIterator<Item = TokenTree>>(&mut self, iter: T) {
        let tokens = self.upgrade();
        tokens.extend(iter);
    }
}

macro_rules! impl_extend_tokenbuf_tt {
    ($($ty:ty),*) => {
        $(
            impl Extend<$ty> for TokenBuf {
                fn extend<T: IntoIterator<Item = $ty>>(&mut self, iter: T) {
                    let tokens = self.upgrade();
                    tokens.extend(iter.into_iter().map(Into::into));
                }
            }
        )*
    };
}

impl_extend_tokenbuf_tt!(Punct, Ident, Group, Literal);

impl IntoIterator for TokenBuf {
    type Item = TokenTree;
    type IntoIter = std::vec::IntoIter<TokenTree>;

    fn into_iter(self) -> Self::IntoIter {
        todo!()
    }
}

impl<'a> IntoIterator for &'a TokenBuf {
    type Item = &'a TokenTree;
    type IntoIter = std::slice::Iter<'a, TokenTree>;

    fn into_iter(self) -> Self::IntoIter {
        todo!()
    }
}

impl FromIterator<TokenTree> for TokenBuf {
    fn from_iter<T: IntoIterator<Item = TokenTree>>(iter: T) -> Self {
        TokenBuf {
            inner: BufStorage::Tokens(Vec::from_iter(iter)),
        }
    }
}

impl From<TokenStream> for TokenBuf {
    fn from(value: TokenStream) -> TokenBuf {
        TokenBuf {
            inner: BufStorage::Stream(value),
        }
    }
}

impl From<TokenBuf> for TokenStream {
    fn from(value: TokenBuf) -> Self {
        match value.inner {
            BufStorage::Tokens(tts) => TokenStream::from_iter(tts),
            BufStorage::Stream(ts) => ts,
        }
    }
}
