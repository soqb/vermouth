use std::mem::take;

use proc_macro::{Delimiter, Group, Ident, Literal, Punct, Span, TokenStream, TokenTree};

#[derive(Clone)]
enum BufChunk {
    Embed(TokenStream),
    Unit(TokenTree),
    Push(Delimiter),
    Pop(Span),
}

impl BufChunk {
    fn size_hint(&self) -> (usize, Option<usize>) {
        match self {
            BufChunk::Embed(_) => (0, None),
            BufChunk::Unit(_) => (1, Some(1)),
            BufChunk::Push(..) => (0, Some(0)),
            BufChunk::Pop(..) => (0, Some(0)),
        }
    }

    fn is_empty(&self) -> bool {
        match self {
            BufChunk::Embed(ts) => ts.is_empty(),
            BufChunk::Unit(_) |
            // NB: if we encounter an open,
            // then we know we should emit a group,
            // and whether or not that group has any contents,
            // it still counts as a token tree,
            // so we can be sure the stream is non-empty.
            BufChunk::Push(..) |
            BufChunk::Pop(..) => false,
        }
    }
}

impl From<TokenTree> for BufChunk {
    fn from(tt: TokenTree) -> Self {
        BufChunk::Unit(tt)
    }
}

macro_rules! impl_into_bufchunk_for_tt {
    ($($ty:ty),*) => {
        $(
            impl From<$ty> for BufChunk {
                #[inline]
                fn from(tt: $ty) -> Self {
                    BufChunk::Unit(tt.into())
                }
            }
        )*
    };
}

impl_into_bufchunk_for_tt!(Punct, Ident, Literal, Group);

impl From<TokenStream> for BufChunk {
    #[inline]
    fn from(ts: TokenStream) -> Self {
        BufChunk::Embed(ts)
    }
}

macro_rules! enumerate_into_bufchunk_implementors {
    ($macro:ident) => {
        $macro! { BufChunk, TokenTree, TokenStream, Punct, Ident, Literal, Group }
    };
}

pub trait PushToken {
    fn push_to(self, buf: &mut TokenBuf);
}

macro_rules! impl_pushtoken_for_into_bufchunk {
    ($($ty:ty),*) => {
        $(
            impl PushToken for $ty {
                #[inline]
                fn push_to(self, buf: &mut TokenBuf) {
                    buf.chunks.push(self.into());
                }
            }
        )*
    };
}

enumerate_into_bufchunk_implementors!(impl_pushtoken_for_into_bufchunk);

#[derive(Default, Clone)]
pub struct TokenBuf {
    chunks: Vec<BufChunk>,
}

// FIXME: fixup c'mon you're better than this.
const POP_NO_PUSH_MSG: &str = "found a pop with no push";
const PUSH_NO_POP_MSG: &str = "found a push with no pop";

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
        self.chunks
            .iter()
            .map(BufChunk::size_hint)
            .fold((0, Some(0)), |(n, p), (m, q)| {
                (n + m, p.and_then(|p| q.map(|q| p + q)))
            })
    }

    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.chunks.iter().all(BufChunk::is_empty)
    }

    /// Reserves space for at least `n` token trees.
    pub fn reserve(&mut self, n: usize) {
        self.chunks.reserve(n);
    }

    pub fn push<T: PushToken>(&mut self, t: T) {
        t.push_to(self);
    }

    pub fn extend_stream(self, ts: &mut TokenStream) {
        let tc = &mut self.chunks.into_iter();
        match stream_chunks_as_tokens(tc, |iter| ts.extend(iter)) {
            (_, Some(_)) => panic!("{POP_NO_PUSH_MSG}"),
            (Ok(_), None) => (),
            (Err(ts2), None) => ts.extend(Some(ts2)),
        }
    }

    // FIXME: we want a single copy. this is not that but it is doable (hopefully with no unsafe !!).
    pub fn concat(&mut self, rhs: &TokenBuf) {
        self.chunks.extend(rhs.chunks.clone());
    }

    pub fn open_group(&mut self, delim: Delimiter) {
        self.chunks.push(BufChunk::Push(delim));
    }

    pub fn close_group_with_span(&mut self, span: Span) {
        self.chunks.push(BufChunk::Pop(span));
    }

    pub fn close_group(&mut self) {
        self.close_group_with_span(Span::call_site());
    }
}

macro_rules! impl_extend_for_into_bufchunk {
    ($($ty:ty),*) => {
        $(
            impl Extend<$ty> for TokenBuf {
                fn extend<T: IntoIterator<Item = $ty>>(&mut self, tcs: T) {
                    self.chunks.extend(tcs.into_iter().map(BufChunk::from));
                }
            }
        )*
    };
}

enumerate_into_bufchunk_implementors!(impl_extend_for_into_bufchunk);

impl From<TokenStream> for TokenBuf {
    fn from(ts: TokenStream) -> TokenBuf {
        TokenBuf {
            chunks: vec![BufChunk::Embed(ts)],
        }
    }
}

impl From<TokenBuf> for TokenStream {
    fn from(buf: TokenBuf) -> TokenStream {
        let (ts, span) = tokens_to_stream(&mut buf.chunks.into_iter());
        match span {
            Some(_) => panic!("{POP_NO_PUSH_MSG}"),
            None => ts,
        }
    }
}

#[inline]
fn stream_chunks_as_tokens<T>(
    chunks: &mut std::vec::IntoIter<BufChunk>,
    f: impl FnOnce(&mut TokenStreamBuilder<'_>) -> T,
) -> (Result<T, TokenStream>, Option<Span>) {
    if let Some((ts, span)) = take_as_single_stream(chunks.as_mut_slice()) {
        return (Err(ts), span);
    }

    let mut builder = TokenStreamBuilder {
        chunks,
        ts_queue: None,
        pop_span: None,
    };

    let t = f(&mut builder);

    (Ok(t), builder.pop_span)
}

#[inline(never)]
fn tokens_to_stream(chunks: &mut std::vec::IntoIter<BufChunk>) -> (TokenStream, Option<Span>) {
    let (r, span) = stream_chunks_as_tokens(chunks, |iter| iter.by_ref().collect());
    let ts = match r {
        Ok(ts) => ts,
        Err(ts) => ts,
    };

    (ts, span)
}

struct TokenStreamBuilder<'a> {
    ts_queue: Option<proc_macro::token_stream::IntoIter>,
    chunks: &'a mut std::vec::IntoIter<BufChunk>,
    pop_span: Option<Span>,
}

impl<'a> Iterator for TokenStreamBuilder<'a> {
    type Item = TokenTree;

    fn next(&mut self) -> Option<TokenTree> {
        if let Some(tt) = self.ts_queue.as_mut().and_then(|ts| ts.next()) {
            return Some(tt);
        }

        let Some(tc) = self.chunks.next() else {
            return None;
        };

        let tt = match tc {
            BufChunk::Embed(ts) => {
                let ts_queue = ts.into_iter();
                self.ts_queue = Some(ts_queue);
                return self.next();
            }
            BufChunk::Unit(tt) => tt,
            BufChunk::Push(delim) => {
                let (ts, span) = tokens_to_stream(self.chunks);

                let mut group = Group::new(delim, ts);
                match span {
                    Some(span) => group.set_span(span),
                    None => panic!("{PUSH_NO_POP_MSG}"),
                };
                group.into()
            }
            BufChunk::Pop(span) => {
                self.pop_span = Some(span);
                return None;
            }
        };

        Some(tt)
    }
}

fn take_as_single_stream(chunks: &mut [BufChunk]) -> Option<(TokenStream, Option<Span>)> {
    // accepts either [_] or [_, Pop, ..] since those are both "1-stream".
    // NB: [Push, .., Pop, ..] is kinda also 1-stream but we don't consider it to be
    // because branching there wouldn't be any faster.
    // FIXME: make less ugly
    if let Some((tc, chunks)) = chunks.split_first_mut() {
        let span = if let Some(tc2) = chunks.first() {
            match tc2 {
                &BufChunk::Pop(span) => Some(span),
                _ => return None,
            }
        } else {
            None
        };
        let ts = match tc {
            BufChunk::Embed(ts) => take(ts),
            BufChunk::Unit(tt) => tt.clone().into(),
            _ => return None,
        };

        Some((ts, span))
    } else {
        None
    }
}
