use std::{convert::Infallible, fmt, num::NonZero};

use proc_macro::{Delimiter, Group, Ident, Literal, Punct, Span, TokenStream, TokenTree};

use crate::{TryIntoTokens, TtResult};

/// General purpose buffer for token composition.
///
/// # I Have No Mouth And I Must [`TokenStream`]
/// ***TLDR:*** `TokenQueue` is like the `Vec<SmallVec<[T; 1]>>` to [`TokenStream`]'s `Arc<Vec<T>>`.
///
/// [`TokenStream`] is well-suited for token storage.
/// Under the hood, it holds a clone-on-write handle to an underlying buffer
/// so it is cheap to share and traverse, but not cheap to modify.
///
/// The API surface of [`TokenStream`] is also rather constrained:
/// no storage preallocation, by-reference traversal, or
///
/// `TokenQueue`, by contrast, owns its contents and so is cheap to modify.
/// We can also preallocate the underlying buffer and so escape quadratic time complexity.
/// The read API is locked down such that
/// [conversions into `TokenStream`](TokenQueue#impl-From%3CTokenQueue%3E-for-TokenStream)
/// ("commits") happen all at once,
/// significantly reducing the overhead of dealing with the proc-macro server.
///
// / # By Analogy
// / Dealing with the proc-macro server feels a little like GPU programming at times.
// / Under this analogy, `TokenQueue` is our "command queue."
// / Writing to the queue is fast, and ["commits"](TokenQueue#impl-From%3CTokenQueue%3E-for-TokenStream) occur up to once,
// / all at once, reducing the "API cost" of sending messages to an from the server.
#[derive(Debug, Clone)]
pub struct TokenQueue {
    chunks: Vec<Chunk>,
    /// Index of the top of the group stack.
    group_stack_top_ptr: Option<usize>,
}

// NB: not the `Debug` impl we use for the queue, but useful in its own right.
#[derive(Debug, Clone)]
enum Chunk {
    PutGroup(Group),
    PutIdent(Ident),
    PutLiteral(Literal),
    PutPunct(Punct),
    Embed(TokenStream),
    OpenGroup(GroupHeader),
}

#[derive(Debug, Clone)]
struct GroupHeader {
    delim: Delimiter,
    /// The negative offset of the parent group within the chunk buffer.
    parent: Option<NonZero<usize>>,
}

impl GroupHeader {
    pub fn new(addr: usize, delim: Delimiter, parent: Option<usize>) -> GroupHeader {
        let parent = parent.map(|a| NonZero::new(addr - a).unwrap());
        GroupHeader { delim, parent }
    }

    pub fn parent(&self, addr: usize) -> Option<usize> {
        self.parent.map(|n| addr - n.get())
    }
}

impl From<TokenTree> for Chunk {
    fn from(tt: TokenTree) -> Chunk {
        match tt {
            TokenTree::Group(a) => Chunk::PutGroup(a),
            TokenTree::Ident(a) => Chunk::PutIdent(a),
            TokenTree::Punct(a) => Chunk::PutPunct(a),
            TokenTree::Literal(a) => Chunk::PutLiteral(a),
        }
    }
}

pub(crate) struct DisplayChunks<'a>(&'a [Chunk]);
impl<'a> fmt::Display for DisplayChunks<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fn delim_wings(delim: Delimiter) -> (&'static str, &'static str) {
            match delim {
                Delimiter::Parenthesis => ("(", ")"),
                Delimiter::Brace => ("{", "}"),
                Delimiter::Bracket => ("[", "]"),
                Delimiter::None => ("∅", "∅"),
            }
        }

        fn fmt_preamble(f: &mut fmt::Formatter<'_>, n: usize) -> fmt::Result {
            if f.alternate() {
                f.write_str("\n    ")?;
                for _ in 0..n {
                    f.write_str("    ")?;
                }
            } else {
                f.write_str(" ")?;
            }

            Ok(())
        }

        fn fmt_group<'a>(group: &Group, f: &mut fmt::Formatter<'_>, n: usize) -> fmt::Result {
            let (open, close) = delim_wings(group.delimiter());
            f.write_str(open)?;
            fmt_ts(group.stream().into_iter(), f, n + 1)?;
            f.write_str(close)
        }

        fn fmt_ts<'a>(
            ts: proc_macro::token_stream::IntoIter,
            f: &mut fmt::Formatter<'_>,
            n: usize,
        ) -> fmt::Result {
            for tt in ts {
                fmt_preamble(f, n)?;

                match tt {
                    TokenTree::Group(group) => fmt_group(&group, f, n)?,
                    TokenTree::Ident(id) => write!(f, "{id}")?,
                    TokenTree::Punct(p) => write!(f, "{p}")?,
                    TokenTree::Literal(lit) => write!(f, "{lit}")?,
                }
            }

            Ok(())
        }

        fn fmt_chunks<'a>(
            chunks: &'a [Chunk],
            f: &mut fmt::Formatter<'_>,
            mut n: usize,
        ) -> fmt::Result {
            let mut chunks = chunks.iter();
            while let Some(chunk) = chunks.next() {
                match chunk {
                    Chunk::Embed(ts) => fmt_ts(ts.clone().into_iter(), f, n)?,
                    Chunk::PutGroup(group) => fmt_group(group, f, n)?,
                    Chunk::PutIdent(id) => write!(f, "{id}")?,
                    Chunk::PutPunct(p) => write!(f, "{p}")?,
                    Chunk::PutLiteral(lit) => write!(f, "{lit}")?,
                    Chunk::OpenGroup(hdr) => {
                        let (open, _) = delim_wings(hdr.delim);
                        f.write_str(open)?;
                        n += 1;
                    }
                }
            }

            Ok(())
        }

        fmt_chunks(self.0, f, 0)
    }
}

macro_rules! impl_into_chunk_for_tt {
    ($($var:ident($ty:ty),)*) => {
        $(
            impl From<$ty> for Chunk {
                #[inline]
                fn from(tt: $ty) -> Self {
                    Chunk::$var(tt.into())
                }
            }
        )*
    };
}

impl_into_chunk_for_tt! {
    PutPunct(Punct),
    PutIdent(Ident),
    PutLiteral(Literal),
    PutGroup(Group),
}

impl From<TokenStream> for Chunk {
    #[inline]
    fn from(ts: TokenStream) -> Self {
        Chunk::Embed(ts)
    }
}

/// Invokes the given macro with all of the implementors of `Into<Chunk>`.
///
/// This is a kind of AOT monomorphisation, which is not done for performance,
/// but instead purely for the sake of privacy (we don't want to leak [`Chunk`], so we don't put it in trait bounds).
macro_rules! enumerate_into_chunk_implementors {
    ($macro:ident) => {
        $macro! { Chunk, TokenTree, TokenStream, Punct, Ident, Literal, Group }
    };
}

/// See [`TokenQueue::push`].
pub trait PushToken {
    fn push_to(self, q: &mut TokenQueue);
}

macro_rules! impl_pushtoken_for_into_chunk {
    ($($ty:ty),*) => {
        $(
            impl PushToken for $ty {
                #[inline]
                fn push_to(self, q: &mut TokenQueue) {
                    q.chunks.push(self.into());
                }
            }
        )*
    };
}

enumerate_into_chunk_implementors!(impl_pushtoken_for_into_chunk);

impl<T: PushToken> PushToken for Option<T> {
    fn push_to(self, q: &mut TokenQueue) {
        if let Some(this) = self {
            this.push_to(q);
        }
    }
}

impl Default for TokenQueue {
    fn default() -> Self {
        TokenQueue::new()
    }
}

// FIXME: fixup c'mon you're better than this.
const POP_NO_PUSH_MSG: &str = "found a pop with no push";
const PUSH_NO_POP_MSG: &str = "found a push with no pop";

impl TokenQueue {
    /// Creates an empty token queue.
    pub const fn new() -> TokenQueue {
        TokenQueue {
            chunks: Vec::new(),
            group_stack_top_ptr: None,
        }
    }

    /// Creates a token queue with space for at least `n` tokens.
    pub fn with_capacity(n: usize) -> TokenQueue {
        TokenQueue {
            chunks: Vec::with_capacity(n),
            group_stack_top_ptr: None,
        }
    }

    #[must_use]
    pub fn len(&self) -> usize {
        self.chunks.len()
    }

    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.chunks.is_empty()
    }

    #[must_use]
    pub fn token_size_hint(&self) -> (usize, Option<usize>) {
        token_size_hint_for_chunks(&self.chunks)
    }

    /// Reserves space for at least `n` additional token trees.
    pub fn reserve(&mut self, n: usize) {
        self.chunks.reserve(n);
    }

    /// Appends the given element to the queue.
    ///
    /// # Type Support
    /// The following types are currently supported.
    /// * [`TokenStream`]
    /// * [`TokenTree`]
    /// * [`Punct`]
    /// * [`Ident`]
    /// * [`Literal`]
    /// * [`Group`]
    pub fn push<T: PushToken>(&mut self, t: T) {
        t.push_to(self);
    }

    /// Extends an existing [`TokenStream`] with the contents of this queue.
    pub fn extend_stream(mut self, ts: &mut TokenStream) {
        let mut builder = TokenStreamBuilder::new(self.chunks.drain(..));
        builder.drain_to::<(), _>(ts);
    }

    /// Copies the contents of the given `TokenQueue`
    pub fn try_extend_from<T: TryIntoTokens>(&mut self, rhs: T) -> TtResult<(), T::Error> {
        // self.chunks.extend_from_slice(&rhs.chunks);
        // self.stack_depth += rhs.stack_depth;
        rhs.try_extend_tokens(self)
    }

    pub fn open_group(&mut self, delim: Delimiter) {
        let ptr = self.chunks.len();
        let group = GroupHeader::new(ptr, delim, self.group_stack_top_ptr);
        self.chunks.push(Chunk::OpenGroup(group));
        self.group_stack_top_ptr = Some(ptr);
    }

    #[must_use = "`close_group_with_span` returns a group, rather than enqueuing it."]
    pub fn close_group_with_span(&mut self, span: Option<Span>) -> Group {
        let Some(ptr) = self.group_stack_top_ptr else {
            panic!("{POP_NO_PUSH_MSG}")
        };

        let mut drain = self.chunks.drain(ptr..);
        let Some(Chunk::OpenGroup(hdr)) = drain.next() else {
            panic!("expected chunk at index {ptr} to be a `PushGroup`");
        };

        self.group_stack_top_ptr = hdr.parent(ptr);

        let ts = TokenStreamBuilder::new(drain).drain_to_token_stream();
        let mut group = Group::new(hdr.delim, ts);
        if let Some(span) = span {
            group.set_span(span);
        }
        group
    }

    #[must_use = "`close_group` returns a group, rather than enqueuing it."]
    pub fn close_group(&mut self) -> Group {
        self.close_group_with_span(None)
    }

    pub fn close_and_enqueue_group(&mut self) {
        let group = self.close_group();
        self.push(group);
    }

    pub fn display(&self) -> impl fmt::Display {
        DisplayChunks(&self.chunks)
    }
}

fn token_size_hint_for_chunks(chunks: &[Chunk]) -> (usize, Option<usize>) {
    let mut n = 0;
    let mut bounded_above = true;
    let mut chunks = chunks.iter();

    while let Some(chunk) = chunks.next() {
        match chunk {
            Chunk::Embed(_) => bounded_above = false,
            Chunk::PutGroup(_) | Chunk::PutIdent(_) | Chunk::PutLiteral(_) | Chunk::PutPunct(_) => {
                n += 1
            }
            // push group is just element shuffling, i.e. entirely immaterial.
            Chunk::OpenGroup(_) => (),
        }
    }

    (n, bounded_above.then_some(n))
}

macro_rules! impl_extend_for_into_chunk {
    ($($ty:ty),*) => {
        $(
            impl Extend<$ty> for TokenQueue {
                fn extend<T: IntoIterator<Item = $ty>>(&mut self, tcs: T) {
                    self.chunks.extend(tcs.into_iter().map(Into::<Chunk>::into));
                }
            }
        )*
    };
}

enumerate_into_chunk_implementors!(impl_extend_for_into_chunk);

impl From<TokenStream> for TokenQueue {
    fn from(ts: TokenStream) -> TokenQueue {
        TokenQueue {
            chunks: vec![Chunk::Embed(ts)],
            group_stack_top_ptr: None,
        }
    }
}

impl TryIntoTokens for TokenQueue {
    type Error = Infallible;

    fn try_extend_tokens(self, q: &mut TokenQueue) -> TtResult<()> {
        q.chunks.extend(self.chunks);
        Ok(())
    }

    fn try_into_tokens(self) -> TtResult<TokenQueue> {
        Ok(self)
    }

    fn queue_size_hint(&self) -> (usize, Option<usize>) {
        let n = self.len();
        (n, Some(n))
    }
}

/// "Commits" the token queue by value, constructing a [`TokenStream`] from the contents.
impl From<TokenQueue> for TokenStream {
    fn from(mut q: TokenQueue) -> TokenStream {
        let mut builder = TokenStreamBuilder::new(q.chunks.drain(..));
        let ts = builder.drain_to_token_stream();
        ts
    }
}

trait ChunkBuf: Iterator<Item = Chunk> {
    fn remaining(&self) -> &[Chunk];
    fn peek(&self) -> Option<&Chunk> {
        self.remaining().first()
    }
}

impl<'a> ChunkBuf for std::vec::Drain<'a, Chunk> {
    fn remaining(&self) -> &[Chunk] {
        self.as_slice()
    }
}

/// Parses a buffer of [`Chunk`]s until either EOS or a [`Pop`](Chunk::Pop).
struct TokenStreamBuilder<S> {
    ts_queue: Option<proc_macro::token_stream::IntoIter>,
    chunks: S,
}

impl<S: ChunkBuf> fmt::Debug for TokenStreamBuilder<S> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("TokenStreamBuilder")
            .field("ts_queue", &self.ts_queue.clone().map(Vec::from_iter))
            .field("chunks", &self.chunks.remaining())
            .finish()
    }
}

impl<S: ChunkBuf> TokenStreamBuilder<S> {
    fn new(chunks: S) -> TokenStreamBuilder<S> {
        TokenStreamBuilder {
            ts_queue: None,
            chunks,
        }
    }

    #[inline(never)]
    fn drain_to_token_stream(&mut self) -> TokenStream {
        self.drain_to(())
    }

    #[inline]
    fn as_unwrapped_streams(&mut self) -> Option<impl Iterator<Item = TokenStream> + '_> {
        let possible = self
            .chunks
            .remaining()
            .iter()
            .all(|tc| matches!(tc, Chunk::Embed(_)));

        if !possible {
            return None;
        }

        let ts = self.chunks.by_ref().filter_map(|chunk| match chunk {
            Chunk::Embed(ts) => Some(ts),
            _ => None,
        });

        Some(ts)
    }

    #[inline]
    fn take_as_single_stream(&mut self) -> Option<TokenStream> {
        let Some(Chunk::Embed(_)) = self.chunks.peek() else {
            return None;
        };

        let Chunk::Embed(ts) = self.chunks.next().unwrap() else {
            unreachable!();
        };

        Some(ts)
    }

    #[inline]
    fn drain_to<B: FromTokens<T>, T>(&mut self, t: T) -> B {
        if let Some(ts) = self.take_as_single_stream() {
            // if the buf contains just a single steam elem, use that directly.
            // this is useful for something like `quote! { #[@meta] }`.
            B::from_lone(t, ts)
        } else if let Some(tss) = self.as_unwrapped_streams() {
            // if the buf is all streams, get `proc_macro` to concat them directly, rather than copying ourselves.
            // this is useful for something like `quote! { @attrs @item @trait_impls }`.
            B::from_streams(t, tss)
        } else {
            B::from_tokens(t, self)
        }
    }
}

trait FromTokens<T> {
    fn from_lone(t: T, ts: TokenStream) -> Self;
    fn from_streams(t: T, tss: impl Iterator<Item = TokenStream>) -> Self;
    fn from_tokens(t: T, tts: impl Iterator<Item = TokenTree>) -> Self;
}

impl FromTokens<()> for TokenStream {
    fn from_lone(_: (), ts: TokenStream) -> TokenStream {
        ts
    }
    fn from_streams(_: (), tss: impl Iterator<Item = TokenStream>) -> TokenStream {
        tss.collect()
    }
    fn from_tokens(_: (), tts: impl Iterator<Item = TokenTree>) -> TokenStream {
        tts.collect()
    }
}

impl<'a> FromTokens<&'a mut TokenStream> for () {
    fn from_lone(s: &'a mut TokenStream, ts: TokenStream) {
        s.extend(Some(ts));
    }
    fn from_streams(s: &'a mut TokenStream, tss: impl Iterator<Item = TokenStream>) {
        s.extend(tss);
    }

    fn from_tokens(s: &'a mut TokenStream, tts: impl Iterator<Item = TokenTree>) {
        s.extend(tts);
    }
}

impl<S: ChunkBuf> Iterator for TokenStreamBuilder<S> {
    type Item = TokenTree;

    fn size_hint(&self) -> (usize, Option<usize>) {
        token_size_hint_for_chunks(self.chunks.remaining())
    }

    fn next(&mut self) -> Option<TokenTree> {
        if let Some(tt) = self.ts_queue.as_mut().and_then(|ts| ts.next()) {
            return Some(tt);
        }

        match self.chunks.next() {
            None => None,
            Some(Chunk::Embed(ts)) => {
                let ts_queue = ts.into_iter();
                self.ts_queue = Some(ts_queue);
                self.next()
            }
            Some(Chunk::PutGroup(tt)) => Some(tt.into()),
            Some(Chunk::PutIdent(tt)) => Some(tt.into()),
            Some(Chunk::PutPunct(tt)) => Some(tt.into()),
            Some(Chunk::PutLiteral(tt)) => Some(tt.into()),
            Some(Chunk::OpenGroup(_)) => panic!("{PUSH_NO_POP_MSG}"),
        }
    }
}
