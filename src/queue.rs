use std::fmt;

use proc_macro::{Delimiter, Group, Ident, Literal, Punct, Spacing, Span, TokenStream, TokenTree};

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
#[derive(Clone)]
pub struct TokenQueue {
    chunks: Vec<Chunk>,
    stack_depth: usize,
}

// NB: not the `Debug` impl we use for the queue, but useful in its own right.
#[derive(Debug, Clone)]
enum Chunk {
    Embed(TokenStream),
    Unit(TokenTree),
    Push(Delimiter),
    Pop(Option<Span>),
}

impl fmt::Debug for TokenQueue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("TokenQueue")
            .field("chunks", &FmtChunks(&self.chunks))
            .field("stack_depth", &self.stack_depth)
            .finish()
    }
}
impl From<TokenTree> for Chunk {
    fn from(tt: TokenTree) -> Self {
        Chunk::Unit(tt)
    }
}

pub(crate) struct FmtChunks<'a>(&'a [Chunk]);
impl<'a> fmt::Debug for FmtChunks<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fn fmt_chunks_until_pop<'a>(
            chunks: &'a [Chunk],
            f: &mut fmt::Formatter<'_>,
            mut n: usize,
        ) -> Result<&'a [Chunk], fmt::Error> {
            let mut chunks = chunks.iter();
            while let Some(chunk) = chunks.next() {
                if let Chunk::Pop(_) = chunk {
                    n = n.saturating_sub(1);
                }

                if f.alternate() {
                    f.write_str("\n    ")?;
                    for _ in 0..n {
                        f.write_str("    ")?;
                    }
                } else {
                    f.write_str(" ")?;
                }
                match chunk {
                    Chunk::Embed(ts) => fmt::Debug::fmt(ts, f)?,
                    Chunk::Unit(tt) => match tt {
                        TokenTree::Group(_) => todo!(),
                        TokenTree::Ident(tt) => write!(f, "{tt}")?,
                        TokenTree::Punct(p) => {
                            let mut p = p;
                            write!(f, "{p}")?;
                            while let Some(Chunk::Unit(TokenTree::Punct(p2))) =
                                chunks.as_slice().get(0)
                                && p.spacing() == Spacing::Joint
                            {
                                p = p2;
                                write!(f, "{p}")?;
                                chunks.next();
                            }
                        }
                        TokenTree::Literal(tt) => write!(f, "{tt}")?,
                    },
                    Chunk::Push(delim) => {
                        let (open, close) = match delim {
                            Delimiter::Parenthesis => ("(", ")"),
                            Delimiter::Brace => ("{", "}"),
                            Delimiter::Bracket => ("[", "]"),
                            Delimiter::None => ("\\(", "\\)"),
                        };

                        f.write_str(open)?;
                        if let Some(Chunk::Pop(_)) = chunks.as_slice().get(0) {
                            chunks.next();
                        } else {
                            chunks = fmt_chunks_until_pop(chunks.as_slice(), f, n + 1)?.iter();
                        }
                        f.write_str(close)?;
                    }
                    Chunk::Pop(_) => break,
                }
            }

            Ok(chunks.as_slice())
        }

        f.write_str("${")?;
        let mut chunks = self.0;
        while !chunks.is_empty() {
            if chunks.as_ptr() != self.0.as_ptr() {
                f.write_str("$POP")?;
            }
            chunks = fmt_chunks_until_pop(chunks, f, 0)?;
        }
        let s = if f.alternate() { "\n}$" } else { " }$" };
        f.write_str(s)
    }
}

macro_rules! impl_into_chunk_for_tt {
    ($($ty:ty),*) => {
        $(
            impl From<$ty> for Chunk {
                #[inline]
                fn from(tt: $ty) -> Self {
                    Chunk::Unit(tt.into())
                }
            }
        )*
    };
}

impl_into_chunk_for_tt!(Punct, Ident, Literal, Group);

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
            stack_depth: 0,
        }
    }

    /// Creates a token queue with space for at least `n` tokens.
    pub fn with_capacity(n: usize) -> TokenQueue {
        TokenQueue {
            chunks: Vec::with_capacity(n),
            stack_depth: 0,
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
        let (min, max, popped) = token_size_hint_for_chunks(&self.chunks);
        assert!(!popped, "{POP_NO_PUSH_MSG}");
        (min, max)
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
    pub fn extend_stream(self, ts: &mut TokenStream) {
        let mut builder = TokenStreamBuilder::new(self.chunks.into_iter());
        builder.drain_to::<(), _>(ts);
        builder.assert_no_trailing_pop();
    }

    /// Copies the contents of the given `TokenQueue`
    pub fn concat(&mut self, rhs: &TokenQueue) {
        self.chunks.extend_from_slice(&rhs.chunks);
        self.stack_depth += rhs.stack_depth;
    }

    pub fn open_group(&mut self, delim: Delimiter) {
        self.stack_depth += 1;
        self.chunks.push(Chunk::Push(delim));
    }

    pub fn close_group_with_span(&mut self, span: Option<Span>) {
        self.stack_depth = self
            .stack_depth
            .checked_sub(1)
            .unwrap_or_else(|| panic!("{POP_NO_PUSH_MSG}"));
        self.chunks.push(Chunk::Pop(span));
    }

    pub fn close_group(&mut self) {
        self.close_group_with_span(None);
    }
}

fn balance_chunks(chunks: &[Chunk]) -> &[Chunk] {
    let mut stack = 0usize;
    let mut chunks = chunks.iter();

    while let Some(chunk) = chunks.next() {
        match chunk {
            Chunk::Push(_) => stack += 1,
            Chunk::Pop(_) => match stack.checked_sub(1) {
                Some(n) => stack = n,
                None => return chunks.as_slice(),
            },
            _ => {}
        }
    }

    &[]
}

fn token_size_hint_for_chunks(chunks: &[Chunk]) -> (usize, Option<usize>, bool) {
    let mut n = 0;
    let mut bounded_above = true;
    let mut chunks = chunks.iter();

    let mut flagged = false;
    while let Some(chunk) = chunks.next() {
        match chunk {
            Chunk::Embed(_) => bounded_above = false,
            Chunk::Unit(_) => n += 1,
            Chunk::Push(_) => {
                // 1 for the group:
                n += 1;
                chunks = balance_chunks(chunks.as_slice()).iter()
            }
            // always consumed by balance_chunks
            Chunk::Pop(_) => {
                flagged = true;
                break;
            }
        }
    }

    (n, bounded_above.then_some(n), flagged)
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
            stack_depth: 0,
        }
    }
}

/// "Commits" the token queue by value, constructing a [`TokenStream`] from the contents.
impl From<TokenQueue> for TokenStream {
    fn from(q: TokenQueue) -> TokenStream {
        let mut builder = TokenStreamBuilder::new(q.chunks.into_iter());
        let ts = builder.drain_to_token_stream();
        builder.assert_no_trailing_pop();
        ts
    }
}

impl<'a> From<&'a TokenQueue> for TokenStream {
    fn from(q: &'a TokenQueue) -> TokenStream {
        let mut builder = TokenStreamBuilder::new(ByRef(q.chunks.iter()));
        let ts = builder.drain_to_token_stream();
        builder.assert_no_trailing_pop();
        ts
    }
}

impl<'a> From<&'a mut TokenQueue> for TokenStream {
    fn from(q: &'a mut TokenQueue) -> TokenStream {
        let mut builder = TokenStreamBuilder::new(q.chunks.drain(..));
        let ts = builder.drain_to_token_stream();
        builder.assert_no_trailing_pop();
        ts
    }
}

trait ChunkBuf: Iterator<Item = Chunk> {
    fn remaining(&self) -> &[Chunk];
    fn lookahead(&self, n: usize) -> Option<&Chunk> {
        self.remaining().get(n)
    }
    fn len(&self) -> usize {
        self.remaining().len()
    }
}

impl ChunkBuf for std::vec::IntoIter<Chunk> {
    fn remaining(&self) -> &[Chunk] {
        self.as_slice()
    }
}

impl<'a> ChunkBuf for std::vec::Drain<'a, Chunk> {
    fn remaining(&self) -> &[Chunk] {
        self.as_slice()
    }
}

struct ByRef<'a>(std::slice::Iter<'a, Chunk>);

impl<'a> Iterator for ByRef<'a> {
    type Item = Chunk;

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.0.size_hint()
    }

    fn next(&mut self) -> Option<Chunk> {
        self.0.next().cloned()
    }
}

impl<'a> ChunkBuf for ByRef<'a> {
    fn remaining(&self) -> &[Chunk] {
        self.0.as_slice()
    }
}

/// Parses a buffer of [`Chunk`]s until either EOS or a [`Pop`](Chunk::Pop).
struct TokenStreamBuilder<S> {
    ts_queue: Option<proc_macro::token_stream::IntoIter>,
    chunks: S,
    /// The span associated with a trailing [`Pop`](Chunk::Pop).
    ///
    /// Note that `Option` is sufficient:
    /// every pop is preceeded by a push, which ensures that the span is pulled out before it is replaced.
    last: TsbLast,
}

#[derive(Debug, Clone)]
enum TsbLast {
    Pop(Option<Span>),
    FallOut,
}

impl TsbLast {
    fn take(&mut self) -> Option<Option<Span>> {
        let s = match self {
            TsbLast::Pop(span) => Some(*span),
            TsbLast::FallOut => None,
        };
        *self = TsbLast::FallOut;
        s
    }

    fn set(&mut self, span: Option<Span>) {
        match self {
            TsbLast::Pop(span2) => {
                panic!("{span:?} overwrote pop span {span2:?}")
            }
            TsbLast::FallOut => *self = TsbLast::Pop(span),
        }
    }

    fn is_ready(&self) -> bool {
        match self {
            TsbLast::Pop(_) => true,
            TsbLast::FallOut => false,
        }
    }
}

impl<S: ChunkBuf> fmt::Debug for TokenStreamBuilder<S> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("TokenStreamBuilder")
            .field("ts_queue", &self.ts_queue.clone().map(Vec::from_iter))
            .field("chunks", &FmtChunks(self.chunks.remaining()))
            .field("last", &self.last)
            .finish()
    }
}

impl<S: ChunkBuf> TokenStreamBuilder<S> {
    fn new(chunks: S) -> TokenStreamBuilder<S> {
        TokenStreamBuilder {
            ts_queue: None,
            last: TsbLast::FallOut,
            chunks,
        }
    }

    fn assert_no_trailing_pop(&self) {
        assert!(!self.last.is_ready(), "{POP_NO_PUSH_MSG}");
    }

    #[inline(never)]
    fn drain_to_token_stream(&mut self) -> TokenStream {
        self.drain_to(())
    }

    #[inline]
    fn as_unwrapped_streams(&mut self) -> Option<impl Iterator<Item = TokenStream> + '_> {
        let (n, span) = self
            .chunks
            .remaining()
            .iter()
            .enumerate()
            .find_map(|(i, tc)| match tc {
                &Chunk::Pop(span) => Some((i, Some(span))),
                _ => None,
            })
            .unwrap_or_else(|| (self.chunks.len(), None));

        let possible = self.chunks.remaining()[..n]
            .iter()
            .all(|tc| matches!(tc, Chunk::Embed(_)));

        if !possible {
            return None;
        }

        let ts = self.chunks.by_ref().take(n + 1).filter_map(|tc| match tc {
            Chunk::Embed(ts) => Some(ts),
            _ => None,
        });

        if let Some(span) = span {
            self.last.set(span);
        }

        Some(ts)
    }

    #[inline]
    fn take_as_single_stream(&mut self) -> Option<TokenStream> {
        let Some(Chunk::Embed(_)) = self.chunks.lookahead(0) else {
            return None;
        };

        let Chunk::Embed(ts) = self.chunks.next().unwrap() else {
            unreachable!();
        };

        // accepts either [Embed] or [Embed, Pop, ..] since those are both "1-stream".
        // NB: [Push, .., Pop, ..] is kinda also 1-stream but we don't consider it to be rn.
        match self.chunks.lookahead(0) {
            Some(&Chunk::Pop(span)) => {
                // skip the pop in the parent stream.
                self.chunks.next();
                self.last.set(span);
                Some(ts)
            }
            None => Some(ts),
            Some(_) => None,
        }
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
        let (min, max, _) = token_size_hint_for_chunks(self.chunks.remaining());
        (min, max)
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
            Some(Chunk::Unit(tt)) => Some(tt),
            Some(Chunk::Push(delim)) => {
                // let s = format!("{self:?}");
                // NB: recursion.
                let ts = self.drain_to_token_stream();
                let mut group = Group::new(delim, ts);
                match self.last.take() {
                    Some(Some(span)) => group.set_span(span),
                    Some(None) => {}
                    None => {
                        panic!("{PUSH_NO_POP_MSG}")
                    }
                };

                Some(group.into())
            }
            Some(Chunk::Pop(span)) => {
                self.last.set(span);
                None
            }
        }
    }
}
