//! General purpose buffer for token composition. See [`TokenQueue`].

use std::fmt;

use proc_macro::{Delimiter, Group, Ident, Literal, Punct, Span, TokenStream, TokenTree};

use crate::IntoTokens;

/// General purpose buffer for token composition.
///
/// See also [`quote`](crate::quote!) and [`Transcriber`](crate::Transcriber).
///
/// # I Have No Mouth And I Must [`TokenStream`]
/// ***TLDR:*** `TokenQueue` is like the `Vec<SmallVec<[T; 1]>>` to [`TokenStream`]'s `Arc<Vec<T>>`.
///
/// [`TokenStream`] is well-suited for token storage.
/// Under the hood, it holds a clone-on-write handle to an underlying buffer
/// so it is cheap to share and traverse, but not cheap to modify.
///
/// The API surface of [`TokenStream`] is also too constrained for our purposes,
/// prohibiting buffer preallocation, buffer reuse, slicing, and by-reference traversal.
///
/// `TokenQueue`, by contrast, owns its contents and so is cheap to modify.
/// We can also preallocate the underlying buffer and so escape quadratic time complexity.
/// The read API is locked down such that
/// [conversions into `TokenStream`](TokenQueue#impl-From%3CTokenQueue%3E-for-TokenStream)
/// ("commits") happen all at once,
/// significantly reducing the overhead of dealing with the proc-macro server.
///
/// There are many methods for interacting directly with the queue,
/// but it is most common to combine [`extend_from`](TokenQueue::extend_from) and [`quote`](crate::quote!):
///
/// ```
/// # vermouth::ඞ_declare_test!();
/// # use vermouth::{quote, TokenQueue};
/// let ref mut q = TokenQueue::new();
/// q.extend_from(quote! { 1 + 2 });
/// q.extend_from(quote! { = });
/// q.extend_from(quote! { 3 });
/// // q: `1 + 2 = 3`
/// ```
///
/// # Substreams
///
/// To ensure high memory utilisation, we would like to reuse the same heap allocation
/// for constructing both a root [`TokenStream`], and the `TokenStream`s nested within any [`Group`]s it contains.
///
/// To this end, [`TokenQueue::open_substream`] and [`TokenQueue::close_substream`]
/// reconfigure the queue to begin collecting tokens which will go towards building a nested stream.
///
/// For groups in particular, there are the [`close_substream_and_push_as_group`]
/// and [`close_substream_and_push_as_group_with_span`] helper methods:
///
/// [`close_substream_and_push_as_group`]: TokenQueue::close_substream_and_push_as_group
/// [`close_substream_and_push_as_group_with_span`]: TokenQueue::close_substream_and_push_as_group_with_span
///
/// ```
/// # vermouth::ඞ_declare_test!();
/// # use proc_macro::Delimiter;
/// # use vermouth::{quote, TokenQueue};
/// let ref mut q = TokenQueue::new();
/// q.extend_from(quote! { let x = });
/// q.open_substream();
/// q.extend_from(quote! { 1, 2, 3 });
/// q.close_substream_and_push_as_group(Delimiter::Parenthesis);
/// q.extend_from(quote! { ; });
/// // q: `let x = (1, 2, 3);`
/// ```
///
/// It is also possible to factor out substreams onto the stack.
/// This approach has better heap efficiency but worse stack efficiency,
/// resulting in no practical performance difference.
/// It often leads to more readable `quote` invocations.
///
/// ```
/// # vermouth::ඞ_declare_test!();
/// # use proc_macro::{Group, Delimiter};
/// # use vermouth::{quote, TokenQueue};
/// let ref mut q = TokenQueue::new();
/// q.open_substream();
/// q.extend_from(quote! { 1, 2, 3 });
/// let expr = Group::new(Delimiter::Parenthesis, q.close_substream());
/// q.extend_from(quote! { let x = $expr; });
/// // q: `let x = (1, 2, 3);`
/// ```
///
/// Internally, the stack of open substreams is an intrusive, singly-linked list,
/// ensuring opening and closing are amortized to constant execution time.
#[derive(Debug, Clone)]
pub struct TokenQueue {
    chunks: Vec<Chunk>,
    /// Index of the top of the substream stack.
    substream_stack_top_ptr: Option<usize>,
    span_tracking: SpanTracking,
}

/// A "shallow stack" managing [span annotations](Transcriber::with_span).
///
/// This only holds the outermost span; nested spans are counted but not used.
#[derive(Debug, Clone, Default)]
struct SpanTracking {
    current: Option<Span>,
    ignored: usize,
}

impl SpanTracking {
    const fn none() -> SpanTracking {
        SpanTracking {
            current: None,
            ignored: 0,
        }
    }

    fn set(&mut self, next: Span) {
        match self.current {
            Some(_) => self.ignored += 1,
            None => self.current = Some(next),
        }
    }

    fn unset(&mut self) {
        match self.ignored.checked_sub(1) {
            Some(n) => self.ignored = n,
            None => self.current = None,
        }
    }

    fn current(&self) -> Option<Span> {
        self.current
    }
}

#[derive(Debug, Clone)]
enum Chunk {
    PutGroup(Group),
    PutIdent(Ident),
    PutLiteral(Literal),
    PutPunct(Punct),
    Embed(TokenStream),
    OpenSubstream(StackParent),
}

/// A chunk's reference to its parent.
///
/// If zero, this is a top-level element.
/// If nonzero, this is the offset of this substream relative to the parent within the chunk buffer.
///
/// No chunk is its own parent, so offset is never 0.
#[derive(Debug, Default, Clone, Copy)]
struct StackParent(u32);

impl StackParent {
    #[inline]
    pub fn new(addr: usize, ptr: Option<usize>) -> StackParent {
        let n = ptr.map_or(0, |i| u32::try_from(addr - i).unwrap());
        StackParent(n)
    }

    #[inline]
    pub fn get(self, addr: usize) -> Option<usize> {
        if self.0 == 0 {
            return None;
        }

        Some(addr - usize::try_from(self.0).unwrap())
    }
}

struct DisplayChunks<'a>(&'a [Chunk]);
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

        fn fmt_group(group: &Group, f: &mut fmt::Formatter<'_>, n: usize) -> fmt::Result {
            let (open, close) = delim_wings(group.delimiter());
            f.write_str(open)?;
            fmt_ts(group.stream().into_iter(), f, n + 1)?;
            f.write_str(close)
        }

        fn fmt_ts(
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

        fn fmt_chunks(chunks: &[Chunk], f: &mut fmt::Formatter<'_>, mut n: usize) -> fmt::Result {
            for chunk in chunks {
                match chunk {
                    Chunk::Embed(ts) => fmt_ts(ts.clone().into_iter(), f, n)?,
                    Chunk::PutGroup(group) => fmt_group(group, f, n)?,
                    Chunk::PutIdent(id) => write!(f, "{id}")?,
                    Chunk::PutPunct(p) => write!(f, "{p}")?,
                    Chunk::PutLiteral(lit) => write!(f, "{lit}")?,
                    Chunk::OpenSubstream(_) => {
                        f.write_str("∅")?;
                        n += 1;
                    }
                }
            }

            Ok(())
        }

        fmt_chunks(self.0, f, 0)
    }
}

/// Utility trait for managing `TokenTree` to `Chunk` conversion.
///
/// This is a kind of AOT monomorphisation, which is not done for performance,
/// but instead purely for the sake of privacy (we don't want to leak [`Chunk`], so we don't put it in trait bounds).
trait ChunkLike: Clone {
    fn into_chunk(self) -> Chunk;
    fn set_span(&mut self, span: Span);

    #[inline]
    fn into_chunk_with_span(mut self, span: Option<Span>) -> Chunk {
        if let Some(span) = span {
            self.set_span(span);
        }

        self.into_chunk()
    }
}

/// See [`ChunkLike`].
macro_rules! impl_chunklike_for_tt {
    ($($var:ident($ty:ty),)*) => {
        $(
            impl ChunkLike for $ty {
                #[inline]
                fn into_chunk(self) -> Chunk {
                    Chunk::$var(self.into())
                }

                fn set_span(&mut self, span: Span) {
                    self.set_span(span);
                }
            }
        )*
    };
}

impl_chunklike_for_tt! {
    PutPunct(Punct),
    PutIdent(Ident),
    PutLiteral(Literal),
    PutGroup(Group),
}

impl ChunkLike for TokenTree {
    fn into_chunk(self) -> Chunk {
        match self {
            TokenTree::Group(a) => Chunk::PutGroup(a),
            TokenTree::Ident(a) => Chunk::PutIdent(a),
            TokenTree::Punct(a) => Chunk::PutPunct(a),
            TokenTree::Literal(a) => Chunk::PutLiteral(a),
        }
    }

    fn set_span(&mut self, span: Span) {
        self.set_span(span);
    }
}

impl From<TokenStream> for Chunk {
    #[inline]
    fn from(ts: TokenStream) -> Self {
        Chunk::Embed(ts)
    }
}

/// Implements `IntoTokens` for various `TokenTree`-like types.
macro_rules! impl_into_tokens_for_tt {
    ($($t:ty),*) => {
        $(
            impl IntoTokens for $t {
                fn extend_tokens(self, q: &mut TokenQueue) {
                    q.chunks.push(self.into_chunk_with_span(q.tracked_span()));
                }

                fn queue_size_hint(&self) -> (usize, Option<usize>) {
                    (1, Some(1))
                }
            }
        )*
    };
}

impl_into_tokens_for_tt! { TokenTree, Punct, Ident, Group, Literal }

/// Invokes the given macro with all of the implementors of `Into<Chunk>`.
///
/// This is a kind of AOT monomorphisation, which is not done for performance,
/// but instead purely for the sake of privacy (we don't want to leak [`Chunk`], so we don't put it in trait bounds).
macro_rules! enumerate_into_chunk_implementors {
    ($macro:ident) => {
        // NB: Not `TokenTree`.
        // we want to avoid the footgun of like `q.extend(stream)` when `q.push(stream)` will always be faster.
        $macro! { Punct, Ident, Literal, Group }
    };
}

/// See [`TokenQueue::push`].
pub trait PushToken: IntoTokens {}

/// Implements [`PushToken`] for `Chunk`-likes.
macro_rules! impl_push_token_for_into_chunk {
    ($($ty:ty),*) => {
        $(
            impl PushToken for $ty {}
        )*
    };
}

enumerate_into_chunk_implementors!(impl_push_token_for_into_chunk);

impl PushToken for TokenStream {}
impl PushToken for TokenTree {}
impl<T: PushToken> PushToken for Option<T> {}

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
            substream_stack_top_ptr: None,
            span_tracking: SpanTracking::none(),
        }
    }

    /// Creates a token queue with space for at least `n` tokens.
    pub fn with_capacity(n: usize) -> TokenQueue {
        TokenQueue {
            chunks: Vec::with_capacity(n),
            substream_stack_top_ptr: None,
            span_tracking: SpanTracking::none(),
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
        self.extend_from(t);
    }

    /// Extends an existing [`TokenStream`] with the contents of this queue.
    pub fn extend_stream(mut self, ts: &mut TokenStream) {
        self.chunks.drain(..).collect_by(ts)
    }

    /// Enqueues some tokens via [`IntoTokens::extend_tokens`].
    pub fn extend_from<T: IntoTokens>(&mut self, t: T) {
        // self.chunks.extend_from_slice(&rhs.chunks);
        // self.stack_depth += rhs.stack_depth;
        t.extend_tokens(self)
    }

    /// Opens a new substream. See [the substream documentation](#substreams).
    pub fn open_substream(&mut self) {
        let ptr = self.chunks.len();
        let parent = StackParent::new(ptr, self.substream_stack_top_ptr);
        self.chunks.push(Chunk::OpenSubstream(parent));
        self.substream_stack_top_ptr = Some(ptr);
    }

    /// Closes and returns the top substream. See [the substream documentation](#substreams).
    #[must_use = "`close_substream` returns a `TokenStream` and does not enqueue anything."]
    #[track_caller]
    pub fn close_substream(&mut self) -> TokenStream {
        let Some(ptr) = self.substream_stack_top_ptr else {
            // NB: track caller because it's not our fault if this panic procs.
            panic!("{POP_NO_PUSH_MSG}")
        };

        let mut drain = self.chunks.drain(ptr..);
        let Some(Chunk::OpenSubstream(parent)) = drain.next() else {
            // NB: it is our fault if this one procs, but uhh.. it shouldn't.
            unreachable!(
                "`close_substream`: expected chunk at index {ptr} to be a `OpenSubstream`"
            );
        };

        self.substream_stack_top_ptr = parent.get(ptr);

        drain.collect_by(())
    }

    /// Closes and enqueues the top substream. See [the substream documentation](#substreams).
    pub fn close_substream_and_push_as_group_with_span(
        &mut self,
        delim: Delimiter,
        span: Option<Span>,
    ) {
        let mut group = Group::new(delim, self.close_substream());
        if let Some(span) = span {
            group.set_span(span);
        }
        self.push(group);
    }

    /// Closes and enqueues the top substream. See [the substream documentation](#substreams).
    pub fn close_substream_and_push_as_group(&mut self, delim: Delimiter) {
        self.close_substream_and_push_as_group_with_span(delim, None);
    }

    /// Annotates the tokens inserted between this call and the corresponding
    /// [`unset_tracked_span`](TokenQueue::unset_tracked_span) call with the given span.
    ///
    /// This method is leveraged by [`Transcriber::with_span`](crate::Transcriber::with_span).
    ///
    /// # Nesting
    ///
    /// Multiple span-tracking regions may be nested, but only the outermost span will be respected.
    ///
    /// ```
    /// # vermouth::ඞ_declare_test!();
    /// # use vermouth::{quote, TokenQueue};
    /// # use proc_macro::Span;
    /// #
    /// # let a = Span::call_site();
    /// # #[cfg(any())]
    /// let a: Span = omitted!();
    /// # let b = Span::call_site();
    /// # #[cfg(any())]
    /// let b: Span = omitted!();
    /// #
    /// let ref mut q = TokenQueue::new();
    /// q.set_tracked_span(a);
    /// q.extend_from(quote! { 1 });
    /// // span = a           ^^^
    /// q.set_tracked_span(b);
    /// q.extend_from(quote! { 2 });
    /// // span = a           ^^^
    /// q.unset_tracked_span();
    /// q.extend_from(quote! { 3 });
    /// // span = a           ^^^
    /// q.unset_tracked_span();
    /// q.extend_from(quote! { 4 });
    /// // span = none        ^^^
    /// ```
    ///
    /// This behavior might seem counterintuitive,
    /// but it allows callers to override the spans provided by nested calls.
    pub fn set_tracked_span(&mut self, span: Span) {
        self.span_tracking.set(span);
    }

    /// Closes a span-tracking region.
    ///
    /// See [`set_tracked_span`](TokenQueue::set_tracked_span) for more.
    pub fn unset_tracked_span(&mut self) {
        self.span_tracking.unset();
    }

    /// Returns the current span for this span-tracking region.
    ///
    /// See [`set_tracked_span`](TokenQueue::set_tracked_span) for more.
    pub fn tracked_span(&self) -> Option<Span> {
        self.span_tracking.current()
    }

    /// Formats the queue as Rust source.
    ///
    /// # Stability
    /// This is primarily for debugging purposes,
    /// no guarantees are made about the format or its fidelity.
    pub fn display(&self) -> impl fmt::Display {
        DisplayChunks(&self.chunks)
    }
}

fn token_size_hint_for_chunks(chunks: &[Chunk]) -> (usize, Option<usize>) {
    let mut n = 0;
    let mut bounded_above = true;

    for chunk in chunks {
        match chunk {
            Chunk::Embed(_) => bounded_above = false,
            Chunk::PutGroup(_) | Chunk::PutIdent(_) | Chunk::PutLiteral(_) | Chunk::PutPunct(_) => {
                n += 1
            }
            // substreams are just element shuffling, i.e. entirely immaterial.
            Chunk::OpenSubstream(_) => break,
        }
    }

    (n, bounded_above.then_some(n))
}

/// Implements `Extend` and `FromIterator` over the various `Chunk`-likes.
macro_rules! impl_extend_for_into_chunk {
    ($($ty:ty),*) => {
        $(
            impl Extend<$ty> for TokenQueue {
                fn extend<T: IntoIterator<Item = $ty>>(&mut self, tcs: T) {
                    let span = self.tracked_span();
                    self.chunks.extend(tcs.into_iter().map(move |tc| tc.into_chunk_with_span(span)));
                }
            }

            impl FromIterator<$ty> for TokenQueue {
                fn from_iter<I: IntoIterator<Item = $ty>>(tcs: I) -> TokenQueue {
                    TokenQueue {
                        chunks: tcs.into_iter().map(move |tc| tc.into_chunk()).collect(),
                        substream_stack_top_ptr: None,
                        span_tracking: SpanTracking::none(),
                    }
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
            substream_stack_top_ptr: None,
            span_tracking: SpanTracking::none(),
        }
    }
}

impl IntoTokens for TokenQueue {
    fn extend_tokens(self, q: &mut TokenQueue) {
        q.chunks.extend(self.chunks);
    }

    fn into_tokens(self) -> TokenQueue {
        self
    }

    fn queue_size_hint(&self) -> (usize, Option<usize>) {
        let n = self.len();
        (n, Some(n))
    }
}

/// "Commits" the token queue by value, constructing a [`TokenStream`] from the contents.
impl From<TokenQueue> for TokenStream {
    fn from(mut q: TokenQueue) -> TokenStream {
        q.chunks.drain(..).collect_by(())
    }
}

trait ChunkBuf: Iterator<Item = Chunk> + Sized {
    fn remaining(&self) -> &[Chunk];
    #[inline]
    fn peek(&self) -> Option<&Chunk> {
        self.remaining().first()
    }
    #[inline]
    fn as_unwrapped_streams(&mut self) -> Option<impl Iterator<Item = TokenStream> + '_> {
        let possible = self
            .remaining()
            .iter()
            .all(|tc| matches!(tc, Chunk::Embed(_)));

        if !possible {
            return None;
        }

        let ts = self.by_ref().filter_map(|chunk| match chunk {
            Chunk::Embed(ts) => Some(ts),
            _ => None,
        });

        Some(ts)
    }

    #[inline]
    fn take_as_single_stream(&mut self) -> Option<TokenStream> {
        let Some(Chunk::Embed(_)) = self.peek() else {
            return None;
        };

        let Chunk::Embed(ts) = self.next().unwrap() else {
            unreachable!();
        };

        Some(ts)
    }

    #[inline]
    fn collect_by<B: FromTokens<T>, T>(mut self, t: T) -> B {
        if let Some(ts) = self.take_as_single_stream() {
            // if the buf contains just a single stream elem, use that directly.
            // this is useful for quoting something like `#[$meta]`.
            B::from_lone(t, ts)
        } else if let Some(tss) = self.as_unwrapped_streams() {
            // if the buf is all streams, get `proc_macro` to concat them directly, rather than copying ourselves.
            // this is useful for quoting something like `$attrs $item $trait_impls`.
            B::from_streams(t, tss)
        } else {
            let builder = TokenStreamBuilder {
                chunks: self,
                ts_queue: None,
            };
            B::from_tokens(t, builder)
        }
    }
}

impl<'a> ChunkBuf for std::vec::Drain<'a, Chunk> {
    fn remaining(&self) -> &[Chunk] {
        self.as_slice()
    }
}

/// Parses a buffer of [`Chunk`]s until EOS.
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

trait FromTokens<T> {
    fn from_lone(t: T, ts: TokenStream) -> Self;
    fn from_streams(t: T, tss: impl Iterator<Item = TokenStream>) -> Self;
    fn from_tokens(t: T, tts: impl Iterator<Item = TokenTree>) -> Self;
}

impl FromTokens<()> for TokenStream {
    #[inline]
    fn from_lone(_: (), ts: TokenStream) -> TokenStream {
        ts
    }
    #[inline]
    fn from_streams(_: (), tss: impl Iterator<Item = TokenStream>) -> TokenStream {
        tss.collect()
    }
    #[inline]
    fn from_tokens(_: (), tts: impl Iterator<Item = TokenTree>) -> TokenStream {
        tts.collect()
    }
}

impl<'a> FromTokens<&'a mut TokenStream> for () {
    #[inline]
    fn from_lone(s: &'a mut TokenStream, ts: TokenStream) {
        s.extend(Some(ts));
    }
    #[inline]
    fn from_streams(s: &'a mut TokenStream, tss: impl Iterator<Item = TokenStream>) {
        s.extend(tss);
    }
    #[inline]
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
            Some(Chunk::OpenSubstream(_)) => panic!("{PUSH_NO_POP_MSG}"),
        }
    }
}
