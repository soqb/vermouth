//! [A parser](Parser) for proc-macro input.

use proc_macro::{Group, Ident, Punct, Spacing, Span, TokenStream, TokenTree};

use crate::{Diagnostic, Expected, IntoTokens, Pattern, Result, ToSpan, TokenQueue};

struct Stream<T, I> {
    seen_buffer: Vec<T>,
    seen_idx: SeenIdx,
    iter: I,
}

impl<T: Clone, I: Iterator<Item = T>> Stream<T, I> {
    fn take_in(&mut self) {
        self.iter
            .next()
            .inspect(|tok| self.seen_buffer.push(tok.clone()));
    }

    pub fn with_capacity(iter: I, len: u32) -> Self {
        assert!(len < u32::MAX, "stream length cannot be u32::MAX");

        let mut this = Self {
            seen_buffer: Vec::with_capacity(len as usize),
            seen_idx: SeenIdx::START,
            iter,
        };
        this.take_in();
        this
    }

    pub fn peek(&self) -> Option<T> {
        self.seen_buffer
            .get(self.seen_idx.to_u32() as usize)
            .cloned()
    }

    pub fn idx(&self) -> SeenIdx {
        self.seen_idx
    }

    pub fn move_on(&mut self) -> SeenIdx {
        self.take_in();
        self.seen_idx.increment()
    }

    pub fn seek_to(&mut self, idx: SeenIdx) {
        self.seen_idx = idx;
    }
}

mod indices {
    use std::num::NonZero;

    /// An index into the `seen` buffer in a `Parser`.
    ///
    /// Internally holds a `u32` which is one more than the index into the buffer.
    ///
    /// A value of `0` (i.e. a position guaranteed not to be in the buffer)
    /// is a sentinel marking
    #[repr(transparent)]
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub struct SeenIdx {
        idx: NonZero<u32>,
    }

    impl SeenIdx {
        pub const START: Self = Self::new_raw(1);
        pub const ARBITRARY: Self = {
            // any value is as good as any other
            // but im listening to hamilton rn:
            let value = 1776;
            Self::new_raw(value)
        };

        /// Creates a new non-zero [`SeenIdx`].
        #[inline]
        pub(crate) const fn new_raw(idx: u32) -> Self {
            SeenIdx {
                idx: NonZero::new(idx).unwrap(),
            }
        }

        /// Unwraps and shifts down the index.
        #[inline]
        pub(crate) fn to_u32(self) -> u32 {
            self.idx.get().wrapping_sub(1)
        }

        #[inline]
        pub fn increment(&mut self) -> SeenIdx {
            let old = *self;
            self.idx = self.idx.checked_add(1).unwrap();
            old
        }

        #[inline]
        pub fn seek_back(self, n: usize) -> Option<Self> {
            let idx = self.idx.get() as usize;
            if n >= idx {
                #[cold]
                fn none() -> Option<SeenIdx> {
                    None
                }
                return none();
            }
            let idx = u32::try_from(idx - n).unwrap();
            Some(Self::new_raw(idx))
        }

        #[inline]
        pub fn from_pos(pos: PosRepr) -> Option<Self> {
            NonZero::new(pos.idx).map(|idx| Self { idx })
        }
    }

    /// A `u32`-packed representation of a parser position.
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub(crate) struct PosRepr {
        idx: u32,
    }

    impl PosRepr {
        pub const EOS: Self = Self { idx: 0 };

        #[inline]
        pub fn from_seen(seen: SeenIdx) -> Self {
            Self {
                idx: seen.idx.get(),
            }
        }

        #[cfg(test)]
        pub fn into_raw(self) -> u32 {
            self.idx
        }
    }
}

use indices::{PosRepr, SeenIdx};

/// A location in the stream of a [`Parser`].
///
/// This is can be used for backtracking after encountering an error.
/// This type can be a burden in simple cases;
/// consider [`Parser::gag`] when this is the case.
///
/// Created exclusively with [`Parser::save`] and
/// consumed exclusively by [`Parser::restore`].
#[derive(Clone, Debug)]
#[must_use = "saving a `Checkpoint` is useless if it is never restored"]
pub struct Checkpoint {
    seen_idx: SeenIdx,
    error_count: usize,
}

impl From<Group> for Parser {
    fn from(value: Group) -> Self {
        Parser::new(value.stream(), value.span())
    }
}

/// A simple parser for Rust source which traverses [`TokenTree`]s.
// FIXME: go back to parameterising by iterator type!
pub struct Parser {
    stream: Stream<TokenTree, proc_macro::token_stream::IntoIter>,
    eos_span: Span,
    diag_buf: Vec<Diagnostic>,
}

impl Parser {
    /// Creates a new [`Parser`].
    ///
    /// The `parent_span` argument is used to generate [end-of-stream spans](Parser::here)
    /// when the stream has no tokens.
    ///
    /// For parsing the input from a procedural macro, `parent_span` should be [`Span::call_site()`].
    pub fn new(stream: TokenStream, parent_span: Span) -> Self {
        let len = stream.clone().into_iter().count();
        Self {
            stream: Stream::with_capacity(stream.into_iter(), len.try_into().unwrap()),
            #[cfg(not(feature = "proc-macro2"))]
            eos_span: parent_span.end(),
            #[cfg(feature = "proc-macro2")]
            // you'd fucking think it were possible. wouldn't you..
            eos_span: parent_span,
            diag_buf: Vec::new(),
        }
    }

    /// Inserts another [`Diagnostic`] into the parser's internal diagnostics buffer.
    ///
    /// These diagnostics can be emitted with the [`Parser::finish_diagnostics`] method.
    #[inline]
    pub fn report(&mut self, err: impl Into<Diagnostic>) {
        self.diag_buf.push(err.into());
    }

    /// If passed `Err`, [report it] and return `false`. Returns `true` otherwise.
    ///
    /// These diagnostics can be emitted with the [`Parser::finish_diagnostics`] method.
    ///
    /// [report it]: Parser::report
    #[inline]
    pub fn maybe_report(&mut self, res: Result<(), impl Into<Diagnostic>>) -> bool {
        match res {
            Ok(()) => true,
            Err(err) => {
                self.report(err);
                false
            }
        }
    }

    /// Unit test function for validating parser position.
    #[cfg_attr(not(test), expect(dead_code, reason = "test-only function"))]
    pub(crate) fn raw_pos(&self) -> ParserPos {
        ParserPos {
            span: self.eos_span,
            repr: PosRepr::from_seen(self.stream.idx()),
        }
    }

    /// Returns `true` if the parser stream contains no tokens after the cursor.
    pub fn is_empty(&self) -> bool {
        self.here().is_eos()
    }

    /// Returns the token from the stream just after the cursor, without moving the cursor.
    pub fn peek(&self) -> Option<TokenTree> {
        self.stream.peek()
    }

    /// Consumes a token from the parser's internal stream.
    ///
    /// This is the most direct way of interacting with the parser stream.
    ///
    /// Note that the token is returned in an `Option`, rather than a `Result`,
    /// to ensure that errors are accurate and well-handled.
    ///
    /// The returned `ParserPos` is useful for creating `Expected` errors;
    /// it represents the position in the stream just before the returned token.
    pub fn nibble(&mut self) -> (Option<TokenTree>, ParserPos) {
        let (tt, pos_data) = self.peek().map_or_else(
            || (None, PosRepr::EOS),
            |tt| {
                let idx = self.stream.move_on();
                self.eos_span = tt.span();
                (Some(tt), PosRepr::from_seen(idx))
            },
        );

        (
            tt,
            ParserPos {
                span: self.eos_span,
                repr: pos_data,
            },
        )
    }

    /// Parses a value from the parser's internal stream using a pattern for context.
    ///
    /// This method is an ergonomic alias for [`Pattern::eat`].
    pub fn eat<T: Pattern>(&mut self, meal: T) -> Result<T::Output> {
        meal.eat(self)
    }

    /// Parses a value from the parser's internal stream using some arguments for context.
    ///
    /// This method is an ergonomic alias for [`Parse::parse_with`].
    pub fn parse_with<T: Parse>(&mut self, args: T::Args<'_>) -> Result<T> {
        T::parse_with(self, args)
    }

    /// Parses a value from the parser's internal stream without context.
    ///
    /// This method is an ergonomic alias for [`Parse::parse`].
    pub fn parse<T: Parse>(&mut self) -> Result<T>
    where
        for<'a> T::Args<'a>: Default,
    {
        T::parse(self)
    }

    /// A combinator for [`Parser::nibble`] which enables graceful, consistent error reporting.
    ///
    /// Both in the case that there are no tokens in the parser's stream ([`Parser::nibble`] returns `None`),
    /// and in the case that `None` is returned from the `try_pass` argument,
    /// the same error (supplied by the `expects` argument) is returned.
    ///
    /// Note that due to borrow checker restrictions, the parser is inaccessible during the `try_pass` call,
    /// So any transitive data must be returned from `try_pass` (and thus the entire funciton)
    /// to be used in parsing.
    pub fn eat_expectantly<T>(
        &mut self,
        try_pass: impl FnOnce(TokenTree) -> Option<T>,
        expects: impl FnOnce(ParserPos) -> Expected,
    ) -> Result<T, Expected> {
        let pos = match self.nibble() {
            (None, pos) => pos,
            (Some(tt), pos) => match try_pass(tt) {
                None => pos,
                Some(res) => return Ok(res),
            },
        };

        Err(expects(pos))
    }

    /// Returns the next token in the parser stream, if it is an [`Ident`].
    ///
    /// If the next token is not an `Ident`, returns a representative error.
    pub fn eat_ident(&mut self) -> Result<Ident, Expected> {
        self.eat_expectantly(
            |tok| match tok {
                TokenTree::Ident(ident) => Some(ident),
                _ => None,
            },
            |span| Expected::noun(span, "an identifier"),
        )
    }

    /// Returns the next token in the parser stream, if it is a particular [`Punct`].
    ///
    /// If the next token is not the specified `Punct`, returns a representative error.
    pub fn eat_punct(&mut self, punct: char) -> Result<Punct, Expected> {
        self.eat_expectantly(
            |tok| match tok {
                TokenTree::Punct(pt) if pt.as_char() == punct => Some(pt),
                _ => None,
            },
            |span| Expected::lit(span, punct.to_string()),
        )
    }

    /// Similar to [`Parser::eat_punct`], but taking into account the [`Punct`]'s spacing.
    pub fn eat_punct_with_spacing(
        &mut self,
        punct: char,
        spacing: Spacing,
    ) -> Result<Punct, Expected> {
        self.eat_expectantly(
            |tok| match tok {
                TokenTree::Punct(pt) if pt.as_char() == punct && pt.spacing() == spacing => {
                    Some(pt)
                }
                _ => None,
            },
            |span| Expected::lit(span, punct.to_string()),
        )
    }

    /// Returns the position of the parser within a stream of tokens, as a [`ParserPos`].
    ///
    /// If the parser is at the end of the stream,
    /// it returns a position with a `Span` covering the entire stream.
    pub fn here(&self) -> ParserPos {
        let (span, pos_data) = self.peek().map_or_else(
            || (self.eos_span, PosRepr::EOS),
            |tt| (tt.span(), PosRepr::from_seen(self.stream.idx())),
        );

        ParserPos {
            repr: pos_data,
            span,
        }
    }

    /// Saves the state of the parser to a [`Checkpoint`].
    ///
    /// This state can be returned to by [`Parser::restore`].
    pub fn save(&self) -> Checkpoint {
        Checkpoint {
            seen_idx: self.stream.idx(),
            error_count: self.diag_buf.len(),
        }
    }

    /// Restores the state of the parser to a [previously saved](Parser::save) [`Checkpoint`].
    ///
    /// This is useful for backtracking when encountering errors.
    /// Depending on the use case, one of
    /// [`Parser::restore_forgiving`], [`Parser::seek_to`], or [`Parser::gag`]
    /// may be more correct and/or ergonomic.
    ///
    /// This method returns the `Span` of the position the parser
    /// was at before restoring which be useful for error reporting.
    /// See [`Parser::here`] for details on how the span is calculated.
    ///
    /// # Reporting
    ///
    /// **NB:** This method is "unforgiving"
    /// in that it preserves all errors which have been [reported]
    /// since the parser state was saved.
    /// The rationale behind this is that errors which get reported
    /// (as opposed to being returned in a `Result::Err`)
    /// are typically important enough to warrant not omitting retroactively.
    ///
    /// If the converse behavior is required, see [`Parser::restore_forgiving`].
    ///
    /// [reported]: Parser::report
    pub fn restore(&mut self, point: &Checkpoint) -> ParserPos {
        let span = self.here();
        self.stream.seek_to(point.seen_idx);
        span
    }

    /// Restores the state of a parser to a previous [position](ParserPos).
    ///
    /// This is useful for backtracking when encountering errors.
    /// Depending on the use case, one of
    /// [`Parser::restore`], [`Parser::restore_forgiving`], or [`Parser::gag`]
    /// may be more correct and/or ergonomic.
    ///
    /// # Reporting
    ///
    /// **NB:** This method has the same "unforgiving" effects on error reporting as [`Parser::restore`].
    pub fn seek_to(&mut self, pos: &ParserPos) {
        if let Some(idx) = SeenIdx::from_pos(pos.repr) {
            self.stream.seek_to(idx);
        } else {
            // doesn't really make sense,
            // nor is it useful to seek to end-of-stream.
        }
    }

    /// Restores the state of the parser to a [previously saved](Parser::save) [`Checkpoint`].
    ///
    /// This is useful for backtracking when encountering errors.
    /// Depending on the use case, one of
    /// [`Parser::restore`], [`Parser::seek_to`], or [`Parser::gag`]
    /// may be more correct and/or ergonomic.
    ///
    /// # Reporting
    ///
    /// See [`Parser::restore`] for more details and what makes this method "forgiving".
    pub fn restore_forgiving(&mut self, point: &Checkpoint) -> ParserPos {
        self.diag_buf.drain(point.error_count..self.diag_buf.len());
        self.restore(point)
    }

    /// Undoes the consumption of a certain number of tokens.
    ///
    /// This is a lighter, and less powerful alternative to [`Parser::save`] and [`Parser::restore`]
    /// and is useful for backtracking when encountering errors.
    /// Depending on the use case, one of
    /// [`Parser::restore`], [`Parser::restore_forgiving`], or [`Parser::seek_to`]
    /// may be more correct and/or ergonomic.
    ///
    /// # Reporting
    ///
    /// **NB:** This method has the same "unforgiving" effects on error reporting as [`Parser::restore`].
    #[track_caller]
    pub fn gag(&mut self, n: usize) {
        let idx = self.stream.idx().seek_back(n).unwrap_or_else(|| {
            // NB: track caller because it's not our fault if this panic procs.
            panic!("tried to `Parser::gag` to before the beginning of the parser's stream")
        });
        self.stream.seek_to(idx);
    }

    /// Returns an opaque [`IntoTokens`] instance which lazily emits
    /// all diagnostics [reported] during parsing.
    ///
    /// This method has the same semantics as [`Diagnostic::finish`].
    ///
    /// [reported]: Parser::report
    pub fn finish_diagnostics(&mut self) -> impl IntoTokens {
        struct Wrap<T>(T);
        impl<T: Iterator<Item = Diagnostic>> IntoTokens for Wrap<T> {
            fn extend_tokens(self, q: &mut TokenQueue) {
                for d in self.0 {
                    q.extend_from(d.finish());
                }
            }
        }

        Wrap(self.diag_buf.drain(..))
    }

    /// Collects all tokens until a condition is met.
    ///
    /// A token which fails the condition will not be added to the collection.
    ///
    /// An end-of-stream will finish the collection.
    /// It's common to use another parser method
    /// immediately after to detect such a case.
    pub fn collect_until<C: FromIterator<TokenTree>>(
        &mut self,
        mut stop_condition: impl FnMut(&TokenTree) -> bool,
    ) -> C {
        std::iter::from_fn(move || match self.nibble() {
            (Some(tok), _) if stop_condition(&tok) => None,
            (Some(tok), _) => Some(tok),
            (None, _) => None,
        })
        .collect()
    }

    /// Returns the remaining contents of the parser's stream, as a [`TokenStream`].
    pub fn rest(&mut self) -> TokenStream {
        self.stream.iter.by_ref().collect()
    }
}

/// A reference to a position within the internal stream of a [`Parser`].
///
/// This is primarily useful for error reporting and parser recovery.
#[derive(Debug, Clone, Copy)]
pub struct ParserPos {
    span: Span,
    repr: PosRepr,
}

impl ToSpan for ParserPos {
    fn span(&self) -> Span {
        self.span
    }
}

impl ParserPos {
    /// Creates an arbitrary [`ParserPos`] value for use in testing and documentation.
    ///
    /// Values returned from this method should never be used for modifying a `Parser`.
    ///
    /// While this method is allowed to produce absurd or illogical results,
    /// it is never unsound to call.
    pub fn arbitrary() -> Self {
        Self {
            // NB: this value affects error messages,
            // so should stay constant.
            repr: PosRepr::from_seen(SeenIdx::ARBITRARY),
            span: Span::call_site(),
        }
    }

    /// Returns whether this [`ParserPos`] is the position at the end of the stream,
    /// after the last token.
    pub fn is_eos(self) -> bool {
        self.repr == PosRepr::EOS
    }

    #[cfg(test)]
    pub(crate) fn into_raw(self) -> u32 {
        self.repr.into_raw()
    }
}

/// A common interface for parsing values from a [`Parser`].
///
/// Often, this trait is interfaced through the `Parser` type itself
/// with [`Parser::parse_with`] and [`Parser::parse`].
///
/// # Comparison with [`Pattern`]
///
/// For a comparison with the `Pattern` trait,
/// which provides a similar yet distinct API,
/// see [that trait's documentation].
///
/// [that trait's documentation]: Pattern#comparison-with-parse
pub trait Parse: Sized {
    /// Arguments passed to [`Parse::parse_with`].
    type Args<'a>;

    /// Parses a value from a [`Parser`], using the provided [`Args`].
    ///
    /// Note that when `Args` unconditionally implement [`Default`],
    /// the more terse [`Parse::parse`] method can be used.
    ///
    /// [`Args`]: Self::Args
    fn parse_with(parser: &mut Parser, args: Self::Args<'_>) -> Result<Self>;

    /// A terse alias for [`Parse::parse_with`] for when [`Args`] implement [`Default`].
    ///
    /// [`Args`]: Self::Args
    fn parse(parser: &mut Parser) -> Result<Self>
    where
        for<'a> Self::Args<'a>: Default,
    {
        Self::parse_with(parser, Default::default())
    }
}

impl Parse for TokenStream {
    type Args<'a> = ();

    fn parse_with(parser: &mut Parser, _args: Self::Args<'_>) -> Result<Self> {
        Ok(parser.rest())
    }
}
