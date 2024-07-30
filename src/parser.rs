use proc_macro::{Group, Ident, Punct, Spacing, Span, TokenStream, TokenTree};

use crate::{Error, Expected, Pattern, Result, ToTokens, TokensExtend};

/// A simple parser for Rust source which traverses [`TokenTree`]s.
///
pub struct Parser {
    seen: Vec<TokenTree>,
    seen_ptr: usize,
    stream: proc_macro::token_stream::IntoIter,
    eos_span: Span,
    error_buf: Vec<Error>,
}

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
    seen_ptr: usize,
    error_count: usize,
}

impl From<Group> for Parser {
    fn from(value: Group) -> Self {
        Parser::new(value.stream(), value.span())
    }
}

impl Parser {
    /// Creates a new [`Parser`].
    ///
    /// The `parent_span` argument is used to generate [end-of-stream spans](Parser::here)
    /// when the stream has no tokens.
    ///
    /// For parsing the input from a procedural macro, `parent_span` should be [`Span::call_site()`].
    pub fn new(stream: TokenStream, parent_span: Span) -> Self {
        Self {
            stream: stream.into_iter(),
            eos_span: parent_span,
            seen: Vec::new(),
            seen_ptr: 0,
            error_buf: Vec::new(),
        }
    }

    /// Inserts another [`Error`] into the parser's internal error buffer.
    ///
    /// These errors can be retrieved with the [`Parser::compile_errors`] method.
    #[inline]
    pub fn report(&mut self, err: impl Into<Error>) {
        self.error_buf.push(err.into());
    }

    /// If passed `Err`, [report it] and return `false`. Returns `true` otherwise.
    ///
    /// These errors can be retrieved with the [`Parser::compile_errors`] method.
    ///
    /// [report it]: Parser::report
    #[inline]
    pub fn try_report(&mut self, res: Result<(), impl Into<Error>>) -> bool {
        match res {
            Ok(()) => true,
            Err(err) => {
                self.report(err);
                false
            }
        }
    }

    /// Consumes a token from the parser's internal stream.
    ///
    /// This is the most direct way of interacting with the parser stream.
    ///
    /// Note that this method returns an `Option`, rather than a `Result`,
    /// to ensure that errors are accurate and well-handled.
    pub fn nibble(&mut self) -> Option<TokenTree> {
        let opt = match self.seen.get(self.seen_ptr) {
            Some(o) => Some(o.clone()),
            None => self
                .stream
                .next()
                .inspect(|tok| self.seen.push(tok.clone())),
        };

        opt.inspect(|tok| {
            self.eos_span = tok.span();
            self.seen_ptr += 1;
        })
    }

    /// Parses a value from the parser's internal stream using a pattern for context.
    ///
    /// This method is an ergonomic alias for [`Pattern::eat`].
    pub fn eat<T: Pattern>(&mut self, hungry_guy: T) -> Result<T::Output> {
        hungry_guy.eat(self)
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

    /// A combinator for [`Parser::nibble`], which enables graceful, consistent error reporting.
    ///
    /// Both in the case that there are no tokens in the parser's stream ([`Parser::nibble`] returns `None`),
    /// and in the case that `None` is returned from the `pass_if` argument,
    /// the same error (supplied by the `expects` argument) is returned.
    ///
    /// Note that due to borrow checker restrictions, the parser is inaccessible during the `pass_if` call,
    /// So any transitive data must be returned from `pass_if` (and thus the entire funciton)
    /// to be used in parsing.
    pub fn eat_expectantly<T>(
        &mut self,
        pass_if: impl FnOnce(TokenTree) -> Option<T>,
        expects: impl FnOnce(ParserPos) -> Expected,
    ) -> Result<T, Expected> {
        let pos;
        match self.nibble() {
            None => pos = self.here(),
            Some(tok) => match pass_if(tok) {
                None => pos = self.digesting(),
                Some(res) => return Ok(res),
            },
        }

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

    /// Similar to [`Parser::eat_punct`], but considers the `Punct`'s spacing.
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

    /// Returns the position of the parser within a stream of tokens, as a [`Span`].
    ///
    /// If the parser is at the end of the stream, it returns a `Span` covering the entire stream.
    pub fn here(&self) -> ParserPos {
        let (span, span_kind) = self.seen.get(self.seen_ptr).map_or_else(
            || (self.eos_span, SpanKind::EndOfStream),
            |tt| (tt.span(), SpanKind::InStream),
        );

        ParserPos {
            span_kind,
            span,
            checkpoint: self.save(),
        }
    }

    /// Returns the position of the just-parsed token in the parser stream.
    ///
    /// If are no previous tokens, a best-effort position
    /// pointing to the start of the stream is returned.
    pub fn digesting(&self) -> ParserPos {
        let (span, seen_ptr) = (self.seen_ptr.checked_sub(1))
            .and_then(|ptr| self.seen.get(ptr))
            .map_or_else(|| (self.eos_span, 0), |tt| (tt.span(), self.seen_ptr - 1));

        ParserPos {
            span_kind: SpanKind::InStream,
            span,
            checkpoint: Checkpoint {
                seen_ptr,
                error_count: self.error_buf.len(),
            },
        }
    }

    /// Saves the state of the parser to a [`Checkpoint`].
    ///
    /// This state can be returned to by [`Parser::restore`].
    pub fn save(&self) -> Checkpoint {
        Checkpoint {
            seen_ptr: self.seen_ptr,
            error_count: self.error_buf.len(),
        }
    }

    /// Restores the state of the parser to a [previously saved](Parser::save) [`Checkpoint`].
    ///
    /// This is useful for backtracking when encountering errors.
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
        self.seen_ptr = point.seen_ptr;
        span
    }

    /// Restores the state of the parser to a [previously saved](Parser::save) [`Checkpoint`].
    ///
    /// This is useful for backtracking when encountering errors.
    ///
    /// # Reporting
    ///
    /// See [`Parser::restore`] for more details and what makes this method "forgiving".
    pub fn restore_forgiving(&mut self, point: &Checkpoint) -> ParserPos {
        self.error_buf
            .drain(point.error_count..self.error_buf.len());
        self.restore(point)
    }

    /// Undos the consumption of a certain number of tokens.
    ///
    /// This is a lighter, and less powerful alternative to [`Parser::save`] and [`Parser::restore`].
    ///
    /// # Reporting
    ///
    /// **NB:** This method has the same "unforgiving" effects on error reporting as [`Parser::restore`].
    pub fn gag(&mut self, n: usize) {
        let point = Checkpoint {
            seen_ptr: self.seen_ptr - n,
            error_count: self.error_buf.len(),
        };
        self.restore(&point);
    }

    /// Returns all compile errors [reported] during parsing.
    ///
    /// [reported]: Parser::report
    pub fn compile_errors(&self) -> TokenStream {
        let mut buf = TokenStream::new();
        for error in &self.error_buf {
            error.extend_tokens(&mut buf);
        }
        buf
    }

    /// Collects all tokens until a condition is met.
    ///
    /// The behavior of the final token is specified by [`Finish`],
    /// and the end-of-stream response is handled by the `eos_behavior` parameter.
    pub fn collect_until(
        &mut self,
        mut stop_condition: impl FnMut(&TokenTree) -> bool,
        stop_behavior: impl FnOnce(&TokenTree) -> Result<Finish>,
        eos_behavior: impl FnOnce(ParserPos) -> Result<()>,
    ) -> Result<TokenStream> {
        let mut buf = TokenStream::new();
        while let Some(tok) = self.nibble() {
            if stop_condition(&tok) {
                match stop_behavior(&tok)? {
                    Finish::Eat => buf.push(tok),
                    Finish::Gag => {
                        self.gag(1);
                    }
                    Finish::Void => (),
                }
                return Ok(buf);
            }

            buf.push(tok);
        }

        eos_behavior(self.here()).map(|_| buf)
    }

    /// Returns the remaining contents of the parser's stream, as a [`TokenStream`].
    pub fn rest(&mut self) -> TokenStream {
        self.stream.by_ref().collect()
    }
}

#[derive(Debug)]
pub(crate) enum SpanKind {
    InStream,
    EndOfStream,
}

#[derive(Debug)]
pub struct ParserPos {
    pub(crate) span_kind: SpanKind,
    pub(crate) span: Span,
    pub(crate) checkpoint: Checkpoint,
}

impl ParserPos {
    /// Creates an arbitrary [`ParserPos`] value for use in testing and documentation.
    ///
    /// While this method is allowed to produce absurd or illogical values,
    /// it is never unsafe to call.
    pub fn arbitrary() -> Self {
        Self {
            // NB: this value affects error messages,
            // so should stay constant.
            span_kind: SpanKind::InStream,
            span: Span::call_site(),
            checkpoint: Checkpoint {
                seen_ptr: 0,
                error_count: 0,
            },
        }
    }

    pub fn restore(&self, cx: &mut Parser) {
        cx.restore(&self.checkpoint);
    }
}

/// Describes the termination behaviour of [`Parser::collect_until`].
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Finish {
    /// Consume the token and push it into the collection buffer.
    Eat,
    /// Leave the token in the parser stream.
    Gag,
    /// Drop the token.
    Void,
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
