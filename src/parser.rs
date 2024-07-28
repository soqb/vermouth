use proc_macro::{Delimiter, Group, Ident, Span, TokenStream, TokenTree};

use crate::{Error, Result, ToSpan, TokensExtend};

/// A simple parser for Rust source which traverses [`TokenTree`]s.
///
/// This type typically iterates over a [`TokenStream`],
/// but is generic as to support many different stream implementations.
pub struct Parser<I = proc_macro::token_stream::IntoIter> {
    seen: Vec<TokenTree>,
    seen_ptr: usize,
    stream: I,
    eos_span: Span,
}

/// A location in the stream of a [`Parser`].
///
/// This is can be used for backtracking after encountering an error.
/// This type can be a burden in simple cases;
/// consider [`Parser::gag`] when this becomes the case.
///
/// Created exclusively with [`Parser::save`] and
/// consumed exclusively by [`Parser::restore`].
#[derive(Clone)]
#[must_use = "saving a `Checkpoint` is useless if it is never restored"]
pub struct Checkpoint {
    seen_ptr: usize,
}

impl From<Group> for Parser {
    fn from(value: Group) -> Self {
        Parser::new(value.stream(), value.span())
    }
}

impl<I: Iterator<Item = TokenTree>> Parser<I> {
    /// Creates a new [`Parser`].
    ///
    /// The `parent_span` argument is used to generate [end-of-stream spans](Parser::here)
    /// when the stream has no tokens.
    ///
    /// For parsing the input from a procedural macro, `parent_span` should be [`Span::call_site()`].
    pub fn new(stream: impl IntoIterator<IntoIter = I>, parent_span: Span) -> Self {
        Self {
            stream: stream.into_iter(),
            eos_span: parent_span,
            seen: Vec::new(),
            seen_ptr: 0,
        }
    }

    /// Consumes a token from the parser's internal stream.
    pub fn eat(&mut self) -> Option<TokenTree> {
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

    /// A combinator for [`Parser::eat`], which enables graceful error reporting.
    ///
    /// Both in the case that there are no tokens in the parser's stream ([`Parser::eat`] returns `None`),
    /// and in the case that `None` is returned from the `pass_if` argument,
    /// the same error (supplied by the `expects` argument) is returned.
    ///
    /// Note that due to borrow checker restrictions, the parser is inaccessible during the `pass_if` call,
    /// So any transitive data must be returned from `pass_if` (and thus the entire funciton)
    /// to be used in parsing.
    pub fn eat_expectantly<T>(
        &mut self,
        pass_if: impl FnOnce(&TokenTree) -> Option<T>,
        expects: impl FnOnce(Span) -> Error,
    ) -> Result<T> {
        let span;
        match self.eat() {
            None => span = self.here(),
            Some(tok) => match pass_if(&tok) {
                None => span = self.gag(1),
                Some(res) => return Ok(res),
            },
        }

        Err(expects(span))
    }

    /// Returns the position of the parser within a stream of tokens, as a [`Span`].
    ///
    /// If the parser is at the end of the stream, it returns a `Span` covering the entire stream.
    pub fn here(&self) -> Span {
        self.seen
            .get(self.seen_ptr)
            .map_or(self.eos_span, ToSpan::span)
    }

    /// Saves the state of the parser to a [`Checkpoint`].
    ///
    /// This state can be returned to by [`Parser::restore`].
    pub fn save(&self) -> Checkpoint {
        let seen_ptr = self.seen_ptr;
        Checkpoint { seen_ptr }
    }

    /// Restores the state of the parser to a [previously saved](Parser::save) [`Checkpoint`].
    ///
    /// This is useful for backtracking when encountering errors.
    ///
    /// This method returns the `Span` of the position the parser was at before restoring.
    /// This span can be useful for error reporting.
    /// See [`Parser::here`] for details on how the span is calculated.
    pub fn restore(&mut self, ck: Checkpoint) -> Span {
        let span = self.here();
        self.seen_ptr = ck.seen_ptr;
        span
    }

    /// Undos the consumption of a certain number of tokens.
    ///
    /// This is a lighter, and less powerful alternative to [`Parser::save`] and [`Parser::restore`].
    pub fn gag(&mut self, n: usize) -> Span {
        let ck = Checkpoint {
            seen_ptr: self.seen_ptr - n,
        };
        self.restore(ck)
    }
}

impl<I: Iterator<Item = TokenTree>> Parser<I> {
    /// Returns a new [`Parser`] which traverses the contents of a `Group`.
    ///
    /// The `Group` must be delimited by the specified [`Delimiter`].
    pub fn delimited_by(&mut self, delim: Delimiter) -> Result<Group> {
        match self.eat() {
            Some(TokenTree::Group(contents)) if contents.delimiter() == delim => Ok(contents),
            _ => {
                let delim_str = match delim {
                    Delimiter::Parenthesis => "parentheses",
                    Delimiter::Brace => "braces",
                    Delimiter::Bracket => "brackets",
                    Delimiter::None => "implicit delimiters",
                };
                Err(Error::from_expected_noun(self.gag(1), delim_str))
            }
        }
    }

    /// Collects all tokens until a condition is met.
    ///
    /// The behavior of the final token is specified by [`Finish`],
    /// and the end-of-stream response is handled by the `eos_behavior` parameter.
    pub fn collect_until(
        &mut self,
        mut stop_condition: impl FnMut(&TokenTree) -> bool,
        stop_behavior: impl FnOnce(&TokenTree) -> Result<Finish>,
        eos_behavior: impl FnOnce(Span) -> Result<()>,
    ) -> Result<TokenStream> {
        let mut buf = TokenStream::new();
        while let Some(tok) = self.eat() {
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

        eos_behavior(self.eos_span).map(|_| buf)
    }

    /// Returns the remaining contents of the parser's stream, as a [`TokenStream`].
    pub fn rest(&mut self) -> TokenStream {
        self.stream.by_ref().collect()
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
pub trait Parse: Sized {
    /// Parses a value from a [`Parser`].
    fn parse<I: Iterator<Item = TokenTree>>(parser: &mut Parser<I>) -> Result<Self>;
}

impl Parse for Ident {
    fn parse<I: Iterator<Item = TokenTree>>(cx: &mut Parser<I>) -> Result<Self> {
        match cx.eat() {
            Some(TokenTree::Ident(i)) => Ok(i),
            _ => Err(Error::from_expected_noun(cx.gag(1), "an identifier")),
        }
    }
}
