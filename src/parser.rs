use std::mem::replace;

use proc_macro::{Delimiter, Group, Ident, Span, TokenStream, TokenTree};

use crate::{Error, Result, ToSpan, TokensExtend};

/// A simple parser for Rust source which traverses [`TokenTree`]s.
pub struct Parser {
    seen: Vec<TokenTree>,
    seen_ptr: usize,
    stream: proc_macro::token_stream::IntoIter,
    eof_span: Span,
}

/// A location in the stream of a [`Parser`].
///
/// This is can be used for backtracking after encountering an error.
/// This type can be a burden in simple cases;
/// consider [`Parser::gag`] when this becomes the case.
///
/// Created exclusively with [`Parser::save`] and
/// consumed exclusively by [`Parser::restore`].
pub struct Checkpoint {
    seen_ptr: usize,
}

impl From<Group> for Parser {
    fn from(value: Group) -> Self {
        Parser::new(value.stream(), value.span_close())
    }
}

impl Parser {
    /// Creates a new [`Parser`].
    ///
    /// The `parent_span` argument is used to generate [end-of-stream spans](Parser::here)
    /// when the stream has no tokens.
    pub fn new(stream: TokenStream, parent_span: Span) -> Self {
        Self {
            stream: stream.into_iter(),
            eof_span: parent_span,
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
            self.eof_span = tok.span();
            self.seen_ptr += 1;
        })
    }

    pub fn eat_tentatively<T>(
        &mut self,
        pass_if: impl FnOnce(&TokenTree) -> Option<T>,
        fail: impl FnOnce(Span) -> Error,
    ) -> Result<T> {
        let Some(tok) = self.eat() else {
            return Err(fail(self.here()));
        };

        let p = self.save();
        pass_if(&tok).ok_or_else(|| fail(self.restore(p)))
    }

    /// Returns the position of the parser within a stream of tokens, as a [`Span`].
    ///
    /// If the parser is at the end of the stream, it returns a `Span` covering the entire stream.
    pub fn here(&self) -> Span {
        self.seen
            .get(self.seen_ptr)
            .map(ToSpan::span)
            .unwrap_or(self.eof_span)
    }

    /// Saves the state of the parser to a [`Checkpoint`].
    ///
    /// This state can be returned to by [`Parser::restore`].
    pub fn save(&self) -> Checkpoint {
        let seen_ptr = self.seen_ptr;
        Checkpoint { seen_ptr }
    }

    /// Restores the state of the parser to a previously [saved](Parser::save) [`Checkpoint`].
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

impl Parser {
    /// An alias for [`Parse::parse`].
    pub fn parse<T: Parse>(&mut self) -> Result<T> {
        T::parse(self)
    }

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
            } else {
                buf.push(tok);
            }
        }

        eos_behavior(self.eof_span).map(|_| buf)
    }

    /// Returns the remaining contents of the parser's stream, as a [`TokenStream`].
    pub fn rest(&mut self) -> TokenStream {
        TokenStream::from_iter(replace(
            &mut self.stream,
            TokenStream::default().into_iter(),
        ))
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
    fn parse(parser: &mut Parser) -> Result<Self>;
}

impl Parse for Ident {
    fn parse(cx: &mut Parser) -> Result<Self> {
        match cx.eat() {
            Some(TokenTree::Ident(i)) => Ok(i),
            _ => Err(Error::from_expected_noun(cx.gag(1), "an identifier")),
        }
    }
}
