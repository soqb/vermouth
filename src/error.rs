use core::fmt;
use std::{borrow::Cow, mem::replace};

use proc_macro::{Delimiter, Group, Ident, Literal, Punct, Spacing, Span, TokenStream, TokenTree};

use crate::{Parser, ParserPos, PosKind, ToSpan, ToTokens, TokensExtend};

/// An alias for the standard library [`Result`](core::result::Result).
///
/// By default, the error type is [`Expected`].
pub type Result<T, E = Expected> = core::result::Result<T, E>;

#[derive(Debug, PartialEq, Eq)]
enum Syntax {
    Literal(Cow<'static, str>),
    Noun(Cow<'static, str>),
}

impl fmt::Display for Syntax {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Syntax::Literal(s) => write!(f, "`{s}`"),
            Syntax::Noun(n) => f.write_str(n),
        }
    }
}

/// Builds an [error](Error) which represents some syntax which was expected.
///
/// After construction, syntax variants can be chained with
/// [`Expected::or_lit`] and [`Expected::or_noun`],
/// or they can be inserted in place with
/// [`Expected::push_lit`] and [`Expected::push_noun`].
#[derive(Debug)]
pub struct Expected {
    pos: ParserPos,
    syntaxes: Vec<Syntax>,
}

impl From<Expected> for ParserPos {
    fn from(value: Expected) -> Self {
        value.pos
    }
}

impl ToSpan for Expected {
    fn span(&self) -> Span {
        self.pos.span()
    }
}

impl PartialEq for Expected {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.syntaxes == other.syntaxes
    }
}

impl Expected {
    /// Indicates no syntax was expected.
    ///
    /// For clarity, it may be preferrable
    /// to use [`Expected::lit`] or [`Expected::noun`] directly,
    /// rather than chaining this method and [`Expected::or_lit`] or [`Expected::or_noun`].
    ///
    /// # Examples
    ///
    /// ```
    /// # vermouth::ඞ_declare_test!();
    /// # use vermouth::Expected;
    /// #
    /// # let parser_pos = vermouth::ParserPos::arbitrary();
    /// let error = Expected::nothing(parser_pos);
    /// assert_eq!(error.to_string(), "expected no tokens");
    /// ```
    #[inline]
    pub fn nothing(pos: impl Into<ParserPos>) -> Self {
        Self {
            pos: pos.into(),
            syntaxes: vec![],
        }
    }

    /// Indicates literal syntax was expected.
    ///
    /// # Examples
    ///
    /// ```
    /// # vermouth::ඞ_declare_test!();
    /// # use vermouth::Expected;
    /// #
    /// # let parser_pos = vermouth::ParserPos::arbitrary();
    /// let error = Expected::lit(parser_pos, "foo");
    /// assert_eq!(error.to_string(), "expected `foo`");
    /// ```
    #[inline]
    pub fn lit(pos: impl Into<ParserPos>, lit: impl Into<Cow<'static, str>>) -> Self {
        Self {
            pos: pos.into(),
            syntaxes: vec![Syntax::Literal(lit.into())],
        }
    }

    /// Indicates the name of some syntax was expected.
    ///
    /// # Examples
    ///
    /// ```
    /// # vermouth::ඞ_declare_test!();
    /// # use vermouth::Expected;
    /// #
    /// # let parser_pos = vermouth::ParserPos::arbitrary();
    /// let error = Expected::noun(parser_pos, "a bar");
    /// assert_eq!(error.to_string(), "expected a bar");
    /// ```
    #[inline]
    pub fn noun(pos: impl Into<ParserPos>, noun: impl Into<Cow<'static, str>>) -> Self {
        Self {
            pos: pos.into(),
            syntaxes: vec![Syntax::Noun(noun.into())],
        }
    }

    /// Pushes a literal variant into this expectation.
    ///
    /// # Examples
    ///
    /// ```
    /// # vermouth::ඞ_declare_test!();
    /// # use vermouth::Expected;
    /// #
    /// # let parser_pos = vermouth::ParserPos::arbitrary();
    /// let mut error = Expected::lit(parser_pos, "foo");
    /// error.push_lit("bar");
    /// error.push_lit("baz");
    /// assert_eq!(error.to_string(), "expected `foo`, `bar`, or `baz`");
    /// ```
    #[inline]
    pub fn push_lit(&mut self, lit: impl Into<Cow<'static, str>>) {
        self.syntaxes.push(Syntax::Literal(lit.into()));
    }

    /// Pushes a named variant into this expectation.
    ///
    /// # Examples
    ///
    /// ```
    /// # vermouth::ඞ_declare_test!();
    /// # use vermouth::Expected;
    /// #
    /// # let parser_pos = vermouth::ParserPos::arbitrary();
    /// let mut error = Expected::noun(parser_pos, "some foo");
    /// error.push_noun("any kind of bar");
    /// error.push_noun("a baz");
    /// assert_eq!(error.to_string(), "expected some foo, any kind of bar, or a baz");
    /// ```
    #[inline]
    pub fn push_noun(&mut self, noun: impl Into<Cow<'static, str>>) {
        self.syntaxes.push(Syntax::Noun(noun.into()));
    }

    /// Combines this expectation with a literal variant.
    ///
    /// # Examples
    ///
    /// ```
    /// # vermouth::ඞ_declare_test!();
    /// # use vermouth::Expected;
    /// #
    /// # let parser_pos = vermouth::ParserPos::arbitrary();
    /// let error = Expected::nothing(parser_pos).or_lit("foo").or_noun("any bar");
    /// assert_eq!(error.to_string(), "expected `foo`, or any bar");
    /// ```
    #[inline]
    pub fn or_lit(mut self, lit: impl Into<Cow<'static, str>>) -> Self {
        self.push_lit(lit);
        self
    }

    /// Combines this expectation with a noun.
    ///
    /// # Examples
    ///
    /// ```
    /// # vermouth::ඞ_declare_test!();
    /// # use vermouth::Expected;
    /// #
    /// # let parser_pos = vermouth::ParserPos::arbitrary();
    /// let error = Expected::nothing(parser_pos).or_noun("some foo").or_lit("bar");
    /// assert_eq!(error.to_string(), "expected some foo, or `bar`");
    /// ```
    #[inline]
    pub fn or_noun(mut self, noun: impl Into<Cow<'static, str>>) -> Self {
        self.push_noun(noun);
        self
    }

    /// Restores the state of the parser to the [postion] just before the error was encountered.
    ///
    /// Functionally parallel to [`Parser::restore`] and [`Parser::seek_to`].
    pub fn recover(&self, cx: &mut Parser) {
        cx.seek_to(&self.pos)
    }
}

impl fmt::Display for Expected {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "expected ")?;
        let Some((last, rest)) = self.syntaxes.split_last() else {
            return write!(f, "no tokens");
        };

        for syntax in rest {
            write!(f, "{syntax}, ")?;
        }

        if self.syntaxes.len() > 1 {
            write!(f, "or ")?;
        }
        write!(f, "{last}")?;

        if let PosKind::EndOfStream = self.pos.pos_kind {
            write!(f, ", but found the end of input")?;
        }

        Ok(())
    }
}

trait DisplayDebug: fmt::Debug + fmt::Display {}
impl<T: fmt::Debug + fmt::Display> DisplayDebug for T {}

#[derive(Debug)]
enum ErrorKind {
    Expected(Expected),
    Custom(Span, Box<dyn DisplayDebug>),
    Join(Vec<ErrorKind>),
}

impl PartialEq for ErrorKind {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (ErrorKind::Expected(a), ErrorKind::Expected(b)) => a == b,
            (ErrorKind::Join(a), ErrorKind::Join(b)) => a == b,
            // explicitly return false in this case, we don't have `PartialEq` for the dynamic error.
            (ErrorKind::Custom(_, _), ErrorKind::Custom(_, _)) => false,
            _ => false,
        }
    }
}

/// The opaque error type used for parsing.
///
/// The `Error` type contains one or more errors encountered during parsing.
///
/// ## Construction
///
/// Errors representing expected syntax elements can be constructed
/// using the [`Expected`], and this types corresponding `From<Expected>` implementation.
/// See their docs for more information.
///
/// ## Composition
///
/// For several variants of expected, but not found syntax,
/// [`Expected`] can be composed with [`Expected::or_lit`] and [`Expected::or_noun`].
///
/// Discrete errors can be combined into a single error value
/// using [`Error::and`] and [`Error::and_many`].
///
/// ## Reporting
///
/// For use in `proc_macro`s, `Error` provides a [`ToTokens`] implementation,
/// which compiles to an invocation of the [`compile_error`] macro.
#[derive(Debug, PartialEq)]
pub struct Error {
    kind: ErrorKind,
}

impl From<Expected> for Error {
    #[inline]
    fn from(value: Expected) -> Self {
        Self {
            kind: ErrorKind::Expected(value),
        }
    }
}

impl Error {
    // pub fn from_expected_noun(span: impl ToSpan, name: impl Into<Cow<'static, str>>) -> Error {
    //     Self {
    //         kind: ErrorKind::Expected(span.span(), Syntax::Noun(name.into())),
    //     }
    // }

    /// Creates an error with a customizable message.
    ///
    /// # Reporting
    ///
    /// Errors creates with this constructor transparently report the message
    /// exactly as it is passed.
    #[inline]
    pub fn custom(span: impl ToSpan, msg: impl fmt::Display + fmt::Debug + 'static) -> Error {
        Self {
            kind: ErrorKind::Custom(span.span(), Box::new(msg)),
        }
    }

    /// Combines several errors in a single object.
    ///
    /// # Reporting
    ///
    /// Each error passed to this constructor
    /// generates a separate invocation of [`compile_error`].
    pub fn and_many(errors: impl IntoIterator<Item = Error>) -> Error {
        let errors = errors.into_iter();
        let mut buf = Vec::with_capacity(errors.size_hint().0);

        // note we do not need to recursively flatten `ErrorKind::Join`s,
        // because we ensure they are shallow on construction.
        errors.for_each(|err| Self::extend(&mut buf, err.kind));

        Self {
            kind: ErrorKind::Join(buf),
        }
    }

    #[inline]
    fn extend(buf: &mut Vec<ErrorKind>, err: ErrorKind) {
        match err {
            ErrorKind::Join(errors) => buf.extend(errors),
            _ => buf.push(err),
        }
    }

    /// Combines another error to this object.
    ///
    /// # Reporting
    ///
    /// The passed error is generated alongside those stored in `self`.
    pub fn and(&mut self, b: impl Into<Error>) {
        let (a, b) = (self, b.into());
        match (&mut a.kind, b.kind) {
            (ErrorKind::Join(a_buf), ErrorKind::Join(b_buf)) => {
                a_buf.extend(b_buf);
            }
            (ErrorKind::Join(a_buf), b_kind) => {
                Self::extend(a_buf, b_kind);
            }
            (_, ErrorKind::Join(b_buf)) => {
                let old_a = replace(
                    &mut a.kind,
                    ErrorKind::Join(Vec::with_capacity(b_buf.len() + 1)),
                );
                let ErrorKind::Join(a_buf) = &mut a.kind else {
                    // we just made `a` an `Error::Or`.
                    unreachable!()
                };

                Self::extend(a_buf, old_a);
                a_buf.extend(b_buf);
            }
            (_, b_kind) => {
                let old_a = replace(&mut a.kind, ErrorKind::Join(Vec::with_capacity(2)));
                let ErrorKind::Join(a_buf) = &mut a.kind else {
                    // we just made `a` an `Error::Or`.
                    unreachable!()
                };

                a_buf.extend([old_a, b_kind]);
            }
        }
    }
}

impl ToTokens for ErrorKind {
    fn extend_tokens(&self, buf: &mut TokenStream) {
        fn compile_err_call(buf: &mut TokenStream, span: Span, msg: &str) {
            macro_rules! quote_path {
                ($buf:ident <-) => {};
                ($buf:ident <- :: $n:ident $(:: $r:ident)*) => {
                    $buf.push(Punct::new(':', Spacing::Joint));
                    $buf.push(Punct::new(':', Spacing::Alone));
                    $buf.push(Ident::new(stringify!($n), span));

                    // recurse
                    quote_path!($buf <- $(:: $r)*)
                };

            }

            quote_path!(buf <- ::core::compile_error);
            buf.push(Punct::new('!', Spacing::Alone));

            let msg: TokenTree = Literal::string(msg).into();
            buf.push(Group::new(
                Delimiter::Parenthesis,
                TokenStream::from_iter([msg]),
            ));
            buf.push(Punct::new(';', Spacing::Alone));
        }

        match &self {
            ErrorKind::Expected(exp) => compile_err_call(buf, exp.pos.span(), &exp.to_string()),
            ErrorKind::Custom(span, custom_err) => {
                compile_err_call(buf, *span, &custom_err.to_string())
            }
            ErrorKind::Join(errors) => {
                for err in errors {
                    err.extend_tokens(buf);
                }
            }
        }
    }
}

impl ToTokens for Error {
    #[inline]
    fn extend_tokens(&self, buf: &mut TokenStream) {
        self.kind.extend_tokens(buf)
    }
}
