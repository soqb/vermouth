use core::fmt;
use std::{borrow::Cow, mem::replace};

use proc_macro::{Delimiter, Group, Ident, Literal, Punct, Spacing, Span, TokenStream, TokenTree};

use crate::{ToSpan, ToTokens, TokensExtend};

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
#[derive(Debug)]
pub struct Expected {
    span: Span,
    syntaxes: Vec<Syntax>,
}

impl PartialEq for Expected {
    fn eq(&self, other: &Self) -> bool {
        self.syntaxes == other.syntaxes
    }
}

impl Expected {
    /// Indicates no syntax was expected.
    ///
    /// For clarity, it may be preferrable
    /// to use [`Expected::from_lit`] or [`Expected::from_noun`] directly,
    /// rather than chaining this method and [`Expected::or_lit`] or [`Expected::or_noun`].
    ///
    /// # Reporting
    ///
    /// An error returned from `Expected::never` is displayed
    /// as `expected no tokens`.
    pub fn never(span: impl ToSpan) -> Self {
        Self {
            span: span.span(),
            syntaxes: vec![],
        }
    }

    /// Indicates literal syntax was expected.
    ///
    /// # Reporting
    ///
    /// Interpreted as an expected literal error, `"foo"` is displayed
    /// as ``expected `foo` ``.
    pub fn from_lit(span: impl ToSpan, lit: impl Into<Cow<'static, str>>) -> Self {
        Self {
            span: span.span(),
            syntaxes: vec![Syntax::Literal(lit.into())],
        }
    }

    /// Indicates the name of some syntax was expected.
    ///
    /// # Reporting
    ///
    /// Interpreted as an expected noun error, `"foo"` is displayed
    /// exactly as written, like `expected foo`.
    pub fn from_noun(span: impl ToSpan, noun: impl Into<Cow<'static, str>>) -> Self {
        Self {
            span: span.span(),
            syntaxes: vec![Syntax::Noun(noun.into())],
        }
    }

    pub fn push_lit(&mut self, lit: impl Into<Cow<'static, str>>) {
        self.syntaxes.push(Syntax::Literal(lit.into()));
    }
    pub fn push_noun(&mut self, noun: impl Into<Cow<'static, str>>) {
        self.syntaxes.push(Syntax::Noun(noun.into()));
    }

    /// Combines this expectation with a literal.
    ///
    /// # Reporting
    ///
    /// This could result in an error like `expected foo, bar, or baz`.
    pub fn or_lit(mut self, lit: impl Into<Cow<'static, str>>) -> Self {
        self.syntaxes.push(Syntax::Literal(lit.into()));
        self
    }

    /// Combines this expectation with a noun.
    ///
    /// # Reporting
    ///
    /// This could result in an error like `expected foo, bar, or baz`.
    pub fn or_noun(mut self, noun: impl Into<Cow<'static, str>>) -> Self {
        self.syntaxes.push(Syntax::Noun(noun.into()));
        self
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
        write!(f, "{last}")
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

/// The opaque error type used for parsing in `sx`.
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
    fn from(value: Expected) -> Self {
        Self {
            kind: ErrorKind::Expected(value),
        }
    }
}

impl Error {
    pub fn expectation(expectation: Expected) -> Error {
        Self {
            kind: ErrorKind::Expected(expectation),
        }
    }

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

    fn try_and<T>(&mut self, res: Result<T>) -> Option<T> {
        match res {
            Ok(ok) => Some(ok),
            Err(err) => {
                self.and(err);
                None
            }
        }
    }

    /// Combines a potential error to this object.
    ///
    /// Returns `true` iff `res` is `Ok(())`.
    ///
    /// See [`Error::and`] for semantic details.
    pub fn try_and_ok(&mut self, res: Result<()>) -> bool {
        self.try_and(res).is_some()
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
            ErrorKind::Expected(syntax) => {
                compile_err_call(buf, syntax.span, &format!("expected {syntax}"))
            }
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
    fn extend_tokens(&self, buf: &mut TokenStream) {
        self.kind.extend_tokens(buf)
    }
}
