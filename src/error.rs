use core::fmt;
use std::{borrow::Cow, mem::replace};

use proc_macro::{Delimiter, Group, Ident, Literal, Punct, Spacing, Span, TokenStream, TokenTree};

use crate::{ToSpan, ToTokens, TokensExtend};

pub type Result<T, E = Error> = core::result::Result<T, E>;

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

#[derive(Debug)]
enum ErrorKind {
    Expected(Span, Syntax),
    Custom(Span, Box<dyn std::error::Error>),
    Join(Vec<ErrorKind>),
}

impl PartialEq for ErrorKind {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (ErrorKind::Expected(_, a), ErrorKind::Expected(_, b)) => a == b,
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
/// using [`Error::from_expected_lit`] and [`Error::from_expected_noun`].
/// See their docs for more information.
///
/// ## Composition
///
/// Discrete errors can be combined into a single error value
/// using [`Error::flatten`] and [`Error::join`].
///
/// ## Reporting
///
/// For use in `proc_macro`s, `Error` provides a [`ToTokens`] implementation,
/// which compiles to an invocation of the [`compile_error`] macro.
#[derive(Debug, PartialEq)]
pub struct Error {
    kind: ErrorKind,
}

impl Error {
    /// Creates an error indicating literal syntax that was expected.
    ///
    /// # Reporting
    ///
    /// Interpreted as an expected literal error, `"the syntax"` is displayed
    /// as ``expected `the syntax` ``.
    pub fn from_expected_lit(span: impl ToSpan, literal: impl Into<Cow<'static, str>>) -> Error {
        Self {
            kind: ErrorKind::Expected(span.span(), Syntax::Literal(literal.into())),
        }
    }

    /// Creates an error indicating the name of some syntax that was expected.
    ///
    /// # Reporting
    ///
    /// Interpreted as an expected noun error, `"the syntax"` is displayed
    /// exactly as written, like `expected the syntax`.
    pub fn from_expected_noun(span: impl ToSpan, name: impl Into<Cow<'static, str>>) -> Error {
        Self {
            kind: ErrorKind::Expected(span.span(), Syntax::Noun(name.into())),
        }
    }

    /// Creates an error with a customizable message.
    ///
    /// # Reporting
    ///
    /// Errors creates with this constructor transparently report the message
    /// exactly as it is passed.
    pub fn new_custom(span: impl ToSpan, msg: impl std::error::Error + 'static) -> Error {
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
    pub fn flatten(errors: impl IntoIterator<Item = Error>) -> Error {
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
    pub fn join(&mut self, b: Error) {
        let a = self;
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

    fn try_join<T>(&mut self, res: Result<T>) -> Option<T> {
        match res {
            Ok(ok) => Some(ok),
            Err(err) => {
                self.join(err);
                None
            }
        }
    }

    /// Combines a potential error to this object.
    ///
    /// Returns `true` iff `res` is `Ok(())`.
    ///
    /// See [`Error::join`] for semantic details.
    pub fn try_join_ok(&mut self, res: Result<()>) -> bool {
        self.try_join(res).is_some()
    }
}

impl ToTokens for Error {
    fn to_tokens(&self) -> TokenStream {
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

        fn extend_buf(buf: &mut TokenStream, error: &ErrorKind) {
            match &error {
                ErrorKind::Expected(span, syntax) => {
                    compile_err_call(buf, *span, &format!("expected {syntax}"))
                }
                ErrorKind::Custom(span, custom_err) => {
                    compile_err_call(buf, *span, &custom_err.to_string())
                }
                ErrorKind::Join(errors) => {
                    for err in errors {
                        extend_buf(buf, err);
                    }
                }
            }
        }

        let mut buf = TokenStream::new();
        extend_buf(&mut buf, &self.kind);
        buf
    }
}
