use core::fmt;
use std::{borrow::Cow, mem::replace};

use proc_macro::{Span, TokenStream};

use crate::{Parser, ParserPos, ToSpan};

#[cfg_attr(feature = "unstable-diagnostics-backend", path = "emit_unstable.rs")]
mod emit;

trait Emitter {
    fn new() -> Self;
    fn emit(&mut self, level: DiagnosticLevel, span: impl ToSpan, msg: &impl ToString);
    fn finish(self) -> TokenStream;
}

/// An alias for the standard library [`Result`](core::result::Result).
///
/// By default, the error type is [`Expected`].
pub type Result<T, E = Expected> = core::result::Result<T, E>;

#[derive(Debug, PartialEq, Eq)]
enum Syntax {
    LiteralBox(Box<str>),
    LiteralStr(&'static str),
    NounBox(Box<str>),
    NounStr(&'static str),
}

impl fmt::Display for Syntax {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Syntax::LiteralBox(s) => write!(f, "`{s}`"),
            Syntax::LiteralStr(s) => write!(f, "`{s}`"),
            Syntax::NounBox(s) => f.write_str(s),
            Syntax::NounStr(s) => f.write_str(s),
        }
    }
}

impl Syntax {
    #[inline]
    fn lit(lit: Cow<'static, str>) -> Self {
        match lit {
            Cow::Borrowed(s) => Self::LiteralStr(s),
            Cow::Owned(s) => Self::LiteralBox(s.into_boxed_str()),
        }
    }

    #[inline]
    fn noun(noun: Cow<'static, str>) -> Self {
        match noun {
            Cow::Borrowed(s) => Self::NounStr(s),
            Cow::Owned(s) => Self::NounBox(s.into_boxed_str()),
        }
    }
}

/// Builds an [diagnostic](Diagnostic) which represents some syntax which was expected.
///
/// After construction, syntax variants can be chained with
/// [`Expected::or_lit`] and [`Expected::or_noun`],
/// or they can be inserted in place with
/// [`Expected::push_lit`] and [`Expected::push_noun`].
#[derive(Debug)]
pub struct Expected {
    pos: ParserPos,
    // use `smallvec` because most of the time
    // there will only be 1 element.
    syntaxes: smallvec::SmallVec<[Syntax; 1]>,
    // fixme: we could easily do something like `Option<Box<String>>`
    // to save on stack space since `Expected` is already quite large.
    notes: Vec<Box<dyn DisplayDebug>>,
}

impl From<Expected> for ParserPos {
    #[inline]
    fn from(value: Expected) -> Self {
        value.pos
    }
}

impl ToSpan for Expected {
    #[inline]
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
            syntaxes: smallvec::SmallVec::new(),
            notes: Vec::new(),
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
        Self::nothing(pos).or_lit(lit)
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
        Self::nothing(pos).or_noun(noun)
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
        self.syntaxes.push(Syntax::lit(lit.into()));
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
        self.syntaxes.push(Syntax::noun(noun.into()));
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

    /// Restores the state of the parser to the [position](ParserPos)
    /// just before the error was encountered.
    ///
    /// Functionally parallel to [`Parser::restore`] and [`Parser::seek_to`].
    #[inline]
    pub fn recover(&self, cx: &mut Parser) {
        cx.seek_to(&self.pos)
    }

    #[inline]
    pub fn add_note(&mut self, note: impl fmt::Display + fmt::Debug + 'static) {
        self.notes.push(Box::new(note));
    }

    #[inline]
    pub fn with_note(mut self, note: impl fmt::Display + fmt::Debug + 'static) -> Self {
        self.add_note(note);
        self
    }
}

impl fmt::Display for Expected {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "expected ")?;

        if let Some((last, rest)) = self.syntaxes.split_last() {
            for syntax in rest {
                write!(f, "{syntax}, ")?;
            }

            if self.syntaxes.len() > 1 {
                write!(f, "or ")?;
            }
            write!(f, "{last}")?;

            if self.pos.is_eos() {
                write!(f, ", but found the end of input")?;
            }
        } else {
            write!(f, "no tokens")?;
        }

        for note in &self.notes {
            write!(f, "\nnote: {note}")?;
        }

        Ok(())
    }
}

trait DisplayDebug: fmt::Debug + fmt::Display {}
impl<T: fmt::Debug + fmt::Display> DisplayDebug for T {}

#[derive(Debug)]
struct CustomDiagnostic {
    level: DiagnosticLevel,
    span: Span,
    msg: Box<dyn DisplayDebug>,
}

impl PartialEq for CustomDiagnostic {
    fn eq(&self, other: &Self) -> bool {
        self.level == other.level
    }
}

#[derive(Debug)]
enum DiagnosticKind {
    Expected(Expected),
    Custom(CustomDiagnostic),
    Join(Vec<DiagnosticKind>),
}

impl PartialEq for DiagnosticKind {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (DiagnosticKind::Expected(a), DiagnosticKind::Expected(b)) => a == b,
            (DiagnosticKind::Join(a), DiagnosticKind::Join(b)) => a == b,
            // explicitly return false in this case, we don't have `PartialEq` for the dynamic error.
            (DiagnosticKind::Custom(a), DiagnosticKind::Custom(b)) => a == b,
            _ => false,
        }
    }
}

/// The universal type used for parsing diagnostics.
///
/// The `Diagnostic` type contains one or more errors, warnings,
/// or other pieces of reportable information encountered during parsing.
///
/// ## Construction
///
/// Diagnostic errors representing expected syntax elements can be constructed
/// using the [`Expected`] type, and this type's corresponding `From<Expected>` implementation.
/// See their docs for more information.
///
/// ## Composition
///
/// For several variants of expected, but not found syntax over the same span,
/// [`Expected`] can be composed with [`Expected::or_lit`] and [`Expected::or_noun`].
///
/// Discrete diagnostics can be combined into a single `Diagnostic` value
/// using [`Diagnostic::and`] and [`Diagnostic::and_many`].
///
/// ## Reporting
///
/// To emit accumulated diagnostics at runtime, [`Diagnostic::emit`] and [`Diagnostic::emit_many`]
/// return `TokenStream`s which compile to a series of invocations of the [`compile_error`] macro.
///
/// **NB:** The following is subject to change:
///
/// In stable Rust (as of version 1.80),
/// there is no built-in support for emitting diagnostics other than compile errors.
/// However, by enabling the `"warnings"` feature, `vermouth` will provide
/// best effort support for custom [warnings] by carefully emitting `#[must_use]` attributes.
///
/// If using the nightly toolchain, enabling the `"unstable-diagnostics-backend"` feature
/// will use unstable features of the `proc_macro` crate to emit higher-quality diagnostics.
/// This unstable feature is compatible with but does not imply the `"warnings"` feature,
/// and is strictly incompatible with the `"proc-macro2"` feature.
///
/// [warnings]: DiagnosticLevel::Warning
#[derive(Debug, PartialEq)]
pub struct Diagnostic {
    kind: DiagnosticKind,
}

impl From<Expected> for Diagnostic {
    #[inline]
    fn from(value: Expected) -> Self {
        Self {
            kind: DiagnosticKind::Expected(value),
        }
    }
}

/// The severity level of a [custom `Diagnostic`].
///
/// See [the documentation for `Diagnostic`] for the
/// semantics of each variant here.
///
/// [custom `Diagnostic`]: Diagnostic::custom
///
/// [the documentation for `Diagnostic`]: Diagnostic#reporting
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[non_exhaustive]
pub enum DiagnosticLevel {
    /// An infallible compilation error.
    Error,
    /// A warning, which can be suppressed.
    #[cfg(feature = "warnings")]
    Warning,
}

impl Diagnostic {
    /// Creates a [`Diagnostic`] with a customizable message.
    ///
    /// # Reporting
    ///
    /// Diagnostics made with this constructor transparently report the message
    /// exactly as it is passed.
    #[inline]
    pub fn custom(
        level: DiagnosticLevel,
        span: impl ToSpan,
        msg: impl fmt::Display + fmt::Debug + 'static,
    ) -> Diagnostic {
        Self {
            kind: DiagnosticKind::Custom(CustomDiagnostic {
                level,
                span: span.span(),
                msg: Box::new(msg),
            }),
        }
    }

    /// Combines several errors in a single object.
    ///
    /// # Reporting
    ///
    /// Each error passed to this constructor
    /// generates a separate invocation of [`compile_error`].
    pub fn and_many(errors: impl IntoIterator<Item = Diagnostic>) -> Diagnostic {
        let errors = errors.into_iter();
        let mut buf = Vec::with_capacity(errors.size_hint().0);

        // note we do not need to recursively flatten `ErrorKind::Join`s,
        // because we ensure they are shallow on construction.
        errors.for_each(|err| Self::extend(&mut buf, err.kind));

        Self {
            kind: DiagnosticKind::Join(buf),
        }
    }

    #[inline]
    fn extend(buf: &mut Vec<DiagnosticKind>, err: DiagnosticKind) {
        match err {
            DiagnosticKind::Join(errors) => buf.extend(errors),
            _ => buf.push(err),
        }
    }

    /// Combines another error to this object.
    ///
    /// # Reporting
    ///
    /// The passed error is generated alongside those stored in `self`.
    pub fn and(&mut self, b: impl Into<Diagnostic>) {
        let (a, b) = (self, b.into());
        match (&mut a.kind, b.kind) {
            (DiagnosticKind::Join(a_buf), DiagnosticKind::Join(b_buf)) => {
                a_buf.extend(b_buf);
            }
            (DiagnosticKind::Join(a_buf), b_kind) => {
                Self::extend(a_buf, b_kind);
            }
            (_, DiagnosticKind::Join(b_buf)) => {
                let old_a = replace(
                    &mut a.kind,
                    DiagnosticKind::Join(Vec::with_capacity(b_buf.len() + 1)),
                );
                let DiagnosticKind::Join(a_buf) = &mut a.kind else {
                    // we just made `a` an `Error::Or`.
                    unreachable!()
                };

                Self::extend(a_buf, old_a);
                a_buf.extend(b_buf);
            }
            (_, b_kind) => {
                let old_a = replace(&mut a.kind, DiagnosticKind::Join(Vec::with_capacity(2)));
                let DiagnosticKind::Join(a_buf) = &mut a.kind else {
                    // we just made `a` an `Error::Or`.
                    unreachable!()
                };

                a_buf.extend([old_a, b_kind]);
            }
        }
    }

    /// Emits this diagnostic.
    ///
    /// Depending on the feature configuration and execution context,
    /// some errors may be reported immediately
    /// and some may be contained in the retuned [`TokenStream`].
    #[must_use = "reported diagnostics should be returned from proc macros"]
    #[inline]
    pub fn emit(self) -> TokenStream {
        Self::emit_many(Some(self))
    }

    /// Emits a sequence of diagnostics.
    ///
    /// See [`Diagnostic::emit`] for a detailed description.
    #[must_use = "reported diagnostics should be returned from proc macros"]
    #[inline]
    pub fn emit_many(ds: impl IntoIterator<Item = Self>) -> TokenStream {
        let mut emitter = emit::EmitState::new();
        for d in ds {
            d.kind.emit(&mut emitter);
        }
        emitter.finish()
    }
}

impl DiagnosticKind {
    fn emit(self, emitter: &mut impl Emitter) {
        match self {
            DiagnosticKind::Expected(exp) => {
                emitter.emit(DiagnosticLevel::Error, exp.pos.span(), &exp.to_string())
            }
            DiagnosticKind::Custom(custom) => {
                emitter.emit(custom.level, custom.span, &custom.msg.to_string())
            }
            DiagnosticKind::Join(errors) => {
                for err in errors {
                    err.emit(emitter);
                }
            }
        }
    }
}
