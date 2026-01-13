//! Error handling for parsers. See [`Expected`] and [`Diagnostic`].

use core::fmt;
use std::{borrow::Cow, mem::replace};

use proc_macro::Span;

use crate::{IntoTokens, Parser, ParserPos, ToSpan, TokenQueue};

#[cfg_attr(feature = "unstable-diagnostics-backend", path = "emit_unstable.rs")]
mod emit;

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
    /// # let parser_pos = vermouth::ParserPos::arbitrary();
    /// let error = Expected::nothing(parser_pos);
    /// assert_eq!(error.to_string(), "expected no tokens");
    /// ```
    #[inline]
    #[must_use]
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
    /// # let parser_pos = vermouth::ParserPos::arbitrary();
    /// let error = Expected::lit(parser_pos, "foo");
    /// assert_eq!(error.to_string(), "expected `foo`");
    /// ```
    #[inline]
    #[must_use]
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
    /// # let parser_pos = vermouth::ParserPos::arbitrary();
    /// let error = Expected::nothing(parser_pos).or_lit("foo").or_noun("any bar");
    /// assert_eq!(error.to_string(), "expected `foo`, or any bar");
    /// ```
    #[inline]
    #[must_use]
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
    /// # let parser_pos = vermouth::ParserPos::arbitrary();
    /// let error = Expected::nothing(parser_pos).or_noun("some foo").or_lit("bar");
    /// assert_eq!(error.to_string(), "expected some foo, or `bar`");
    /// ```
    #[inline]
    #[must_use]
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

    /// Appends additional information to the diagnostic message.
    #[inline]
    pub fn add_note(&mut self, note: impl fmt::Display + fmt::Debug + 'static) {
        self.notes.push(Box::new(note));
    }

    /// Appends additional information to the diagnostic message.
    ///
    /// See [`Expected::add_note`].
    #[inline]
    #[must_use]
    pub fn with_note(mut self, note: impl fmt::Display + fmt::Debug + 'static) -> Self {
        self.add_note(note);
        self
    }

    // pub fn merge_from(&mut self)
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
/// To emit accumulated diagnostics at runtime, [`Diagnostic::finish`] and [`Parser::finish_diagnostics`]
/// return opaque `IntoTokens` invocations which evaluate to a series of invocations of the [`compile_error`] macro.
///
/// **NB:** The following is subject to change:
///
/// In stable Rust (as of version 1.92),
/// there is no built-in support for emitting diagnostics other than compile errors.
/// However, by enabling the `"warnings"` feature, `vermouth` will provide
/// best effort support for custom
#[cfg_attr(not(feature = "warnings"), doc = "warnings")]
#[cfg_attr(feature = "warnings", doc = "[warnings](DiagnosticLevel::Warning)")]
/// by carefully emitting `#[must_use]` attributes.
///
/// If using a nightly toolchain, enabling the
/// <a class="stab portability" href="index.html#feature-unstable-diagnostics-backend"><code>unstable-diagnostics-backend</code></a>
/// feature will use the experimental
/// <a class="stab portability" href="https://github.com/rust-lang/rust/issues/54140"><code>proc_macro_diagnostic</code></a>
/// feature of the `proc_macro` crate to emit higher-quality diagnostics.
/// This unstable feature is compatible with but does not imply the
/// <a class="stab portability" href="index.html#feature-warnings"><code>warnings</code></a> feature,
/// and is strictly incompatible with the
/// <a class="stab portability" href="index.html#feature-proc-macro2"><code>proc-macro2</code></a> feature.
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
        Diagnostic {
            kind: DiagnosticKind::Custom(CustomDiagnostic {
                level,
                span: span.span(),
                msg: Box::new(msg),
            }),
        }
    }

    /// Combines several diagnostics into one.
    ///
    /// # Reporting
    ///
    /// All diagnostic passed to this constructor
    /// are reported together.
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
    /// The passed diagnostic is emitted alongside those stored in `self`.
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

    /// Returns an opaque [`IntoTokens`] which lazily emits the error upon conversion.
    #[must_use = "accumulated errors must be returned from the proc-macro."]
    pub fn finish(self) -> impl IntoTokens {
        self.kind
    }
}
impl IntoTokens for DiagnosticKind {
    fn extend_tokens(self, q: &mut TokenQueue) {
        match self {
            DiagnosticKind::Expected(exp) => {
                emit::emit(q, DiagnosticLevel::Error, exp.pos.span(), &exp.to_string())
            }
            DiagnosticKind::Custom(custom) => {
                emit::emit(q, custom.level, custom.span, &custom.msg.to_string())
            }
            DiagnosticKind::Join(errors) => {
                for err in errors {
                    err.extend_tokens(q);
                }
            }
        }
    }
}
