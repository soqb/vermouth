/// A macro which emits different diagnostics at compile time.
///
/// ```compile_fail
/// // produces: `i say "oh! terrible, terrible!"`
/// toy::parrot_diagnostic!(error = "oh! terrible, terrible!");
/// ```
///
/// ```compile_fail
/// #![forbid(unused_must_use)]
/// // produces: `i say "its problematic, but recoverable"`
/// // through a hidden `#[must_use]` diagnostic.
/// //
/// // without the leading `forbid` attribute,
/// // this would just be warning, not an error.
/// toy::parrot_diagnostic!(warning = "its problematic, but recoverable");
/// ```
pub use toy_macros::parrot_diagnostic;
