/// A macro which emits different diagnostics at compile time.
///
/// ```compile_fail
/// // produces: `i say "oh! terrible, terrible!"`
/// toy::parrot_diagnostic!(error = "oh! terrible, terrible!");
/// ```
///
/// ```
/// // produces: `i say "its problematic, but recoverable"`
/// // through a hidden `#[must_use]` diagnostic.
/// #[expect(unused_must_use)]
/// toy::parrot_diagnostic!(warning = "its problematic, but recoverable");
/// ```
pub use toy_macros::parrot_diagnostic;
