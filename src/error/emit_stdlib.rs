use proc_macro::{Diagnostic, Level};

use crate::{DiagnosticLevel, ToSpan, TokenQueue};

pub(super) fn emit(
    _q: &mut TokenQueue,
    level: DiagnosticLevel,
    span: impl ToSpan,
    msg: &impl ToString,
) {
    let (span, msg) = (span.span(), msg.to_string());
    let level = match level {
        DiagnosticLevel::Error => Level::Error,
        #[cfg(feature = "warnings")]
        DiagnosticLevel::Warning => Level::Warning,
    };
    Diagnostic::spanned(span, level, msg).emit()
}
