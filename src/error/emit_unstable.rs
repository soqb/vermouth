use proc_macro::{Diagnostic, Level};

use crate::{DiagnosticLevel, ToSpan, TokenQueue};

pub struct EmitState {
    _marker: (),
}

impl super::Emitter for EmitState {
    fn new() -> Self {
        EmitState { _marker: () }
    }
    fn emit(&mut self, level: DiagnosticLevel, span: impl ToSpan, msg: &impl ToString) {
        let level = match level {
            DiagnosticLevel::Error => Level::Error,
            #[cfg(feature = "warnings")]
            DiagnosticLevel::Warning => Level::Warning,
        };
        Diagnostic::spanned(span.span(), level, msg.to_string()).emit()
    }

    fn finish(self) -> TokenQueue {
        TokenQueue::new()
    }
}
