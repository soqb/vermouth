use std::{
    borrow::Cow,
    fmt::{self, Write},
};

use proc_macro::Span;

use crate::{DiagnosticLevel, ToSpan, TokenQueue};

pub(super) fn emit(q: &mut TokenQueue, level: DiagnosticLevel, span: Span, msg: String) {
    #[allow(irrefutable_let_patterns)]
    if let DiagnosticLevel::Error = level {
        super::emit_compile_error_invocation(q, span, msg);
        return;
    }

    let (file, line, column) = (span.file(), span.line(), span.column());
    let level = level_str(level);
    let msg = msg.to_string();
    let msg2 = format!("{level}: {msg}\n  --> {file}:{line}:{column}");
    eprintln!(
        r#"{{"$message_type":"diagnostic","message":"{msg}","code":null,"level":"{level}","spans":[{span}],"children":[],"rendered":"{rendered}"}}"#,
        msg = json_escape(&msg),
        span = span_str(span.span()),
        rendered = json_escape(&msg2),
    );
}

fn span_str(span: Span) -> String {
    let file_name = span.file();
    let (line_start, line_end) = (span.start().line(), span.end().line());
    let (column_start, column_end) = (span.start().column(), span.end().column());
    format!(
        r#"{{"file_name":"{file_name}","byte_start":0,"byte_end":0,"line_start":{line_start},"line_end":{line_end},"column_start":{column_start},"column_end":{column_end},"is_primary":true,"text":[],"label":null,"suggested_replacement":null,"suggestion_applicability":null,"expansion":null}}"#
    )
}

fn level_str(level: DiagnosticLevel) -> &'static str {
    match level {
        DiagnosticLevel::Error => "error",
        #[cfg(feature = "warnings")]
        DiagnosticLevel::Warning => "warning",
    }
}

// NB: technically json supports newlines in strings (fun fact!) but obviously jsonl cannot.
fn json_escape(s: &str) -> Cow<'_, str> {
    fn needs_escape(c: char) -> bool {
        // see `char::is_control`.
        matches!(c, '\0'..='\x1f' | '\x7f'..='\u{9f}' | '"' | '\\')
    }

    enum Escape {
        None(char),
        Quote,
        Backslash,
        Backspace,
        Formfeed,
        Linefeed,
        CarriageReturn,
        Tab,
        Surrogate(u16),
    }

    impl Escape {
        fn parse(c: char) -> Escape {
            match c {
                '"' => Escape::Quote,
                '\\' => Escape::Backslash,
                '\x08' => Escape::Backspace,
                '\x0c' => Escape::Formfeed,
                '\n' => Escape::Linefeed,
                '\r' => Escape::CarriageReturn,
                '\t' => Escape::Tab,
                '\0'..='\x1f' | '\x7f'..='\u{9f}' => Escape::Surrogate(c as u32 as u16),
                _ => Escape::None(c),
            }
        }
    }

    impl fmt::Display for Escape {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            let s = match self {
                &Escape::None(x) => return f.write_char(x),
                &Escape::Surrogate(n) => return write!(f, r"\u{n:02x}"),
                Escape::Quote => r#"\""#,
                Escape::Backslash => r"\\",
                Escape::Backspace => r"\b",
                Escape::Formfeed => r"\f",
                Escape::Linefeed => r"\n",
                Escape::CarriageReturn => r"\b",
                Escape::Tab => r"\t",
            };
            f.write_str(s)
        }
    }

    if !s.chars().any(needs_escape) {
        Cow::Borrowed(s)
    } else {
        let mut s2 = String::with_capacity(s.len() + 1);
        for escape in s.chars().map(Escape::parse) {
            write!(s2, "{escape}").unwrap();
        }
        Cow::Owned(s2)
    }
}
