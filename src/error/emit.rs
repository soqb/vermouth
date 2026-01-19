use proc_macro::Span;

use crate::{DiagnosticLevel, ToSpan, TokenQueue};

#[cfg(feature = "warnings")]
fn emit_warning(buf: &mut TokenQueue, span: Span, mut msg: String) {
    use proc_macro::{Delimiter, Group, Ident, Literal, Punct, Spacing, Span, TokenStream};
    fn in_const_block(q: &mut TokenQueue, f: impl FnOnce(&mut TokenQueue)) {
        q.push(Ident::new("const", Span::call_site()));
        q.push(Ident::new("_", Span::call_site()));
        q.push(Punct::new(':', Spacing::Alone));
        q.push(Group::new(Delimiter::Parenthesis, TokenStream::new()));
        q.push(Punct::new('=', Spacing::Alone));

        let mut group = TokenQueue::new();
        f(&mut group);
        q.push(Group::new(Delimiter::Brace, group.into()));
        q.push(Punct::new(';', Spacing::Alone));
    }

    fn in_attr(q: &mut TokenQueue, f: impl FnOnce(&mut TokenQueue)) {
        q.push(Punct::new('#', Spacing::Alone));

        let mut group = TokenQueue::new();
        f(&mut group);
        q.push(Group::new(Delimiter::Bracket, group.into()));
    }

    in_const_block(buf, move |q| {
        in_attr(q, move |buf| {
            buf.push(Ident::new("must_use", span));
            buf.push(Punct::new('=', Spacing::Alone));

            msg.insert_str(0, "proc macro produced a warning: ");
            msg.push('\n');
            let mut lit = Literal::string(&msg);

            lit.set_span(span);
            buf.push(lit);
        });

        q.push(Ident::new("struct", span));
        q.push(Ident::new("Warning", span));
        q.push(Punct::new(';', Spacing::Alone));

        q.push(Ident::new("Warning", span));
        q.push(Punct::new(';', Spacing::Alone));
    })
}

pub(super) fn emit(q: &mut TokenQueue, level: DiagnosticLevel, span: Span, msg: String) {
    let (span, msg) = (span.span(), msg.to_string());
    match level {
        DiagnosticLevel::Error => super::emit_compile_error_invocation(q, span, msg),
        #[cfg(feature = "warnings")]
        DiagnosticLevel::Warning => emit_warning(q, span, msg),
    }
}
