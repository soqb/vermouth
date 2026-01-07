use proc_macro::{
    Delimiter, Group, Ident, Literal, Punct,
    Spacing::{Alone, Joint},
    Span, TokenStream, TokenTree,
};

use crate::{DiagnosticLevel, ToSpan, TokenQueue};

fn emit_error(buf: &mut TokenQueue, span: Span, msg: String) {
    macro_rules! quote_path {
        ($buf:ident <-) => {};
        ($buf:ident <- :: $n:ident $(:: $r:ident)*) => {
            let mut p = Punct::new(':', Joint);
            p.set_span(span);
            $buf.push(p);
            let mut p = Punct::new(':', Alone);
            p.set_span(span);
            $buf.push(p);
            $buf.push(Ident::new(stringify!($n), span));

            // recurse
            quote_path!($buf <- $(:: $r)*)
        };
    }

    quote_path!(buf <- ::core::compile_error);

    buf.push(Punct::new('!', Alone));

    let mut msg: TokenTree = Literal::string(&msg).into();
    msg.set_span(span);

    let mut group = Group::new(Delimiter::Parenthesis, TokenStream::from_iter([msg]));
    group.set_span(span);
    buf.push(group);

    buf.push(Punct::new(';', Alone));
}

#[cfg(feature = "warnings")]
fn emit_warning(buf: &mut TokenQueue, span: Span, mut msg: String) {
    fn in_const_block(q: &mut TokenQueue, f: impl FnOnce(&mut TokenQueue)) {
        q.push(Ident::new("const", Span::call_site()));
        q.push(Ident::new("_", Span::call_site()));
        q.push(Punct::new(':', Alone));
        q.push(Group::new(Delimiter::Parenthesis, TokenStream::new()));
        q.push(Punct::new('=', Alone));

        let mut group = TokenQueue::new();
        f(&mut group);
        q.push(Group::new(Delimiter::Brace, group.into()));
        q.push(Punct::new(';', Alone));
    }

    fn in_attr(q: &mut TokenQueue, f: impl FnOnce(&mut TokenQueue)) {
        q.push(Punct::new('#', Alone));

        let mut group = TokenQueue::new();
        f(&mut group);
        q.push(Group::new(Delimiter::Bracket, group.into()));
    }

    in_const_block(buf, move |q| {
        in_attr(q, move |buf| {
            buf.push(Ident::new("must_use", span));
            buf.push(Punct::new('=', Alone));

            msg.insert_str(0, "proc macro produced a warning: ");
            msg.push('\n');
            let mut lit = Literal::string(&msg);

            lit.set_span(span);
            buf.push(lit);
        });

        q.push(Ident::new("struct", span));
        q.push(Ident::new("Warning", span));
        q.push(Punct::new(';', Alone));

        q.push(Ident::new("Warning", span));
        q.push(Punct::new(';', Alone));
    })
}

pub(super) fn emit(
    q: &mut TokenQueue,
    level: DiagnosticLevel,
    span: impl ToSpan,
    msg: &impl ToString,
) {
    let (span, msg) = (span.span(), msg.to_string());
    match level {
        DiagnosticLevel::Error => emit_error(q, span, msg),
        #[cfg(feature = "warnings")]
        DiagnosticLevel::Warning => emit_warning(q, span, msg),
    }
}
