use proc_macro::{
    Delimiter, Group, Ident, Literal, Punct,
    Spacing::{Alone, Joint},
    Span, TokenStream, TokenTree,
};

use crate::{DiagnosticLevel, ToSpan, TokensExtend};

pub struct EmitState {
    tokens: TokenStream,
}

fn emit_error(buf: &mut TokenStream, span: Span, msg: String) {
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

    let mut p = Punct::new('!', Alone);
    p.set_span(span);
    buf.push(p);

    let mut msg: TokenTree = Literal::string(&msg).into();
    msg.set_span(span);

    let mut group = Group::new(Delimiter::Parenthesis, TokenStream::from_iter([msg]));
    group.set_span(span);
    buf.push(group);

    let mut p = Punct::new(';', Alone);
    p.set_span(span);
    buf.push(p);
}

#[cfg(feature = "warnings-mock")]
fn emit_warning(buf: &mut TokenStream, span: Span, msg: String) {
    // we really need quote !!!
    fn in_const_block(buf: &mut TokenStream, f: impl FnOnce(&mut TokenStream)) {
        buf.push(Ident::new("const", Span::call_site()));
        buf.push(Punct::new('_', Alone));
        buf.push(Punct::new(':', Alone));
        buf.push(Punct::new('(', Alone));
        buf.push(Punct::new(')', Alone));
        buf.push(Punct::new('=', Alone));

        let mut group = TokenStream::new();
        f(&mut group);
        buf.push(Group::new(Delimiter::Brace, group));
        buf.push(Punct::new(';', Alone));
    }

    fn in_attr(buf: &mut TokenStream, f: impl FnOnce(&mut TokenStream)) {
        buf.push(Punct::new('#', Alone));

        let mut group = TokenStream::new();
        f(&mut group);
        buf.push(Group::new(Delimiter::Bracket, group));
    }

    in_const_block(buf, move |buf| {
        in_attr(buf, move |buf| {
            buf.push(Ident::new("must_use", span));
            buf.push(Punct::new('=', Alone));

            let mut lit = Literal::string(&msg);
            lit.set_span(span);
            buf.push(lit);
        });

        buf.push(Ident::new("struct", span));
        buf.push(Ident::new("MockWarning", span));
        buf.push(Punct::new(';', Alone));

        buf.push(Ident::new("MockWarning", span));
        buf.push(Punct::new(';', Alone));
    })
}

impl super::Emitter for EmitState {
    fn new() -> Self {
        EmitState {
            tokens: TokenStream::new(),
        }
    }
    fn emit(&mut self, level: DiagnosticLevel, span: impl ToSpan, msg: &impl ToString) {
        let (span, msg) = (span.span(), msg.to_string());
        match level {
            DiagnosticLevel::Error => emit_error(&mut self.tokens, span, msg),
            #[cfg(feature = "warnings-mock")]
            DiagnosticLevel::Warning => emit_warning(&mut self.tokens, span, msg),
        }
    }

    fn finish(self) -> TokenStream {
        self.tokens
    }
}
