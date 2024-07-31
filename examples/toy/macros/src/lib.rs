use proc_macro::{Span, TokenStream, TokenTree};
use vermouth::{Diagnostic, DiagnosticLevel, Expected, Parser};

fn parrot_diagnostic_impl(cx: &mut Parser) -> vermouth::Result<()> {
    let level = cx.eat_expectantly(
        |tt| match tt {
            TokenTree::Ident(id) => match id.to_string().as_str() {
                "warning" => Some(DiagnosticLevel::Warning),
                "error" => Some(DiagnosticLevel::Error),
                _ => None,
            },
            _ => None,
        },
        |pos| Expected::lit(pos, "warning").or_lit("error"),
    )?;

    cx.eat_punct('=')?;

    let lit = cx.eat_expectantly(
        |tt| match tt {
            TokenTree::Literal(lit) => Some(lit),
            _ => None,
        },
        |pos| Expected::noun(pos, "expected a string literal"),
    )?;

    cx.report(Diagnostic::custom(
        level,
        lit.span(),
        format!("i say {lit}"),
    ));

    Ok(())
}

#[proc_macro]
pub fn parrot_diagnostic(tokens: TokenStream) -> TokenStream {
    let ref mut cx = Parser::new(tokens, Span::call_site());
    let _ = parrot_diagnostic_impl(cx).map_err(|exp| cx.report(exp));
    cx.emit_diagnostics()
}
