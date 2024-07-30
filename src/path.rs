use proc_macro::{
    Ident, Punct,
    Spacing::{Alone, Joint},
    Span, TokenStream,
};

use crate::{punct_pat, punctuated::Punctuated, Parse, Parser, Result, ToTokens, TokensExtend};

/// The separator used in paths, `::`.
pub struct PathSep {
    left: Span,
    right: Span,
}

impl Parse for PathSep {
    type Args<'a> = ();

    fn parse_with(parser: &mut Parser, _args: Self::Args<'_>) -> crate::Result<Self> {
        parser.eat(punct_pat!(::)).map(|[left, right]| Self {
            left: left.span(),
            right: right.span(),
        })
    }
}

impl ToTokens for PathSep {
    fn extend_tokens(&self, buf: &mut TokenStream) {
        let mut a = Punct::new(':', Joint);
        a.set_span(self.left);
        buf.push(a);

        let mut b = Punct::new(':', Alone);
        b.set_span(self.right);
        buf.push(b);
    }
}

/// Parses a [simple path]
///
/// [simple path]: https://doc.rust-lang.org/reference/paths.html#simple-paths
pub fn parse_simple_path(cx: &mut Parser) -> Result<Punctuated<Ident, PathSep>> {
    let mut p = Punctuated::new();
    if let Ok(sep) = cx.parse() {
        p.push_sep(sep);
    }

    p.push_value(cx.eat_ident().map_err(|exp| exp.or_lit(","))?);

    loop {
        match cx.parse() {
            Ok(sep) => p.push_sep(sep),
            Err(_) => {
                break;
            }
        }

        p.push_value(cx.eat_ident()?);
    }

    Ok(p)
}
