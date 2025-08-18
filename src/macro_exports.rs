use proc_macro::{Delimiter, Group, Ident, Punct, Spacing, Span, TokenStream, TokenTree};

use crate::TokensExtend;

pub use proc_macro;
pub use std::iter::Extend as StdExtend;

pub fn assert_tokens_extend(_: &impl TokensExtend) {}

pub fn new_punct(c: char, spacing: Spacing) -> TokenTree {
    TokenTree::from(Punct::new(c, spacing))
}

pub fn new_group(stream: TokenStream, delimiter: Delimiter) -> TokenTree {
    TokenTree::from(Group::new(delimiter, stream))
}

pub fn new_ident(str: &'static str) -> TokenTree {
    if let Some(raw) = str.strip_prefix("r#") {
        TokenTree::from(Ident::new_raw(raw, Span::call_site()))
    } else {
        TokenTree::from(Ident::new(str, Span::call_site()))
    }
}

pub fn coerce_nested_trees<'a>(s: &'a [&'a [TokenTree]]) -> &'a [&'a [TokenTree]] {
    s
}
