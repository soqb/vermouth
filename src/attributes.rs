//! Provides utilities for parsing attributes.

use proc_macro::{
    Delimiter, Ident, TokenStream,
    TokenTree::{Group, Punct},
};

use crate::{expect_str, Error, Finish, Parse, Parser, Result, Spanned, TokenTreeExt};

/// Represents a [meta item] which may [`cfg`] or [`cfg_attr`].
///
/// [meta item]: https://doc.rust-lang.org/nightly/reference/attributes.html#meta-item-attribute-syntax
/// [`cfg`]: https://doc.rust-lang.org/nightly/reference/conditional-compilation.html#the-cfg-attribute
/// [`cfg_attr`]: https://doc.rust-lang.org/nightly/reference/conditional-compilation.html#the-cfg_attr-attribute
pub enum Cfgable<T> {
    Attr(T),
    Cfg { meta: TokenStream },
    CfgAttr { meta: TokenStream, inner: Box<Self> },
}

impl<T: Parse> Parse for Cfgable<T> {
    fn parse(cx: &mut Parser) -> Result<Self> {
        let mut err = match T::parse(cx) {
            Ok(attr) => return Ok(Self::Attr(attr)),
            Err(err) => err,
        };

        let ident = Spanned::from(Ident::parse(cx)?);
        let ident = ident.as_deref();

        if err.try_join_ok(expect_str(ident, "cfg")) {
            let ref mut cx: Parser = cx.delimited_by(Delimiter::Parenthesis)?.into();
            Ok(Cfgable::Cfg { meta: cx.rest() })
        } else if err.try_join_ok(expect_str(ident, "cfg_attr")) {
            let ref mut cx: Parser = cx.delimited_by(Delimiter::Parenthesis)?.into();

            let meta = cx.collect_until(
                |tok| tok.is_punct(','),
                |_| Ok(Finish::Void),
                |span| Err(Error::from_expected_lit(span, ",")),
            )?;
            let inner: Self = cx.parse()?;

            Ok(Cfgable::CfgAttr {
                meta,
                inner: Box::new(inner),
            })
        } else {
            Err(err)
        }
    }
}

/// [Attribute syntax], either `#[foo]` or `#![bar]`.
///
/// Parsing the contents of the attribute is performed by this type's type parameter.
///
/// **NB:** Doc comments, both `/// foo` and `//! bar`,
/// are transformed into `#[doc = "foo"]` and `#![doc = "bar"]` attributes (respectively),
/// in the earliest phases of compilation, so they can be parsed by this type.
///
/// [Attribute syntax]: https://doc.rust-lang.org/nightly/reference/attributes.html
pub struct Attribute<T> {
    /// The contents of the attribute.
    ///
    /// Typically, this is a [meta item], but that is not enforced.
    ///
    /// [meta item]: https://doc.rust-lang.org/nightly/reference/attributes.html#meta-item-attribute-syntax
    pub contents: T,
    /// The [kind](AttributeKind) of this attribute.
    pub kind: AttributeKind,
}

/// Whether an attribute is [outer](Self::Outer) or [inner](Self::Inner).
pub enum AttributeKind {
    /// An outer attribute, like `#[foo]`.
    Outer,
    /// An inner attribute, like `#![bar]`.
    Inner,
}

impl<T: Parse> Parse for Attribute<T> {
    fn parse(cx: &mut Parser) -> Result<Self> {
        cx.eat_tentatively(
            |tok| tok.is_punct('#').then_some(()),
            |span| Error::from_expected_noun(span, "an attribute"),
        )?;

        let (kind, group) = cx.eat_tentatively(
            |tok| match tok {
                Group(group) => Some((AttributeKind::Outer, Some(group.clone()))),
                Punct(punct) if punct.as_char() == '!' => Some((AttributeKind::Inner, None)),
                _ => None,
            },
            |span| Error::from_expected_noun(span, "`!` or brackets"),
        )?;

        let group = group
            .ok_or(())
            .or_else(|_| cx.delimited_by(Delimiter::Bracket))?;

        let ref mut cx = Parser::from(group);
        cx.parse().map(|contents| Attribute { contents, kind })
    }
}
