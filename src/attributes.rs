//! Provides utilities for parsing attributes.

use proc_macro::{Delimiter, Group, Punct, Spacing, Span, TokenStream, TokenTree};

use crate::{
    Error, Expected, Finish, Parse, Parser, Result, Spanned, ToSpan, ToTokens, TokenTreeExt,
    TokensExtend,
};

/// Represents the contents of an attribute which may be [`cfg`] or [`cfg_attr`].
///
/// [`cfg`]: https://doc.rust-lang.org/nightly/reference/conditional-compilation.html#the-cfg-attribute
/// [`cfg_attr`]: https://doc.rust-lang.org/nightly/reference/conditional-compilation.html#the-cfg_attr-attribute
pub enum Cfgable<T> {
    Attr(T),
    Cfg { meta: TokenStream },
    CfgAttr { meta: TokenStream, inner: Box<Self> },
}

impl<T: Parse> Parse for Cfgable<T>
where
    for<'a> T::Args<'a>: Clone,
{
    type Args<'a> = T::Args<'a>;

    fn parse_with(cx: &mut Parser, args: Self::Args<'_>) -> Result<Self> {
        let mut exp = match T::parse_with(cx, args.clone()) {
            Ok(attr) => return Ok(Self::Attr(attr)),
            Err(err) => err,
        };

        let ident = Spanned::from(cx.eat_ident()?);
        let ident = ident.as_deref();

        if (ident == "cfg")
            .then_some(())
            .map(|_| true)
            .unwrap_or_else(|| {
                exp.push_lit("cfg");
                false
            })
        {
            let ref mut cx: Parser = cx.eat(Delimiter::Parenthesis)?.into();
            Ok(Cfgable::Cfg { meta: cx.rest() })
        } else if (ident == "cfg_attr")
            .then_some(())
            .map(|_| true)
            .unwrap_or_else(|| {
                exp.push_lit("cfg_attr");
                false
            })
        {
            let ref mut cx: Parser = cx.eat(Delimiter::Parenthesis)?.into();

            let meta = cx.collect_until(
                |tok| tok.is_punct(','),
                |_| Ok(Finish::Void),
                |span| Err(Expected::lit(span, ",").into()),
            )?;
            let inner = Self::parse_with(cx, args)?;

            Ok(Cfgable::CfgAttr {
                meta,
                inner: Box::new(inner),
            })
        } else {
            Err(exp)
        }
    }
}

/// [Attribute syntax], either `#[foo]` or `#![bar]`.
///
/// Parsing the contents of the attribute (the `foo` or `bar` above) is performed by this type's type parameter.
///
/// **NB:** Doc comments, both `/// foo` and `//! bar`
/// are transformed into `#[doc = "foo"]` and `#![doc = "bar"]` attributes (respectively),
/// in the earliest phases of compilation, so they are unconditionally parsed by this type.
///
/// Typically the contents are a [meta item], but that is not enforced in the contract of this type.
///
/// [Attribute syntax]: https://doc.rust-lang.org/nightly/reference/attributes.html
/// [meta item]: https://doc.rust-lang.org/nightly/reference/attributes.html#meta-item-attribute-syntax
pub enum Attribute<O, I> {
    /// An outer attribute like `#[foo]`.
    Outer { contents: O },
    /// An inner attribute like `#![foo]`.
    Inner { contents: I, bang: Punct },
}

impl<O, I> Attribute<O, I> {
    /// Unwraps outer attributes and errors on inner attributes.
    pub fn reject_inner(self) -> Result<O, Error> {
        match self {
            Attribute::Outer { contents } => Ok(contents),
            Attribute::Inner { bang, .. } => Err(Error::custom(
                bang.span(),
                "inner attributes (such as `#![foo]`) are not permitted in this context",
            )),
        }
    }

    /// Parses an attribute according to the specification provided.
    fn parse_separately<A>(
        cx: &mut Parser,
        args: A,
        parse_outer: impl FnOnce(&mut Parser, A) -> Result<O>,
        parse_inner: impl FnOnce(&mut Parser, A) -> Result<I>,
    ) -> Result<Attribute<O, I>> {
        cx.eat_expectantly(
            |tok| tok.is_punct('#').then_some(()),
            |span| Expected::lit(span, "an attribute"),
        )?;

        enum Kind {
            Outer,
            Inner(Punct),
        }

        let kind = cx.eat_expectantly(
            |tok| match tok {
                TokenTree::Group(_) => Some(Kind::Outer),
                TokenTree::Punct(punct) if punct.as_char() == '!' => Some(Kind::Inner(punct)),
                _ => None,
            },
            |span| Expected::lit(span, "!").or_noun("square brackets"),
        )?;

        if let Kind::Outer = &kind {
            cx.gag(1);
        }

        let group = cx.eat(Delimiter::Bracket)?;
        let ref mut cx = Parser::from(group);

        match kind {
            Kind::Outer => parse_outer(cx, args).map(|contents| Attribute::Outer { contents }),
            Kind::Inner(bang) => {
                parse_inner(cx, args).map(|contents| Attribute::Inner { contents, bang })
            }
        }
    }
}

impl<O, I> Parse for Attribute<O, I>
where
    O: Parse,
    I: for<'a> Parse<Args<'a> = O::Args<'a>>,
{
    type Args<'a> = O::Args<'a>;

    fn parse_with(cx: &mut Parser, args: Self::Args<'_>) -> Result<Self> {
        Self::parse_separately(cx, args, O::parse_with, I::parse_with)
    }
}

impl<O: ToSpan, I: ToSpan> ToSpan for Attribute<O, I> {
    fn span(&self) -> Span {
        match self {
            Attribute::Outer { contents } => contents.span(),
            Attribute::Inner { contents, .. } => contents.span(),
        }
    }
}

impl<O: ToTokens, I: ToTokens> ToTokens for Attribute<O, I> {
    fn extend_tokens(&self, buf: &mut TokenStream) {
        buf.push(Punct::new('#', Spacing::Joint));
        let group = match self {
            Attribute::Outer { contents } => contents.to_tokens(),
            Attribute::Inner { bang, contents } => {
                buf.push(bang.clone());
                contents.to_tokens()
            }
        };
        buf.push(Group::new(Delimiter::Bracket, group));
    }
}
