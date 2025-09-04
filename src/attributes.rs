//! Provides utilities for parsing attributes.

use std::marker::PhantomData;

use proc_macro::{Delimiter, Punct, Span, TokenStream, TokenTree};

use crate::{
    Diagnostic, DiagnosticLevel, Eos, Expected, Parse, Parser, Result, ToSpan, ToTokens,
    TokenTreeExt, extend_quote, quote, quote_fn,
};

/// An attribute which may be [`cfg`].
///
/// In order to support `cfg_attr`, wrap this in [`Cfgable`].
///
/// [`cfg`]: https://doc.rust-lang.org/nightly/reference/conditional-compilation.html#the-cfg-attribute
pub enum CfgLeaf<T> {
    /// Some specific non-`cfg` syntax.
    Other(T),
    /// `cfg`.
    Cfg { meta: TokenStream },
}

impl<T: Parse> Parse for CfgLeaf<T> {
    type Args<'a> = T::Args<'a>;

    fn parse_with(cx: &mut Parser, args: T::Args<'_>) -> Result<Self> {
        if cx.eat_ident()?.to_string() == "cfg" {
            let meta = cx.eat(Delimiter::Parenthesis)?.stream();
            Ok(CfgLeaf::Cfg { meta })
        } else {
            cx.gag(1);
            T::parse_with(cx, args)
                .map(CfgLeaf::Other)
                .map_err(|exp| exp.or_lit("cfg"))
        }
    }
}

impl<T: ToTokens> ToTokens for CfgLeaf<T> {
    fn extend_tokens(&self, mut buf: &mut TokenStream) {
        match self {
            CfgLeaf::Cfg { meta } => extend_quote!(buf <- { cfg(@meta) }),
            CfgLeaf::Other(c) => c.extend_tokens(buf),
        }
    }
}

/// Represents the contents of an attribute which may be a neeply nested [`cfg_attr`].
///
/// [`cfg_attr`]: https://doc.rust-lang.org/nightly/reference/conditional-compilation.html#the-cfg_attr-attribute
pub struct Cfgable<T> {
    // FIXME: no evidence this architecture is better than the naive (recursive) approach.
    //     it was lowkey fun though.
    cfg_attr_metas: Vec<TokenStream>,
    pub inner: T,
}

impl<T: Parse> Parse for Cfgable<T> {
    type Args<'a> = T::Args<'a>;

    fn parse_with(cx: &mut Parser, args: T::Args<'_>) -> Result<Self> {
        let mut cfg_attr_metas = Vec::new();
        // sometimes borrowck just needs a little helping hand.
        // by reborrowing `cx`, we can ensure the shadowing variable can borrow from `cx_store`.
        let mut cx_store;
        let mut cx = cx;
        let inner = loop {
            if cx.eat_ident()?.to_string() == "cfg_attr" {
                cx_store = Parser::from(cx.eat(Delimiter::Parenthesis)?);
                cx = &mut cx_store;

                let meta = cx.collect_until(|tok| tok.is_punct(','));
                if let Ok(pos) = cx.eat(Eos) {
                    return Err(Expected::lit(pos, ","));
                }

                cfg_attr_metas.push(meta);
            } else {
                cx.gag(1);
                break T::parse_with(cx, args).map_err(|exp| exp.or_lit("cfg_attr"))?;
            }
        };

        Ok(Cfgable {
            cfg_attr_metas,
            inner,
        })
    }
}

fn cfgable_extend_tokens(metas: &[TokenStream], inner: &impl ToTokens, mut buf: &mut TokenStream) {
    // we iterate in reverse, building up everything that `cfg_attr` parameterises in a single step.
    let Some((last_meta, rest)) = metas.split_last() else {
        inner.extend_tokens(buf);
        return;
    };

    let mut args = quote! { @last_meta, @inner };

    for meta in rest.iter().rev() {
        args = quote! { @meta, cfg_attr(@args) }
    }

    extend_quote!(buf <- { cfg_attr(@args) });
}

impl<T> Cfgable<T> {
    pub fn extend_tokens_as_cfg(&self, buf: &mut TokenStream) {
        let Some((last, rest)) = self.cfg_attr_metas.split_last() else {
            return;
        };

        cfgable_extend_tokens(&rest, &quote_fn! { cfg(@last) }, buf);
    }

    /// Reparameterises a `cfg_attr` attribute into a `cfg`.
    ///
    /// For example, `cfg_attr(foo, cfg_attr(bar, baz))` becomes `cfg_attr(foo, cfg(bar))`.
    pub fn to_tokens_as_cfg(&self) -> TokenStream {
        let mut buf = TokenStream::new();
        self.extend_tokens_as_cfg(&mut buf);
        buf
    }
}

impl<T: ToTokens> ToTokens for Cfgable<T> {
    fn extend_tokens(&self, buf: &mut TokenStream) {
        cfgable_extend_tokens(&self.cfg_attr_metas, &self.inner, buf);
    }
}

/// [Attribute syntax], either `#[foo]` or `#![bar]`.
///
/// Parsing the contents of the attribute (the `foo` or `bar` above)
/// is performed by this type's type parameters.
///
/// **NB:** Doc comments, both `/// foo` and `//! bar`,
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
    pub fn reject_inner(self) -> Result<O, Diagnostic> {
        match self {
            Attribute::Outer { contents } => Ok(contents),
            Attribute::Inner { bang, .. } => Err(Diagnostic::custom(
                DiagnosticLevel::Error,
                bang.span(),
                "inner attributes (such as `#![foo]`) are not permitted in this context",
            )),
        }
    }

    /// Parses an attribute with the provided inner and outer attribute parsers.
    fn parse_separately<A>(
        cx: &mut Parser,
        args: A,
        parse_outer: impl FnOnce(&mut Parser, A) -> Result<O>,
        parse_inner: impl FnOnce(&mut Parser, A) -> Result<I>,
    ) -> Result<Attribute<O, I>> {
        cx.eat_expectantly(
            |tok| tok.is_punct('#').then_some(()),
            |pos| Expected::noun(pos, "an attribute"),
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
            |pos| Expected::lit(pos, "!").or_noun("square brackets"),
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

pub struct Attrs<'a, O, I, A, Fo, Fi> {
    cx: &'a mut Parser,
    args: A,
    fo: Fo,
    fi: Fi,
    _marker: PhantomData<fn(O, I) -> (O, I)>,
}

impl<'a, O, I, A: Clone, Fo, Fi> Attrs<'a, O, I, A, Fo, Fi>
where
    Fo: FnMut(&mut Parser, A) -> Result<O>,
    Fi: FnMut(&mut Parser, A) -> Result<I>,
{
    pub fn parse_many_separately(
        cx: &'a mut Parser,
        args: A,
        parse_outer: Fo,
        parse_inner: Fi,
    ) -> Attrs<'a, O, I, A, Fo, Fi> {
        Attrs {
            cx,
            args,
            fo: parse_outer,
            fi: parse_inner,
            _marker: PhantomData,
        }
    }
}

// without TAIT, not possible to make this an instance method.
pub fn parse_many_attributes_with<'a, O, I>(
    cx: &'a mut Parser,
    args: O::Args<'a>,
) -> Attrs<
    'a,
    O,
    I,
    O::Args<'a>,
    impl FnMut(&mut Parser, O::Args<'a>) -> Result<O>,
    impl FnMut(&mut Parser, O::Args<'a>) -> Result<I>,
>
where
    O: Parse,
    I: for<'b> Parse<Args<'b> = O::Args<'b>>,
    for<'b> O::Args<'b>: Clone,
{
    Attrs::parse_many_separately(cx, args, O::parse_with, I::parse_with)
}

pub fn parse_many_attributes<'a, O, I>(
    cx: &'a mut Parser,
) -> Attrs<
    'a,
    O,
    I,
    O::Args<'a>,
    impl FnMut(&mut Parser, O::Args<'a>) -> Result<O>,
    impl FnMut(&mut Parser, O::Args<'a>) -> Result<I>,
>
where
    O: Parse,
    I: for<'b> Parse<Args<'b> = O::Args<'b>>,
    for<'b> O::Args<'b>: Clone + Default,
{
    parse_many_attributes_with(cx, O::Args::default())
}

// type F<A, T> = fn(&mut Parser, A) -> Result<T>;
// impl<'a, O, I, A: Clone> Attrs<'a, O, I, A, F<A, O>, F<A, I>>
// where
//     O: for<'b> Parse<Args<'b> = A>,
//     I: for<'b> Parse<Args<'b> = A>,
// {
//     // pub fn parse_many_with(
//     //     cx: &'a mut Parser,
//     //     args: A,
//     // ) -> Attrs<'a, O, I, A, F<A, O>, F<A, I>> {
//     //     let f: F<A, O> = O::parse_with(parser, args)
//     // }
// }

impl<'a, O, I, A: Clone, Fo, Fi> Attrs<'a, O, I, A, Fo, Fi>
where
    Fo: FnMut(&mut Parser, A) -> Result<O>,
    Fi: FnMut(&mut Parser, A) -> Result<I>,
{
}
impl<'a, O, I, A: Clone, Fo, Fi> Iterator for Attrs<'a, O, I, A, Fo, Fi>
where
    Fo: FnMut(&mut Parser, A) -> Result<O>,
    Fi: FnMut(&mut Parser, A) -> Result<I>,
{
    type Item = Attribute<O, I>;

    fn next(&mut self) -> Option<Attribute<O, I>> {
        let ck = self.cx.save();
        match Attribute::parse_separately(self.cx, self.args.clone(), &mut self.fo, &mut self.fi) {
            Ok(attr) => Some(attr),
            Err(_) => {
                self.cx.restore(&ck);
                None
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
    fn extend_tokens(&self, mut buf: &mut TokenStream) {
        match self {
            Attribute::Outer { contents } => extend_quote!(buf <- {
                #[@contents]
            }),
            Attribute::Inner { bang, contents } => extend_quote!(buf <- {
                #@bang[@contents]
            }),
        };
    }
}

// pub fn parse_and_fold_attributes_separately<O, I, A: Clone, Bo, Bi>(
//     cx: &mut Parser,
//     args: A,
//     mut parse_outer: impl FnMut(&mut Parser, A) -> Result<O>,
//     mut parse_inner: impl FnMut(&mut Parser, A) -> Result<I>,
//     mut fold_outer: impl FnMut(Bo, O) -> Bo,
//     mut fold_inner: impl FnMut(Bi, I) -> Bi,
//     mut outer_init: Bo,
//     mut inner_init: Bi,
// ) -> (Bo, Bi) {
//     loop {
//         match Attribute::parse_separately(cx, args.clone(), &mut parse_outer, &mut parse_inner) {
//             Ok(Attribute::Outer { contents }) => outer_init = fold_outer(outer_init, contents),
//             Ok(Attribute::Inner { contents, bang: _ }) => {
//                 inner_init = fold_inner(inner_init, contents)
//             }
//             Err(_) => return (outer_init, inner_init),
//         }
//     }
// }

// pub fn parse_and_collect_attributes_with<O, I, Co, Ci>(
//     cx: &mut Parser,
//     args: O::Args<'_>,
// ) -> (Co, Ci)
// where
//     Co: Default + Extend<O>,
//     Ci: Default + Extend<I>,
//     O: Parse,
//     I: for<'a> Parse<Args<'a> = O::Args<'a>>,
//     for<'a> O::Args<'a>: Clone,
// {
//     parse_and_fold_attributes_separately(cx, args, O::parse_with, I::parse_with)
// }

// pub fn parse_and_collect_attributes<O, I, Co, Ci>(cx: &mut Parser) -> (Co, Ci)
// where
//     Co: Default + Extend<O>,
//     Ci: Default + Extend<I>,
//     O: Parse,
//     I: for<'a> Parse<Args<'a> = O::Args<'a>>,
//     for<'a> O::Args<'a>: Default + Clone,
// {
//     parse_and_collect_attributes_with(cx, O::Args::default())
// }
