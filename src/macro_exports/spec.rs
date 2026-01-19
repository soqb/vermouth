//! Macro type-specialization implementations.

use std::marker::PhantomData;

use crate::{IntoTokens, TokenQueue, ඞ_macro_exports::Verbatim};

use super::{
    VerbatimKind,
    lit::{LazyLiteral, LitContents},
};

pub struct Spec<T>(PhantomData<T>);

impl<T> Spec<T> {
    #[inline(always)]
    pub fn empty() -> Spec<T> {
        Spec(PhantomData)
    }
    #[inline(always)]
    pub fn new(_: *const T) -> Spec<T> {
        Spec::empty()
    }
}

/// Autoref specializer for literal types.
pub trait SpecLiteralQuote: Sized {
    type Datum;
    fn ඞ_lit_quote<const REGIME: u8>(
        self,
        datum: &Self::Datum,
        text: &'static str,
        q: &mut TokenQueue,
    );
}

impl<T: LitContents> SpecLiteralQuote for Spec<T> {
    type Datum = T;

    #[inline(always)]
    fn ඞ_lit_quote<const REGIME: u8>(self, &datum: &T, text: &'static str, q: &mut TokenQueue) {
        match LazyLiteral::<T, REGIME>::new(datum) {
            Some(l) => l.extend_tokens(q),
            None => q.push(Verbatim::new(text, VerbatimKind::Literal)),
        }
    }
}

/// Not even a recognised literal *type*, just go to fallback.
impl<T> SpecLiteralQuote for &Spec<T> {
    type Datum = T;

    #[inline(always)]
    fn ඞ_lit_quote<const REGIME: u8>(self, _: &T, text: &'static str, q: &mut TokenQueue) {
        q.push(Verbatim::new(text, VerbatimKind::Literal))
    }
}
