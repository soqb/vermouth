//! Macro type-specialization implementations.

use std::marker::PhantomData;

use crate::{TokenQueue, TryIntoTokens, TtResult};

use super::lit::{self, DelayedLiteral, LitContents};

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
        buf: &mut TokenQueue,
    ) -> TtResult<()>;
}

impl<T: LitContents> SpecLiteralQuote for Spec<T> {
    type Datum = T;

    #[inline(always)]
    fn ඞ_lit_quote<const REGIME: u8>(
        self,
        &datum: &T,
        text: &'static str,
        buf: &mut TokenQueue,
    ) -> TtResult<()> {
        match DelayedLiteral::<T, REGIME>::new(datum) {
            Some(l) => l.try_extend_tokens(buf),
            None => lit::fallback(text, buf),
        }
    }
}

/// Not even a recognised literal *type*, just go to fallback.
impl<T> SpecLiteralQuote for &Spec<T> {
    type Datum = T;

    #[inline(always)]
    fn ඞ_lit_quote<const REGIME: u8>(
        self,
        _: &T,
        text: &'static str,
        buf: &mut TokenQueue,
    ) -> TtResult<()> {
        lit::fallback(text, buf)
    }
}
