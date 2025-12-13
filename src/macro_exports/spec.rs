//! Macro type-specialization implementations.

use std::{error::Error, marker::PhantomData};

use crate::{TokenBuf, TryToTokens, TtError, TtResult};

use super::lit::{self, DelayedLiteral, LitContents};

pub struct Spec<T>(PhantomData<T>);

impl<T> Spec<T> {
    pub fn empty() -> Spec<T> {
        Spec(PhantomData)
    }
    pub fn new(_: *const T) -> Spec<T> {
        Spec::empty()
    }
}

pub trait SpecMut: Sized {
    type Mut;
    fn consume_unreachable(self) -> Self::Mut {
        panic!()
    }
}

/// Autoref specializer for literal types.
pub trait SpecLiteralQuote: Sized {
    type Datum;
    fn ඞ_lit_quote<const REGIME: u8>(
        self,
        datum: &Self::Datum,
        text: &'static str,
        buf: &mut TokenBuf,
    ) -> TtResult<()>;
}

impl<T: LitContents> SpecLiteralQuote for Spec<T> {
    type Datum = T;

    #[inline]
    fn ඞ_lit_quote<const REGIME: u8>(
        self,
        &datum: &T,
        text: &'static str,
        buf: &mut TokenBuf,
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

    #[inline]
    fn ඞ_lit_quote<const REGIME: u8>(
        self,
        _: &T,
        text: &'static str,
        buf: &mut TokenBuf,
    ) -> TtResult<()> {
        lit::fallback(text, buf)
    }
}

pub trait SpecQuoteBail<E>: Sized {
    type Return;
    fn bail(self, err: E) -> Self::Return;
}

impl<T> SpecQuoteBail<TtError<Box<dyn Error>>> for &Spec<T> {
    type Return = T;

    fn bail(self, err: TtError<Box<dyn Error>>) -> T {
        panic!("quote failed: {err}")
    }
}

impl<T, E> SpecQuoteBail<TtError<E>> for &&Spec<TtResult<T, E>> {
    type Return = TtResult<T, E>;

    fn bail(self, err: TtError<E>) -> TtResult<T, E> {
        Err(err)
    }
}

impl<'a, T> SpecMut for &'a Spec<T> {
    type Mut = &'a mut T;
}

impl<'a, T> SpecMut for Spec<&'a mut T> {
    type Mut = &'a mut T;
}
