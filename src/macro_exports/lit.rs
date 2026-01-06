//! Compile-time literal parsing for quasi-quoting.

use std::ffi::CStr;

use proc_macro::Literal;

use crate::{IntoTokens, TokenQueue, ctfe};

/// A hacky representation of a partially-parsed literal.
///
/// Contains both the CTFE-parsed [`Regime`] of the literal,
/// as well as the actual value of the literal, as decided by rustc.
///
/// Kinda like the following tuple.
/// ```rust,ignore
/// (literal, stringify!(literal), Regime::from_str(stringify!(literal)))
/// ```
#[derive(Clone, Copy)]
pub struct DelayedLiteral<T, const REGIME: u8> {
    data: T,
}

impl<T: LitContents, const REGIME: u8> DelayedLiteral<T, REGIME> {
    #[inline]
    pub fn new(data: T) -> Option<DelayedLiteral<T, REGIME>> {
        const { Regime::parse(REGIME, T::PARSERS) }.map(|_| DelayedLiteral { data })
    }
}

impl<T: LitContents, const REGIME: u8> IntoTokens for DelayedLiteral<T, REGIME> {
    #[inline]
    fn extend_tokens(self, buf: &mut TokenQueue) {
        let resolution = const { Regime::parse(REGIME, T::PARSERS) }.unwrap();
        buf.push(resolution(self.data));
    }
}

/// The kind of literal a stringified token represents.
#[derive(Clone, Copy)]
#[repr(u8)]
pub enum Regime {
    Unknown,
    String,
    CString,
    ByteString,
    Character,
    ByteCharacter,
    Int,
    IntSuffixed,
    Float,
    FloatSuffixed,
}

impl Regime {
    const fn is_suffixed_int(s: &[u8]) -> bool {
        match s {
            [.., b'u' | b'i', b's', b'i', b'z', b'e']
            | [.., b'u' | b'i', b'1', b'2', b'8']
            | [.., b'u' | b'i', b'6', b'4']
            | [.., b'u' | b'i', b'3', b'2']
            | [.., b'u' | b'i', b'1', b'6']
            | [.., b'u' | b'i', b'8'] => true,
            _ => false,
        }
    }

    const fn recognize_int(s: &[u8]) -> Regime {
        if Regime::is_suffixed_int(s) {
            Regime::IntSuffixed
        } else {
            Regime::Int
        }
    }

    const fn is_suffixed_float(s: &[u8]) -> bool {
        match s {
            [.., b'f', b'6', b'4'] | [.., b'f', b'3', b'2'] => true,
            _ => false,
        }
    }

    const fn recognize_float(s: &[u8]) -> Regime {
        if Regime::is_suffixed_float(s) {
            Regime::FloatSuffixed
        } else {
            Regime::Float
        }
    }

    const fn recognize_number(s: &[u8]) -> Regime {
        if Regime::is_suffixed_int(s) {
            Regime::IntSuffixed
        } else if Regime::is_suffixed_float(s) {
            Regime::FloatSuffixed
        } else {
            Regime::Int
        }
    }

    pub const fn recognize(input: &str) -> Regime {
        let s = input.as_bytes();
        match s {
            [b'0', b'x' | b'b' | b'o', s @ ..] => Regime::recognize_int(s),
            [b'0'..=b'9', s @ ..] if ctfe::bytes_any(s, b'.') => Regime::recognize_float(s),
            [b'0'..=b'9', s @ ..] => Regime::recognize_number(s),
            [b'b', b'"', ..] => Regime::ByteString,
            [b'b', b'\'', ..] => Regime::ByteCharacter,
            [b'b', b'r', s @ ..] if ctfe::bytes_any(s, b'"') => Regime::ByteString,
            [b'c', b'"', ..] => Regime::CString,
            [b'c', b'r', s @ ..] if ctfe::bytes_any(s, b'"') => Regime::CString,
            [b'"', ..] => Regime::String,
            [b'\'', ..] => Regime::Character,
            [b'r', s @ ..] if ctfe::bytes_any(s, b'"') => Regime::String,
            _ => Regime::Unknown,
        }
    }

    pub const fn parse<T>(
        repr: u8,
        candidates: &'static [LitParser<T>],
    ) -> Option<fn(T) -> Literal> {
        if repr == Regime::Unknown as u8 {
            return None;
        }

        let mut i = 0;
        while i < candidates.len() {
            let (regime, resolution) = candidates[i];
            if regime as u8 == repr {
                return Some(resolution);
            }
            i += 1;
        }

        None
    }
}

type LitParser<T> = (Regime, fn(T) -> Literal);

macro_rules! lit_parsers {
    ($( $reg:expr => $ctor:ident ),* $(,)?) => {
        &[$( ($reg, |x| Literal::$ctor(x)), )*]
    };
}

pub trait LitContents: Copy + 'static {
    const PARSERS: &[LitParser<Self>];
}

impl LitContents for char {
    const PARSERS: &[LitParser<Self>] = lit_parsers![Regime::Character => character];
}

impl LitContents for &'static str {
    const PARSERS: &[LitParser<Self>] = lit_parsers![Regime::String => string];
}

// impl<const N: usize> LitContents for &'static [u8; N] {
//     const PARSERS: &[(Regime, fn(Self) -> Literal)] = &[(Regime::ByteString, Literal::byte_string)];
// }

impl<const N: usize> LitContents for &'static [u8; N] {
    const PARSERS: &[LitParser<Self>] = lit_parsers![Regime::ByteString => byte_string];
}

impl LitContents for &'static CStr {
    const PARSERS: &[LitParser<Self>] = lit_parsers![Regime::CString => c_string];
}

macro_rules! impl_lit_contents_for_int {
    ($($ty:ident($suff:ident, $unsuff:ident);)*) => {
        $(
            impl LitContents for $ty {
                const PARSERS: &[LitParser<Self>] = lit_parsers![
                    Regime::Int => $unsuff,
                    Regime::IntSuffixed => $suff,
                ];
            }
        )*
    };
}

impl_lit_contents_for_int! {
    u16(u16_suffixed, u16_unsuffixed);
    u32(u32_suffixed, u32_unsuffixed);
    u64(u64_suffixed, u64_unsuffixed);
    u128(u128_suffixed, u128_unsuffixed);
    usize(usize_suffixed, usize_unsuffixed);
    i8(i8_suffixed, i8_unsuffixed);
    i16(i16_suffixed, i16_unsuffixed);
    i32(i32_suffixed, i32_unsuffixed);
    i64(i64_suffixed, i64_unsuffixed);
    i128(i128_suffixed, i128_unsuffixed);
    isize(isize_suffixed, isize_unsuffixed);
}

// manual impl because of `Regime::ByteCharacter`:
impl LitContents for u8 {
    const PARSERS: &[LitParser<Self>] = lit_parsers![
        Regime::ByteCharacter => byte_character,
        Regime::Int => u8_unsuffixed,
        Regime::IntSuffixed => u8_suffixed,
    ];
}

impl LitContents for f32 {
    const PARSERS: &[LitParser<Self>] = lit_parsers![
        Regime::Float => f32_unsuffixed,
        Regime::FloatSuffixed => f32_suffixed,
    ];
}

impl LitContents for f64 {
    const PARSERS: &[LitParser<Self>] = lit_parsers![
        Regime::Float => f64_unsuffixed,
        Regime::FloatSuffixed => f64_suffixed,
    ];
}
