//! Compile-time literal parsing for quasi-quoting.

use std::{convert::Infallible, ffi::CStr, str::FromStr};

use proc_macro::{Literal, TokenStream};

use crate::{ReparseError, TokenQueue, TryToTokens, TtResult, ctfe};

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

impl<T: LitContents, const REGIME: u8> TryToTokens for DelayedLiteral<T, REGIME> {
    type Error = Infallible;

    #[inline]
    fn try_extend_tokens(&self, buf: &mut TokenQueue) -> TtResult<()> {
        let resolution = const { Regime::parse(REGIME, T::PARSERS) }.unwrap();
        buf.push(resolution(self.data));
        Ok(())
    }
}

pub fn fallback(text: &'static str, buf: &mut TokenQueue) -> TtResult<()> {
    let tt = TokenStream::from_str(text).map_err(move |lex| ReparseError::from_lit(lex, text))?;
    buf.extend(tt);
    Ok(())
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
    const fn recognize_suffixed_int(s: &[u8]) -> bool {
        if let Some(suffix) = ctfe::bytes_lastn(s, 5) {
            const_bytes_match!(suffix; {
                b"usize" | b"isize" => return true,
            })
        }

        if let Some(suffix) = ctfe::bytes_lastn(s, 4) {
            const_bytes_match!(suffix; {
                b"u128" | b"i128" => return true,
            })
        }

        if let Some(suffix) = ctfe::bytes_lastn(s, 3) {
            const_bytes_match!(suffix; {
                b"u64" | b"i64" | b"u32" | b"i32" | b"u16" | b"i16" => return true,
            })
        }

        if let Some(suffix) = ctfe::bytes_lastn(s, 2) {
            const_bytes_match!(suffix; {
                b"u8" | b"i8" => return true,
            })
        }

        false
    }

    const fn recognize_suffixed_float(s: &[u8]) -> bool {
        if let Some(suffix) = ctfe::bytes_lastn(s, 3) {
            const_bytes_match!(suffix; {
                b"f64" | b"f32" => return true,
            })
        }

        false
    }
    const fn recognize_suffixed_number(s: &[u8]) -> Regime {
        if Regime::recognize_suffixed_int(s) {
            Regime::IntSuffixed
        } else if Regime::recognize_suffixed_float(s) {
            Regime::FloatSuffixed
        } else {
            Regime::Int
        }
    }

    pub const fn recognize(input: &str) -> Regime {
        let mut s = input.as_bytes();
        macro_rules! parse_byte {
            ($($arm:pat $(if $g:expr)? => $ex:expr,)*) => {{
                let (s2, c) = match s.split_first() {
                    Some((c, s)) => {
                        (s, Some(c))
                    },
                    None => (s, None),
                };

                #[allow(unreachable_patterns)]
                return match c {
                    $($arm $(if $g)? => {
                        let _x = $ex;
                        #[allow(unreachable_code, unused_assignments)]
                        {
                            s = s2;
                            _x
                        }
                    })*
                    _ => return Regime::Unknown,
                };
            }};
        }

        // piss-simple parser tree.
        parse_byte! {
            Some(b'0') => parse_byte! {
                Some(b'x' | b'b' | b'o') => Regime::recognize_suffixed_number(s),
                _ if ctfe::bytes_any(s, b'.') => if Regime::recognize_suffixed_float(s)  { Regime::FloatSuffixed } else { Regime::Float },
                _ => Regime::recognize_suffixed_number(s),
            },
            Some(b'1'..=b'9') => parse_byte! {
                _ if ctfe::bytes_any(s, b'.') => if Regime::recognize_suffixed_float(s)  { Regime::FloatSuffixed } else { Regime::Float },
                _ => Regime::recognize_suffixed_number(s),
            },
            Some(b'b') => parse_byte! {
                Some(b'"') => Regime::ByteString,
                Some(b'\'') => Regime::ByteCharacter,
                Some(b'r') if ctfe::bytes_any(s, b'"') => Regime::ByteString,
            },
            Some(b'c') => parse_byte! {
                Some(b'"') => Regime::CString,
                Some(b'r') if ctfe::bytes_any(s, b'"') => Regime::CString,
            },
            Some(b'"') => Regime::String,
            Some(b'\'') => Regime::Character,
            Some(b'r') if ctfe::bytes_any(s, b'"') => Regime::String,
        }

        // match s.as_bytes().split_first() {
        //     Some(b'0'..=b'9') => match ,
        //     Some(b'b') => Som,
        //     None => todo!(),
        // }

        // const fn wing_string(s: &str, p: u8) -> Option<(&str, &str)> {
        //     if let Some((prefix, s2)) = ctfe::split_around(s, p)
        //         && let Some((inner, _suffix)) = ctfe::rsplit_around(s2, p)
        //     {
        //         Some((prefix, inner))
        //     } else {
        //         None
        //     }
        // }

        // if let Some((prefix, inner)) = wing_string(s, b'#')
        //     && let Some(_) = wing_string(inner, b'"')
        // {
        //     const_str_match!(prefix; {
        //         "br" => Some(Regime::ByteString),
        //         "cr" => Some(Regime::CString),
        //         "r" => Some(Regime::String),
        //         _ => None,
        //     })
        // } else if let Some((prefix, _)) = wing_string(s, b'"') {
        //     // string literal:
        //     const_str_match!(prefix; {
        //         "b" => Some(Regime::ByteString),
        //         "c" => Some(Regime::CString),
        //         "" => Some(Regime::String),
        //         _ => None,
        //     })
        // } else if let Some((prefix, _)) = wing_string(s, b'\'') {
        //     // char literal:
        //     const_str_match!(prefix; {
        //         "b" => Some(Regime::ByteCharacter),
        //         "" => Some(Regime::Character),
        //         _ => None,
        //     })
        // } else if let Some(regime) = Regime::recognize_suffixed_numeric(s) {
        //     Some(regime)
        // } else if ctfe::bytes_contain(s.as_bytes(), b'.') {
        //     Some(Regime::Float { suffixed: false })
        // } else {
        //     Some(Regime::Int { suffixed: false })
        // }
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
