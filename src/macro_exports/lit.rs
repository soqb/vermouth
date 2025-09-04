use std::ffi::CStr;

use proc_macro::{Literal, TokenTree};

use crate::ctfe;

pub struct DelayedLiteral<T> {
    regime: Regime,
    data: T,
}

impl<T: LitContents> From<DelayedLiteral<T>> for TokenTree {
    fn from(delay: DelayedLiteral<T>) -> Self {
        T::accept(delay.regime, delay.data)
    }
}

pub const fn new_lit<T: LitContents>(str: &'static str, data: T) -> Option<DelayedLiteral<T>> {
    let Some(regime) = Regime::recognize(str) else {
        return None;
    };

    if !regime.any_matches(T::VALID) {
        return None;
    }

    Some(DelayedLiteral { regime, data })
}

/// The kind of literal a stringified token represents.
#[derive(Clone, Copy)]
pub enum Regime {
    String,
    CString,
    ByteString,
    Character,
    ByteCharacter,
    Int { suffixed: bool },
    Float { suffixed: bool },
}

impl Regime {
    const fn recognize_suffixed_numeric(s: &'static str) -> Option<Regime> {
        if let Some(suffix) = ctfe::str_lastn(s, 5) {
            const_str_match!(suffix; {
                "usize" | "isize" => return Some(Regime::Int { suffixed: true }),
            })
        }

        if let Some(suffix) = ctfe::str_lastn(s, 4) {
            const_str_match!(suffix; {
                "u128" | "i128" => return Some(Regime::Int { suffixed: true }),
            })
        }

        if let Some(suffix) = ctfe::str_lastn(s, 3) {
            const_str_match!(suffix; {
                "u64" | "i64" | "u32" | "i32" | "u16" | "i16" => return Some(Regime::Int { suffixed: true }),
                "f64" | "f32" => return Some(Regime::Float { suffixed: true }),
            })
        }

        if let Some(suffix) = ctfe::str_lastn(s, 2) {
            const_str_match!(suffix; {
                "u8" | "i8" => return Some(Regime::Int { suffixed: true }),
            })
        }

        None
    }

    pub const fn recognize(s: &'static str) -> Option<Regime> {
        const fn wing_string_prefix(s: &str, p: u8) -> Option<&str> {
            if let Some((prefix, s)) = ctfe::split_around(s, p)
                && let Some(..) = ctfe::rsplit_around(s, p)
            {
                Some(prefix)
            } else {
                None
            }
        }

        if let Some(prefix) = wing_string_prefix(s, b'"') {
            // string literal:
            const_str_match!(prefix; {
                "b" | "b#" => Some(Regime::ByteString),
                "c" | "c#" => Some(Regime::CString),
                "" => Some(Regime::String),
                _ => None,
            })
        } else if let Some(prefix) = wing_string_prefix(s, b'\'') {
            // char literal:
            const_str_match!(prefix; {
                "b" => Some(Regime::ByteCharacter),
                "" => Some(Regime::Character),
                _ => None,
            })
        } else if let Some(regime) = Regime::recognize_suffixed_numeric(s) {
            Some(regime)
        } else if ctfe::bytes_contain(s.as_bytes(), b'.') {
            Some(Regime::Float { suffixed: false })
        } else {
            Some(Regime::Int { suffixed: false })
        }
    }

    pub const fn eq(self, other: Regime) -> bool {
        match (self, other) {
            (Regime::String, Regime::String) => true,
            (Regime::CString, Regime::CString) => true,
            (Regime::ByteString, Regime::ByteString) => true,
            (Regime::Character, Regime::Character) => true,
            (Regime::ByteCharacter, Regime::ByteCharacter) => true,
            (Regime::Int { suffixed: a }, Regime::Int { suffixed: b }) => a == b,
            (Regime::Float { suffixed: a }, Regime::Float { suffixed: b }) => a == b,
            _ => false,
        }
    }

    pub const fn any_matches(self, candidates: &'static [Regime]) -> bool {
        let mut i = 0;
        while i < candidates.len() {
            if self.eq(candidates[i]) {
                return true;
            }
            i += 1;
        }

        false
    }
}

pub trait LitContents: Copy {
    const VALID: &[Regime];
    fn accept(regime: Regime, data: Self) -> TokenTree;
}

impl LitContents for char {
    const VALID: &[Regime] = &[Regime::Character];
    fn accept(regime: Regime, data: char) -> TokenTree {
        match regime {
            Regime::Character => Literal::character(data).into(),
            _ => unreachable!(),
        }
    }
}

impl LitContents for &'static str {
    const VALID: &[Regime] = &[Regime::String];
    fn accept(regime: Regime, data: &'static str) -> TokenTree {
        match regime {
            Regime::String => Literal::string(data).into(),
            _ => unreachable!(),
        }
    }
}

impl<const N: usize> LitContents for &'static [u8; N] {
    const VALID: &[Regime] = &[Regime::ByteString];
    fn accept(regime: Regime, data: &'static [u8; N]) -> TokenTree {
        match regime {
            Regime::ByteString => Literal::byte_string(data).into(),
            _ => unreachable!(),
        }
    }
}

impl LitContents for &'static [u8] {
    const VALID: &[Regime] = &[Regime::ByteString];
    fn accept(regime: Regime, data: &'static [u8]) -> TokenTree {
        match regime {
            Regime::ByteString => Literal::byte_string(data).into(),
            _ => unreachable!(),
        }
    }
}

impl LitContents for &'static CStr {
    const VALID: &[Regime] = &[Regime::CString];
    fn accept(regime: Regime, data: &'static CStr) -> TokenTree {
        match regime {
            Regime::CString => Literal::c_string(data).into(),
            _ => unreachable!(),
        }
    }
}

macro_rules! impl_lit_contents_for_int {
    ($($ty:ident($suff:ident, $unsuff:ident);)*) => {
        $(
            impl LitContents for $ty {
                const VALID: &[Regime] = &[Regime::Int { suffixed: true }, Regime::Int { suffixed: false }];
                fn accept(regime: Regime, n: Self) -> TokenTree {
                    let tt = match regime {
                        Regime::Int { suffixed: true } => Literal::$suff(n),
                        Regime::Int { suffixed: false } => Literal::$unsuff(n),
                        _ => unreachable!(),
                    };
                    tt.into()
                }
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
    const VALID: &[Regime] = &[
        Regime::ByteCharacter,
        Regime::Int { suffixed: true },
        Regime::Int { suffixed: false },
    ];
    fn accept(regime: Regime, n: u8) -> TokenTree {
        let tt = match regime {
            Regime::ByteCharacter => Literal::byte_character(n),
            Regime::Int { suffixed: true } => Literal::u8_suffixed(n),
            Regime::Int { suffixed: false } => Literal::u8_unsuffixed(n),
            _ => unreachable!(),
        };
        tt.into()
    }
}

impl LitContents for f32 {
    const VALID: &[Regime] = &[
        Regime::Float { suffixed: true },
        Regime::Float { suffixed: false },
    ];

    fn accept(regime: Regime, f: f32) -> TokenTree {
        let tt = match regime {
            Regime::Float { suffixed: true } => Literal::f32_suffixed(f),
            Regime::Float { suffixed: false } => Literal::f32_unsuffixed(f),
            _ => unreachable!(),
        };
        tt.into()
    }
}

impl LitContents for f64 {
    const VALID: &[Regime] = &[
        Regime::Float { suffixed: true },
        Regime::Float { suffixed: false },
    ];

    fn accept(regime: Regime, f: f64) -> TokenTree {
        let tt = match regime {
            Regime::Float { suffixed: true } => Literal::f64_suffixed(f),
            Regime::Float { suffixed: false } => Literal::f64_unsuffixed(f),
            _ => unreachable!(),
        };
        tt.into()
    }
}
