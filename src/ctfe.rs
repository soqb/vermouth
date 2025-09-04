//! Library-internal CTFE utilities (mostly string operations).

pub const fn split_around(str: &str, p: u8) -> Option<(&str, &str)> {
    let mut i = 0;
    while i < str.len() {
        if str.is_char_boundary(i) {
            if str.as_bytes()[i] == p {
                let (a, b) = str.split_at(i);
                return Some((a, b.split_at(1).1));
            }
        }

        i += 1;
    }

    None
}

pub const fn rsplit_around(str: &str, p: u8) -> Option<(&str, &str)> {
    let mut i = str.len();
    while i > 0 {
        i -= 1;

        if str.is_char_boundary(i) {
            if str.as_bytes()[i] == p {
                let (a, b) = str.split_at(i);
                return Some((a, b.split_at(1).1));
            }
        }
    }

    None
}

pub const fn bytes_contain(bytes: &[u8], p: u8) -> bool {
    let mut i = 0;
    while i < bytes.len() {
        if bytes[i] == p {
            return true;
        }

        i += 1;
    }

    false
}

pub const fn bytes_eq(lhs: &[u8], rhs: &[u8]) -> bool {
    if lhs.len() != rhs.len() {
        return false;
    }

    let mut i = 0;
    while i < lhs.len() {
        if lhs[i] != rhs[i] {
            return false;
        }

        i += 1;
    }

    true
}

pub const fn str_lastn(s: &str, n: usize) -> Option<&str> {
    let Some(m) = s.len().checked_sub(n) else {
        return None;
    };

    Some(s.split_at(m).1)
}

macro_rules! const_str_match {
    ($str:expr; { $($($arm:literal)|+ => $body:expr),* $(, _ => $fallback:expr)? $(,)? }) => {
        if false { unreachable!() }
        $(
            else if $($crate::ctfe::bytes_eq($str.as_bytes(), $arm.as_bytes()))||+ {
                $body
            }
        )*
        $(
            else {
                $fallback
            }
        )?
    };
}
