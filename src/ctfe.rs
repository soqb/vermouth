//! Library-internal CTFE utilities.

pub(crate) const fn bytes_any(mut bytes: &[u8], p: u8) -> bool {
    // impl borrows from stdlib's `is_ascii_simple` for optimal compile-time execution time.
    // but searching from the left should be generally faster
    // because most of the time the first byte will succeed.
    while let [first, rest @ ..] = bytes {
        if *first == p {
            break;
        }

        bytes = rest;
    }

    !bytes.is_empty()
}
