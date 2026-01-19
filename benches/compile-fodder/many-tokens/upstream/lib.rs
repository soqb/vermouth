use proc_macro::TokenStream;
use std::hint::black_box;

#[allow(dead_code)]
const LEN: usize = 256;
const ITERS: usize = 2usize.pow(14);

#[cfg(feature = "dtolnay")]
mod dtolnay {
    use super::*;
    use ::quote::quote;
    include!("impl.rs");
}

#[cfg(feature = "vermouth")]
mod vermouth {
    use super::*;
    use ::vermouth::quote;
    include!("impl.rs");
}

#[allow(unreachable_code)]
#[proc_macro]
pub fn feel_the_burn(_ts: TokenStream) -> TokenStream {
    #[cfg(feature = "vermouth")]
    vermouth::bench();

    #[cfg(feature = "dtolnay")]
    dtolnay::bench();

    #[cfg(not(any(feature = "dtolnay", feature = "vermouth")))]
    bench_fallback();

    TokenStream::new()
}

#[allow(dead_code)]
fn bench_fallback() {
    // this is our best-effort attempt at isolating the API cost of these approaches.
    // compared to the above, especially on -O3, this iterator is basically free.
    let p = proc_macro::Punct::new(',', proc_macro::Spacing::Alone);
    let it = (0..LEN).map(move |_| proc_macro::TokenTree::from(p.clone()));

    for _ in 0..ITERS {
        let _ = black_box(TokenStream::from_iter(it.clone()));
    }
}
