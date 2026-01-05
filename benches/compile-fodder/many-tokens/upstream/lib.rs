use proc_macro::TokenStream;
use std::hint::black_box;

#[cfg(feature = "dtolnay")]
use quote::quote;
#[cfg(feature = "vermouth")]
use vermouth::quote;

const LEN: usize = 256;
const ITERS: usize = 2usize.pow(14);

#[cfg(any(feature = "vermouth", feature = "dtolnay"))]
#[proc_macro]
pub fn feel_the_burn(_ts: TokenStream) -> TokenStream {
    // 256 * 2^14 = 2^22 = 4194304 tokens per invocation
    for _ in 0..ITERS {
        let _ = black_box(TokenStream::from(quote! {
            ,,,, ,,,, ,,,, ,,,,
            ,,,, ,,,, ,,,, ,,,,
            ,,,, ,,,, ,,,, ,,,,
            ,,,, ,,,, ,,,, ,,,,

            ,,,, ,,,, ,,,, ,,,,
            ,,,, ,,,, ,,,, ,,,,
            ,,,, ,,,, ,,,, ,,,,
            ,,,, ,,,, ,,,, ,,,,

            ,,,, ,,,, ,,,, ,,,,
            ,,,, ,,,, ,,,, ,,,,
            ,,,, ,,,, ,,,, ,,,,
            ,,,, ,,,, ,,,, ,,,,

            ,,,, ,,,, ,,,, ,,,,
            ,,,, ,,,, ,,,, ,,,,
            ,,,, ,,,, ,,,, ,,,,
            ,,,, ,,,, ,,,, ,,,,
        }));
    }

    TokenStream::new()
}

#[cfg(not(any(feature = "vermouth", feature = "dtolnay")))]
#[proc_macro]
pub fn feel_the_burn(_ts: TokenStream) -> TokenStream {
    // this is our best-effort attempt at isolating the API cost of these approaches.
    // compared to the above, especially on -O3, this iterator is basically free.
    let p = proc_macro::Punct::new(',', proc_macro::Spacing::Alone);
    let it = (0..LEN).map(move |_| proc_macro::TokenTree::from(p.clone()));

    for _ in 0..ITERS {
        let _ = black_box(TokenStream::from_iter(it.clone()));
    }

    TokenStream::new()
}
