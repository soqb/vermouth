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
            """""""" """""""" """""""" """"""""
            """""""" """""""" """""""" """"""""
            """""""" """""""" """""""" """"""""
            """""""" """""""" """""""" """"""""

            """""""" """""""" """""""" """"""""
            """""""" """""""" """""""" """"""""
            """""""" """""""" """""""" """"""""
            """""""" """""""" """""""" """"""""

            """""""" """""""" """""""" """"""""
            """""""" """""""" """""""" """"""""
            """""""" """""""" """""""" """"""""
            """""""" """""""" """""""" """"""""

            """""""" """""""" """""""" """"""""
            """""""" """""""" """""""" """"""""
            """""""" """""""" """""""" """"""""
            """""""" """""""" """""""" """"""""
        }));
    }

    TokenStream::new()
}
