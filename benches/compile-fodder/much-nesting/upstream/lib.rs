use proc_macro::TokenStream;
use std::hint::black_box;

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

#[proc_macro]
pub fn feel_the_burn(_ts: TokenStream) -> TokenStream {
    #[cfg(feature = "vermouth")]
    vermouth::bench();

    #[cfg(feature = "dtolnay")]
    dtolnay::bench();

    TokenStream::new()
}
