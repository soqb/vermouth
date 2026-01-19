// exact same syntax for both dtolnay & vermouth: we're comparing equivalent idioms, after all.
pub fn bench() {
    for _ in 0..ITERS {
        let tokens = quote! {
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
        };
        let _ = black_box(TokenStream::from(tokens));
    }
}
