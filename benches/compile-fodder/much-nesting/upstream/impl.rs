pub fn bench() {
    for _ in 0..ITERS {
        let tokens = quote! {
            fn foo() -> Result<(), ()> {
                let x = Some(5);
                match x {
                    Some(y) => {
                        if y > 0 {
                            loop { break; }
                        } else { return Err(()); }
                    }
                    None => return Err(()),
                };
                let arr = [1, 2, 3];
                let tup = (4, 5, 6);
                let struct_val = Struct {
                    field1: 10,
                    field2: "hello",
                };
                async {
                    println!("async");
                    loop { break; }
                }
            }
            struct Struct {
                field1: i32,
                field2: &'static str,
            }
            impl Struct {}
            union Union(i32);
            enum Enum {
                Variant(i32),
            }
            impl Enum {}
            mod mod1 {}
            fn bar() -> Result<(), ()> {
                let z = Some(10);
            }
        };
        let _ = black_box(TokenStream::from(tokens));
    }
}
