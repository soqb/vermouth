use crate::for_all_punct_seqs;

/// Quasi-quoting for Rust source.
///
/// # Interpolation
///
/// The rules for interpolation behave similarly to `macro_rules!` using `@` instead of `$`:
/// * `quote! { @foo }` inlines the contents of the variable `foo` into the evaluated token stream.
///   `foo` must implement [`ToTokens`](crate::ToTokens).
/// * `quote! { @@ }` evaluates to just `@`.
/// * `quote! { @foo( @{ foo.bar } )* }`
#[macro_export]
macro_rules! quote {
    {} => { $crate::ඞ_macro_exports::proc_macro::TokenStream::new() };
    {$($t:tt)*} => {{
        #[allow(unused_mut)]
        let mut buf = $crate::ඞ_macro_exports::proc_macro::TokenStream::new();
        $crate::extend_quote!(buf <- { $($t)* });
        buf
    }};
}

/// Returns a closure implementing [`ToTokens`] which represents some quasi-quoted Rust source.
///
/// See [`quote`](crate::quote) for the details of quasi-quoting syntax.
#[macro_export]
macro_rules! quote_fn {
    ($($t:tt)*) => {
        $crate::ඞ_macro_exports::TokenF(
            #[allow(unused_mut)]
            |mut buf: &mut $crate::ඞ_macro_exports::proc_macro::TokenStream| {
                extend_quote!(buf <- { $($t)* });
            },
        )
    };
}

/// [Extends](crate::TokensExtend) an existing buffer with quasi-quoted Rust source.
///
/// One syntax is supported:
/// `extend_quote!(buf <- { my tokens })` writes `my tokens` into `buf`.
///
/// See [`quote`](crate::quote) for the details of quasi-quoting syntax.
#[macro_export]
macro_rules! extend_quote {
    ($buf:ident <- { $($t:tt)* }) => {{
        #[allow(unused_imports)]
        use $crate::{
            ඞ_macro_extend_quote_impl, ඞ_macro_exports::proc_macro,
            ඞ_macro_quote_punct_seq
        };

        let token = $crate::ඞ_macro_exports::Spec(&raw const $buf);
        #[allow(irrefutable_let_patterns)]
        let buf = if let () = () {
            match $buf {
                ref mut foo => foo,
            }
        } else {
            use $crate::ඞ_macro_exports::SpecMut;
            token.consume()
        };

        // NB: we ensure that `TokensExtend` is implemented here
        //     so the case for no tokens also fails
        //     if the implementation is not present.
        $crate::ඞ_macro_exports::assert_tokens_extend(buf);
        ඞ_macro_extend_quote_impl! { buf $($t)* }
    }};
}

#[doc(hidden)]
#[macro_export]
macro_rules! ඞ_macro_extend_quote_impl {
    ($buf:ident) => {};
    ($buf:ident @@ $($r:tt)*) => {
        $crate::TokensExtend::push(
            $buf,
            $crate::ඞ_macro_exports::new_punct('@', $crate::ඞ_macro_exports::proc_macro::Spacing::Alone)
        );
        ඞ_macro_extend_quote_impl! { $buf $($r)* }
    };
    ($buf:ident @ $name:ident $($r:tt)*) => {
        $crate::ToTokens::extend_tokens(&$name, $buf);
        ඞ_macro_extend_quote_impl! { $buf $($r)* }
    };
    ($buf:ident @ $extra:tt $($r:tt)*) => {
        compile_error!("invalid quasi-quoting syntax: see `vermouth::quote` for documentation.");
        ඞ_macro_extend_quote_impl! { $buf $($r)* }
    };
    ($buf:ident $o:tt $($r:tt)*) => {
        $crate::ඞ_macro_exports::quote_tt_impl!($buf $o);
        ඞ_macro_extend_quote_impl! { $buf $($r)* }
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! ඞ_macro_quote_punct_seq {
    ($_:tt $($char:literal)+) => {
        [
            $($crate::ඞ_macro_exports::new_punct($char, $crate::ඞ_macro_exports::proc_macro::Spacing::Joint),)+
        ]
    };
}

macro_rules! def_quote_tt {
    (arms = {$($arm:tt)*}, $({$($p:tt)+})*) => {
        export_macro! {
            #[unused_name(ඞ_macro_quote_tt_impl)]
            macro_rules! quote_tt_impl {
                $(
                    ($buf:ident $($p)+) => {
                        $crate::ඞ_macro_exports::StdExtend::extend(
                            $buf,
                            $crate::punct_decompose!(expand = ඞ_macro_quote_punct_seq, fallback = { }, $($p)*)
                        )
                    };
                )*
                $($arm)*
            }
        }
    };
}

for_all_punct_seqs!(
    def_quote_tt,
    arms = {
        ($buf:ident ($($t:tt)*)) => {
            let group = $crate::quote!($($t)*);
            $crate::TokensExtend::push(
                $buf,
                $crate::ඞ_macro_exports::new_group(group, $crate::ඞ_macro_exports::proc_macro::Delimiter::Parenthesis)
            );
        };
        ($buf:ident {$($t:tt)*}) => {
            let group = $crate::quote!($($t)*);
            $crate::TokensExtend::push(
                $buf,
                $crate::ඞ_macro_exports::new_group(group, $crate::ඞ_macro_exports::proc_macro::Delimiter::Brace)
            );
        };
        ($buf:ident [$($t:tt)*]) => {
            let group = $crate::quote!($($t)*);
            $crate::TokensExtend::push(
                $buf,
                $crate::ඞ_macro_exports::new_group(group, $crate::ඞ_macro_exports::proc_macro::Delimiter::Bracket)
            );
        };
        ($buf:ident $id:ident) => {
            $crate::TokensExtend::push($buf, const { $crate::ඞ_macro_exports::parse_ident(stringify!($id)).unwrap() });
        };
        ($buf:ident $lit:literal) => {
            $crate::TokensExtend::push($buf, const {
                match $crate::ඞ_macro_exports::new_lit(stringify!($lit), $lit) {
                    Some(lit) => lit,
                    None => {
                        panic!(concat!("unrecognised literal: ", stringify!($lit)));
                    }
                }
            });
        };
    }
);
