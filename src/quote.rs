use crate::for_all_punct_seqs;

#[macro_export]
macro_rules! quote {
    {} => { $crate::ඞ_macro_exports::proc_macro::TokenStream::new() };
    {$($t:tt)*} => {{
        #[allow(unused_imports)]
        use $crate::{
            ඞ_extend_quote_impl, ඞ_macro_exports::{proc_macro, StdExtend},
            ඞ_quote_punct_seq, ඞ_quote_tt, ඞ_quote_tt_here
        };

        #[allow(unused_mut)]
        let mut buf = proc_macro::TokenStream::new();
        ඞ_extend_quote_impl! { buf $($t)* }
        buf
    }};
}

#[macro_export]
macro_rules! extend_quote {
    ($buf:ident <- { $($t:tt)* }) => {{
        #[allow(unused_imports)]
        use $crate::{
            ඞ_extend_quote_impl, ඞ_macro_exports::{proc_macro, StdExtend},
            ඞ_quote_punct_seq, ඞ_quote_tt, ඞ_quote_tt_here
        };

        $crate::ඞ_macro_exports::assert_tokens_extend(&$buf);
        ඞ_extend_quote_impl! { $buf $($t)* }
    }};
}

#[doc(hidden)]
#[macro_export]
macro_rules! ඞ_extend_quote_impl {
    ($buf:ident) => {};
    ($buf:ident # $name:ident $($r:tt)*) => {
        $crate::ToTokens::extend_tokens(&$name, &mut $buf);
        ඞ_extend_quote_impl! { $buf $($r)* }
    };
    ($buf:ident $o:tt $($r:tt)*) => {
        ඞ_quote_tt!($buf $o);
        ඞ_extend_quote_impl! { $buf $($r)* }
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! ඞ_quote_punct_seq {
    ($_:tt $($char:literal)+) => {
        [
            $($crate::ඞ_macro_exports::new_punct($char, proc_macro::Spacing::Joint),)+
        ]
    };
}

macro_rules! ඞ_def_quote_tt {
    (name = $name:ident, arms = {$($arm:tt)*}, $({$($p:tt)+})*) => {
        #[doc(hidden)]
        #[macro_export]
        macro_rules! _foo {
            $(
                ($buf:ident $($p)+) => {
                    $crate::ඞ_macro_exports::StdExtend::extend(
                        &mut $buf,
                        $crate::punct_decompose!(expand = ඞ_quote_punct_seq, fallback = { }, $($p)*)
                    )
                };
            )*
            $($arm)*
        }

        pub use _foo as $name;
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! ඞ_quote_tt {
    ($($t:tt)*) => {
        ඞ_quote_tt_here!($($t)*)
    };
}

for_all_punct_seqs!(
    ඞ_def_quote_tt,
    name = ඞ_quote_tt_here,
    arms = {
        ($buf:ident ($($tt:tt)*)) => {
            let group = $crate::quote!($($tt)*);
            $crate::TokensExtend::push(&mut $buf, $crate::ඞ_macro_exports::new_group(group, proc_macro::Delimiter::Parenthesis));
        };
        ($buf:ident {$($tt:tt)*}) => {
            let group = $crate::quote!($($tt)*);
            $crate::TokensExtend::push(&mut $buf, $crate::ඞ_macro_exports::new_group(group, proc_macro::Delimiter::Brace));
        };
        ($buf:ident [$($tt:tt)*]) => {
            let group = $crate::quote!($($tt)*);
            $crate::TokensExtend::push(&mut $buf, $crate::ඞ_macro_exports::new_group(group, proc_macro::Delimiter::Bracket));
        };
        ($buf:ident $id:ident) => {
            $crate::TokensExtend::push(&mut $buf, $crate::ඞ_macro_exports::new_ident(stringify!($id)));
        };
        ($buf:ident $lit:literal) => { };
    }
);
