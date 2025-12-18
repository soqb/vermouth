use crate::for_all_punct_seqs;

/// Quasi-quoting for Rust source.
///
/// # Interpolation
///
/// The rules for interpolation behave similarly to a `macro_rules!` body, using `@` instead of `$`:
/// * `try_quote! { @foo }` inlines the contents of the variable `foo` into the evaluated token stream.
///   `foo` must implement [`TryToTokens`](crate::TryToTokens).
/// * `try_quote! { @@ }` evaluates to just `@`.
///
/// # Errors
///
/// Formally, `try_quote` always evaluates to a [`TtResult<TokenStream, E>`](crate::TtResult) for some [error type] `E`.
///
/// [error type]: crate::TryToTokens::Error
///
/// Due to a plurality of error types and the implementation of [`TryToTokens`](crate::TryToTokens),
/// `try_quote` cannot generally infer an appropriate value of `E`.
/// Usually, [`Infallible`](std::convert::Infallible) is sufficient
/// since [`TtError`](crate::TtError) already encapsulates the errors that arise from e.g. quoting literals.
#[macro_export]
macro_rules! try_quote {
    {} => {
        $crate::ඞ_macro_exports::ok($crate::ඞ_macro_exports::proc_macro::TokenStream::new())
    };
    {$($t:tt)*} => {
        'esc: {
            #[allow(unused_imports)]
            use $crate::{TtResult, TryToTokens as _, ඞ_macro_exports::{self as m, proc_macro, core, Spec, SpecLiteralQuote as _}};
            $crate::ඞ_macro_inline_quote_impl! { buf 'esc $($t)* }
            TtResult::Ok(buf)
        }
    };
}

#[macro_export]
macro_rules! bail_specialized {
    ($e:expr) => {{
        #[allow(unused)]
        let y = match () {
            () => $crate::ඞ_macro_exports::Spec::empty(),
            () => {
                let z = panic!();
                return z;
                $crate::ඞ_macro_exports::Spec::new(&raw const z)
            }
        };
        use $crate::ඞ_macro_exports::SpecQuoteBail as _;
        return (&&y).bail($e)
    }};
    ($e:expr => $lab:lifetime) => {{
        #[allow(unused)]
        let y = match () {
            () => $crate::ඞ_macro_exports::Spec::empty(),
            () => {
                let z = loop {};
                break $lab z;
                $crate::ඞ_macro_exports::Spec::new(&raw const z)
            }
        };
        use $crate::ඞ_macro_exports::SpecQuoteBail as _;
        break $lab (&&y).bail($e)
    }};
}

/// A bail-on-error wrapper around [`try_quote`].
///
/// # Bail Wizardry
///
/// On encountering an error, we leverage some type inference wizardry
/// to make this macro usually what you want:
/// - In a function returning `TtResult<T, E>`, we propogate the error to the caller.
/// - In any other context, we panic by unwrapping.
#[macro_export]
macro_rules! quote {
    {} => { $crate::ඞ_macro_exports::proc_macro::TokenBuf::new() };
    {$($t:tt)*} => {{
        let tokens = $crate::try_quote! { $($t)* };
        match tokens {
            $crate::TtResult::Ok(buf) => buf,
            $crate::TtResult::Err(err) => $crate::bail_specialized!(err),
        }
    }};
}

/// Extends an existing buffer with quasi-quoted Rust source.
///
/// One syntax is supported:
/// `try_extend_quote!(buf, { my tokens })` writes `my tokens` into `buf`.
///
/// See [`try_quote`] for the details of quasi-quoting syntax.
#[macro_export]
macro_rules! try_extend_quote {
    ($buf:expr, { $($t:tt)* }) => {
        'esc: {
            #[allow(unused_imports)]
            use $crate::{TtResult, TryToTokens as _, ඞ_macro_exports::{self as m, proc_macro, core, Spec, SpecLiteralQuote as _}};
            let _buf = $buf;
            $crate::TokenBuf::reserve(_buf, $crate::ඞ_macro_extend_quote_size_reservation! { $($t)* });
            $crate::ඞ_macro_extend_quote_impl! { _buf 'esc $($t)* };
            TtResult::Ok(())
    }};
}

/// Returns an object implementing [`TryToTokens`](crate::TryToTokens) which represents some quasi-quoted Rust source.
///
/// See [`try_quote`] for the details of quasi-quoting syntax.
#[macro_export]
macro_rules! delay_quote {
    ($($t:tt)*) => {
        $crate::ඞ_macro_exports::make_fn(
            |buf| $crate::try_extend_quote!(buf, { $($t)* }),
        )
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! ඞ_macro_inline_quote_impl {
    ($buf:ident $s:lifetime @ $name:ident) => {
        #[allow(unused_mut)]
        let mut $buf = match m::try_to_tokens($name) {
            TtResult::Ok(buf) => buf,
            TtResult::Err(err) => break $s m::err(err),
        };
    };
    ($buf:ident $s:lifetime $($t:tt)*) => {
        let mut $buf = $crate::TokenBuf::new();
        let _buf = &mut $buf;
        $crate::TokenBuf::reserve(_buf, $crate::ඞ_macro_extend_quote_size_reservation! { $($t)* });
        $crate::ඞ_macro_extend_quote_impl! { _buf $s $($t)* };
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! ඞ_macro_extend_quote_size_reservation {
    () => { 0 };
    (@@ $($r:tt)*) => {
        1 + $crate::ඞ_macro_extend_quote_size_reservation! { $($r)* }
    };
    (@ $name:ident $($r:tt)*) => {
        $name.token_size_hint().0 + $crate::ඞ_macro_extend_quote_size_reservation! { $($r)* }
    };
    (@ $extra:tt $($r:tt)*) => { 0 };
    (($($t:tt)*) $($r:tt)*) => {
        2
        + $crate::ඞ_macro_extend_quote_size_reservation! { $($t)* }
        + $crate::ඞ_macro_extend_quote_size_reservation! { $($r)* }
    };
    ({$($t:tt)*} $($r:tt)*) => {
        2
        + $crate::ඞ_macro_extend_quote_size_reservation! { $($t)* }
        + $crate::ඞ_macro_extend_quote_size_reservation! { $($r)* }
    };
    ([$($t:tt)*] $($r:tt)*) => {
        2
        + $crate::ඞ_macro_extend_quote_size_reservation! { $($t)* }
        + $crate::ඞ_macro_extend_quote_size_reservation! { $($r)* }
    };
    ($t:tt $($r:tt)*) => {
        1 + $crate::ඞ_macro_extend_quote_size_reservation! { $($r)* }
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! ඞ_macro_extend_quote_impl {
    ($buf:ident $s:lifetime) => {};
    ($buf:ident $s:lifetime @@ $($r:tt)*) => {
        m::push_punct($buf, &['@']);
        $crate::ඞ_macro_extend_quote_impl! { $buf $s $($r)* }
    };
    ($buf:ident $s:lifetime @ $name:ident $($r:tt)*) => {
        if let TtResult::Err(e) = $name.try_extend_tokens($buf) {
            break $s m::err(e);
        }
        $crate::ඞ_macro_extend_quote_impl! { $buf $s $($r)* }
    };
    ($buf:ident $s:lifetime @ $extra:tt $($r:tt)*) => {
        core::compile_error!(core::concat!(
            "invalid quasi-quoting syntax: `", core::stringify!($extra), "` following `@` is reserved.\n\
            help: use `@@` to quote a single `@` symbol.\n\
            help: see `vermouth::try_quote` for documentation."
        ));
        $crate::ඞ_macro_extend_quote_impl! { $buf $s $($r)* }
    };
    ($buf:ident $s:lifetime $o:tt $($r:tt)*) => {
        $crate::ඞ_macro_quote_tt_impl!($buf $s $o);
        $crate::ඞ_macro_extend_quote_impl! { $buf $s $($r)* }
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! ඞ_macro_quote_punct_seq {
    ($_:tt $($char:literal)+) => {
        &[
            $($char,)+
        ]
    };
}

// FIXME: a dtolnay/quote-style context-aware macro would likely be significantly more performant for long streams.
//     look into breaking up a stream into consecutive extend_quote calls (or equivalent).
macro_rules! def_quote_tt {
    (arms = {$($arm:tt)*}, $($p:tt)*) => {
        #[macro_export]
        #[doc(hidden)]
        macro_rules! ඞ_macro_quote_tt_impl_ {
            $(
                ($buf:ident $s:lifetime $p) => {
                    m::push_punct(
                        $buf,
                        $crate::punct_decompose!(
                            expand = $crate::ඞ_macro_quote_punct_seq,
                            fallback = {
                                core::compile_error!(
                                    core::concat!(
                                        "unrecognised punctuation: ",
                                        core::stringify!($p),
                                    )
                                );
                            },
                            $p
                        )
                    );
                };
            )*
            $($arm)*
        }

        #[doc(hidden)]
        pub use ඞ_macro_quote_tt_impl_ as ඞ_macro_quote_tt_impl;
    };
}

for_all_punct_seqs!(
    def_quote_tt,
    arms = {
        ($buf:ident $s:lifetime ($($t:tt)*)) => {
            $crate::TokenBuf::open_group($buf, proc_macro::Delimiter::Parenthesis);
            $crate::ඞ_macro_extend_quote_impl! { $buf $s $($t)* };
            $crate::TokenBuf::close_group($buf);
        };
        ($buf:ident $s:lifetime {$($t:tt)*}) => {
            $crate::TokenBuf::open_group($buf, proc_macro::Delimiter::Brace);
            $crate::ඞ_macro_extend_quote_impl! { $buf $s $($t)* };
            $crate::TokenBuf::close_group($buf);
        };
        ($buf:ident $s:lifetime [$($t:tt)*]) => {
            $crate::TokenBuf::open_group($buf, proc_macro::Delimiter::Bracket);
            $crate::ඞ_macro_extend_quote_impl! { $buf $s $($t)* };
            $crate::TokenBuf::close_group($buf);
        };
        ($buf:ident $s:lifetime $id:ident) => {
            if let TtResult::Err(e) = const { m::parse_ident(stringify!($id)) }.try_extend_tokens($buf) {
                break $s m::err(e);
            }
        };
        ($buf:ident $s:lifetime $lit:literal) => {
            if let TtResult::Err(e) = m::Spec::new(&$lit).ඞ_lit_quote::<{ m::parse_lit_regime(core::stringify!($lit)) }>(
                &$lit,
                core::stringify!($lit),
                $buf
            ) {
                break $s m::err(e);
            }
        };
    }
);
