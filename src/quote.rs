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
macro_rules! ඞ_macro_quote_window6 {
    ($buf:ident $s:lifetime {@} {@} {@} $_3:tt $_4:tt $_5:tt) => {
        core::compile_error!("the syntax `@@@` is not supported by `vermouth::quote`.");
    };
    ($buf:ident $s:lifetime {@} {@} $a:tt $b:tt $c:tt $d:tt) => {
        $crate::ඞ_macro_quote_tt_impl! { $buf $s {@} };
        $crate::ඞ_macro_quote_window6! { $buf $s _ _ _ _ _ $a }
        $crate::ඞ_macro_quote_window6! { $buf $s _ _ _ _ $a $b }
        $crate::ඞ_macro_quote_window6! { $buf $s _ _ _ $a $b $c }
        $crate::ඞ_macro_quote_window6! { $buf $s _ _ $a $b $c $d }
    };
    ($buf:ident $s:lifetime $_0:tt {@} {$n:ident} {($($t:tt)*)} {*} $a:tt) => {
        core::compile_error!("i'm working on it trust bro");
        $crate::ඞ_macro_quote_window6! { $buf $s _ _ _ _ _ $a }
    };
    ($buf:ident $s:lifetime $_0:tt {@} {$n:ident} {($($t:tt)*)} {$sep:tt} {*}) => {
        core::compile_error!("i'm working on it trust bro");
    };
    ($buf:ident $s:lifetime $_0:tt {@} {$n:ident} $a:tt $b:tt $c:tt) => {
        if let TtResult::Err(e) = $n.try_extend_tokens($buf) {
            break $s m::err(e);
        }
        $crate::ඞ_macro_quote_window6! { $buf $s _ _ _ _ _ $a }
        $crate::ඞ_macro_quote_window6! { $buf $s _ _ _ _ $a $b }
        $crate::ඞ_macro_quote_window6! { $buf $s _ _ _ $a $b $c }
    };
    ($buf:ident $s:lifetime $_0:tt {@} {@} $a:tt $b:tt $c:tt) => {};
    ($buf:ident $s:lifetime $_0:tt {@} $_2:tt $_3:tt $_4:tt $_5:tt) => {
        core::compile_error!(core::concat!(
            "invalid quasi-quoting syntax: `", core::stringify!($extra), "` following `@` is reserved.\n\
            help: use `@@` to quote a single `@` symbol.\n\
            help: see `vermouth::try_quote` for documentation."
        ));
    };
    ($buf:ident $s:lifetime $a:tt $b:tt {@} $_3:tt $_4:tt $_5:tt) => {
        $crate::ඞ_macro_quote_window6! { $buf $s $a $b _ _ _ _ }
    };
    ($buf:ident $s:lifetime $a:tt $b:tt $c:tt {@} $_4:tt $_5:tt) => {
        $crate::ඞ_macro_quote_window6! { $buf $s $a $b $c _ _ _ }
    };
    ($buf:ident $s:lifetime $a:tt $b:tt $c:tt $d:tt {@} $_5:tt) => {
        $crate::ඞ_macro_quote_window6! { $buf $s $a $b $c $d _ _ }
    };
    ($buf:ident $s:lifetime $a:tt $b:tt $c:tt $d:tt $e:tt {@}) => {
        $crate::ඞ_macro_quote_window6! { $buf $s $a $b $c $d $e _ }
    };
    ($buf:ident $s:lifetime $_0:tt $_1:tt $_2:tt $_3:tt $_4:tt $t:tt) => {
        $crate::ඞ_macro_quote_tt_impl! { $buf $s $t };
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! ඞ_macro_quote_matrix6 {
    (
        $buf:ident
        $s:lifetime
        { $($a:tt)* }
        { $($b:tt)* }
        { $($c:tt)* }
        { $($d:tt)* }
        { $($e:tt)* }
        { $($f:tt)* }
    ) => {
        $(
            $crate::ඞ_macro_quote_window6! { $buf $s $a $b $c $d $e $f }
        )*
    };
}
#[doc(hidden)]
#[macro_export]
macro_rules! ඞ_macro_extend_quote_impl {
    ($buf:ident $s:lifetime) => {};
    ($buf:ident $s:lifetime @) => {
        core::compile_error!("invalid quasi-quoting syntax: `@` cannot trail the input.");
    };
    ($buf:ident $s:lifetime $t:tt) => {
        $crate::ඞ_macro_quote_tt_impl! { $buf $s {$t} };
    };
    ($buf:ident $s:lifetime @ $n:ident) => {
        if let TtResult::Err(e) = $n.try_extend_tokens($buf) {
            break $s m::err(e);
        }
    };
    ($buf:ident $s:lifetime $($t:tt)*) => {
        $crate::ඞ_macro_quote_matrix6! {
            $buf $s
            { _ _ _ _ _ $({$t})* }
            { _ _ _ _ $({$t})* _ }
            { _ _ _ $({$t})* _ _ }
            { _ _ $({$t})* _ _ _ }
            { _ $({$t})* _ _ _ _ }
            { $({$t})* _ _ _ _ _ }
        }
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

macro_rules! def_quote_tt {
    (arms = {$($arm:tt)*}, $($p:tt)*) => {
        #[macro_export]
        #[doc(hidden)]
        macro_rules! ඞ_macro_quote_tt_impl_ {
            $(
                ($buf:ident $s:lifetime {$p}) => {
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
        ($buf:ident $s:lifetime _) => {};
        ($buf:ident $s:lifetime {($($t:tt)*)}) => {
            $crate::TokenBuf::open_group($buf, proc_macro::Delimiter::Parenthesis);
            $crate::ඞ_macro_extend_quote_impl! { $buf $s $($t)* };
            $crate::TokenBuf::close_group($buf);
        };
        ($buf:ident $s:lifetime {{$($t:tt)*}}) => {
            $crate::TokenBuf::open_group($buf, proc_macro::Delimiter::Brace);
            $crate::ඞ_macro_extend_quote_impl! { $buf $s $($t)* };
            $crate::TokenBuf::close_group($buf);
        };
        ($buf:ident $s:lifetime {[$($t:tt)*]}) => {
            $crate::TokenBuf::open_group($buf, proc_macro::Delimiter::Bracket);
            $crate::ඞ_macro_extend_quote_impl! { $buf $s $($t)* };
            $crate::TokenBuf::close_group($buf);
        };
        ($buf:ident $s:lifetime {$id:ident}) => {
            if let TtResult::Err(e) = const { m::parse_ident(stringify!($id)) }.try_extend_tokens($buf) {
                break $s m::err(e);
            }
        };
        ($buf:ident $s:lifetime {$lit:literal}) => {
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
