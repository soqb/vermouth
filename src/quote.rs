use crate::for_all_punct_seqs;

/// The fallible variant of [`quote`](crate::quote!).
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
#[cfg_attr(docsrs, doc(cfg(feature = "quote")))]
#[macro_export]
macro_rules! try_quote {
    {} => {
        $crate::ඞ_macro_exports::ok($crate::ඞ_macro_exports::proc_macro::TokenStream::new())
    };
    {$($t:tt)*} => {
        'esc: {
            #[allow(unused_imports)]
            use $crate::{TtResult, TryToTokens as _, ඞ_macro_exports::{self as m, proc_macro, core, Spec, SpecLiteralQuote as _}};
            $crate::ඞ_macro_inline_quote_impl! { q 'esc $($t)* }
            break 'esc TtResult::Ok(q);
        }
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! ඞ_macro_bail_specialized {
    ($e:expr) => {{
        #[allow(unused)]
        let y = match () {
            () => $crate::ඞ_macro_exports::Spec::empty(),
            () => {
                let z = loop {};
                return z;
                $crate::ඞ_macro_exports::Spec::new(&raw const z)
            }
        };
        use $crate::ඞ_macro_exports::SpecQuoteBail as _;
        return (&&y).bail($e);
    }};
}

/// Quasi-quoting for Rust source.
///
/// # Interpolation
///
/// The rules for interpolation behave similarly to
/// [a `macro_rules!` transcriber](https://doc.rust-lang.org/nightly/reference/macros-by-example.html#r-macro.decl.transcription),
/// using `@` instead of `$`:
/// * `quote! { @foo }` inlines the contents of the variable `foo` into the evaluated token stream.
///   `foo` must implement [`TryToTokens`](crate::TryToTokens).
/// * `quote! { @@ }` evaluates to just `@`.
///
/// # Bail Wizardry
///
/// On encountering an error, we leverage some type inference wizardry
/// to make this macro usually what you want:
/// - In a function returning `TtResult<T, E>`, we propogate the error to the caller.
/// - In any other context, we panic by unwrapping.
///
/// If the default behavior is wrong, try using [`try_quote`] and specifying types exactly.
///
/// # Escaping `@@@`
///
/// Notably, while `@@` escapes `@`, `@@@` is not supported.
///
/// ```compile_fail
/// # vermouth::ඞ_declare_test!();
/// # use vermouth::quote;
/// # let my_pattern = quote!();
/// quote! {
///     let foo @@ @my_pattern = todo!();
/// };
/// ```
///
/// Instead, try importing [`YouKnowWhatIMean`](crate::YouKnowWhatIMean), which evaluates to `@`.
///
/// ```no_run
/// # vermouth::ඞ_declare_test!();
/// # use vermouth::quote;
/// # let my_pattern = quote!();
/// use vermouth::YouKnowWhatIMean;
/// quote! {
///     let foo @YouKnowWhatIMean @my_pattern = todo!();
/// };
/// ```
#[cfg_attr(docsrs, doc(cfg(feature = "quote")))]
#[macro_export]
macro_rules! quote {
    {} => { $crate::TokenQueue::new() };
    {$($t:tt)*} => {{
        let tokens = $crate::try_quote! { $($t)* };
        match tokens {
            $crate::TtResult::Ok(buf) => buf,
            $crate::TtResult::Err(err) => $crate::ඞ_macro_bail_specialized!(err),
        }
    }};
}

/// Extends an existing buffer with quasi-quoted Rust source.
///
/// This syntax is supported:
/// `try_extend_quote!(buf, { .. })` writes `..` into `buf`.
///
/// See [`quote`](crate::quote!) for the details of quasi-quoting syntax.
#[cfg_attr(docsrs, doc(cfg(feature = "quote")))]
#[macro_export]
macro_rules! try_extend_quote {
    ($q:expr, { $($t:tt)* }) => {
        'esc: {
            #[allow(unused_imports)]
            use $crate::{TtResult, TryToTokens as _, ඞ_macro_exports::{self as m, proc_macro, core, Spec, SpecLiteralQuote as _}};
            let _q = $q;
            $crate::ඞ_macro_extend_quote_impl! { _q 'esc $($t)* };
            break 'esc TtResult::Ok(());
    }};
}

/// Returns an object implementing [`TryToTokens`](crate::TryToTokens) which represents some quasi-quoted Rust source.
///
/// See [`quote`](crate::quote!) for the details of quasi-quoting syntax.
#[cfg_attr(docsrs, doc(cfg(feature = "quote")))]
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
    ($q:ident $s:lifetime @ $name:ident) => {
        #[allow(unused_mut)]
        let mut $q = match m::try_to_tokens($name) {
            TtResult::Ok(buf) => buf,
            TtResult::Err(err) => break $s m::err(err),
        };
    };
    ($q:ident $s:lifetime $($t:tt)*) => {
        let mut $q = $crate::TokenQueue::new();
        let _q = &mut $q;
        $crate::ඞ_macro_extend_quote_impl! { _q $s $($t)* };
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! ඞ_macro_extend_quote_impl {
    ($q:ident $s:lifetime) => {};
    ($q:ident $s:lifetime @) => {
        core::compile_error!("invalid quasi-quoting syntax: `@` cannot trail the input.");
    };
    ($q:ident $s:lifetime $t:tt) => {
        $crate::ඞ_macro_quote_tt_impl! { $q $s {$t} };
    };
    ($q:ident $s:lifetime @ $n:ident) => {
        if let TtResult::Err(e) = $n.try_extend_tokens($q) {
            break $s m::err(e);
        }
    };
    ($q:ident $s:lifetime $($t:tt)*) => {
        // #[cfg(not(debug_assertions))]
        // $crate::TokenQueue::reserve($q, $crate::ඞ_macro_quote_reserve_size! { $($t)* });
        $crate::ඞ_macro_quote_matrixed! {
            ඞ_macro_quote_emit
            { $q $s }
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
macro_rules! ඞ_macro_quote_emit {
    (tt {$q:ident $s:lifetime} $t:tt) => {
        $crate::ඞ_macro_quote_tt_impl! { $q $s $t };
    };
    (embed {$q:ident $s:lifetime} $n:ident) => {
        if let TtResult::Err(e) = $n.try_extend_tokens($q) {
            break $s m::err(e);
        }
    };
    (rep $cx:tt $n:ident $($t:tt)*) => {
        core::compile_error!("i'm working on it trust bro");
    };
    (seprep $cx:tt $n:ident $sep:tt $($t:tt)*) => {
        core::compile_error!("i'm working on it trust bro");
    };
    (reserved $cx:tt $t:tt) => {
        core::compile_error!(core::concat!(
            "invalid quasi-quoting syntax: `",
            core::stringify!($t),
            "` following `@` is reserved.\n\
            help: use `@@` to quote a single `@` symbol.\n\
            help: see `vermouth::try_quote` for documentation.",
        ));
    };
    (triple_at $cx:tt) => {
        core::compile_error!("the syntax `@@@` is not supported by `vermouth::quote`.");
    };
}
#[doc(hidden)]
#[macro_export]
macro_rules! ඞ_macro_quote_reserve_size {
    () => { 0usize };
    (@) => { 0usize };
    ($t:tt) => {{
        let mut v = 0usize;
        $crate::ඞ_macro_quote_reserve_size_tt! { v {$t} }
        v
    }};
    (@ $n:ident) => {{
        let mut v = 0usize;
        v = v.wrapping_add($n.queue_size_hint().0);
        v
    }};
    ($($t:tt)*) => {{
        let mut v = 0usize;
        $crate::ඞ_macro_quote_matrixed! {
            ඞ_macro_quote_reserve_size_emit
            v
            { _ _ _ _ _ $({$t})* }
            { _ _ _ _ $({$t})* _ }
            { _ _ _ $({$t})* _ _ }
            { _ _ $({$t})* _ _ _ }
            { _ $({$t})* _ _ _ _ }
            { $({$t})* _ _ _ _ _ }
        }
        v
    }};
}

// NB: not a great metric, but overallocating is worse by benchmark.
#[doc(hidden)]
#[macro_export]
macro_rules! ඞ_macro_quote_reserve_size_tt {
    ($v:ident _) => {};
    ($v:ident ()) => {
        $v = $v.wrapping_add(1usize);
    };
    ($v:ident {}) => {
        $v = $v.wrapping_add(1usize);
    };
    ($v:ident []) => {
        $v = $v.wrapping_add(1usize);
    };
    ($v:ident {$lt:lifetime}) => {
        $v = $v.wrapping_add(2usize);
    };
    ($v:ident {$t:tt}) => {
        $v = $v.wrapping_add(1usize);
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! ඞ_macro_quote_reserve_size_emit {
    (tt $v:ident $t:tt) => {
        $crate::ඞ_macro_quote_reserve_size_tt! { $v $t }
    };
    (embed $v:ident $n:ident) => {
        $v = $v.wrapping_add($n.queue_size_hint().0);
    };
    (rep $v:ident $n:ident $($t:tt)*) => {};
    (seprep $v:ident $n:ident $sep:tt $($t:tt)*) => {};
    (reserved $v:ident $t:tt) => {};
    (triple_at $v:ident) => {};
}

#[doc(hidden)]
#[macro_export]
macro_rules! ඞ_macro_quote_parse_windowed {
    ($m:ident $cx:tt {@} {@} {@} $_3:tt $_4:tt $_5:tt) => {
        $crate::$m! { triple_at $cx }
    };
    ($m:ident $cx:tt {@} {@} $a:tt $b:tt $c:tt $d:tt) => {
        $crate::$m! { tt $cx {@} }
        $crate::ඞ_macro_quote_parse_windowed! { $m $cx _ _ _ _ _ $a }
        $crate::ඞ_macro_quote_parse_windowed! { $m $cx _ _ _ _ $a $b }
        $crate::ඞ_macro_quote_parse_windowed! { $m $cx _ _ _ $a $b $c }
        $crate::ඞ_macro_quote_parse_windowed! { $m $cx _ _ $a $b $c $d }
    };
    ($m:ident $cx:tt $_0:tt {@} {$n:ident} {($($t:tt)*)} {*} $a:tt) => {
        $crate::$m! { rep $cx $n $($t)* }
        $crate::ඞ_macro_quote_parse_windowed! { $q $s _ _ _ _ _ $a }
    };
    ($m:ident $cx:tt $_0:tt {@} {$n:ident} {($($t:tt)*)} {$sep:tt} {*}) => {
        $crate::$m! { seprep $cx $n $sep $($t)* }
    };
    ($m:ident $cx:tt $_0:tt {@} {$n:ident} $a:tt $b:tt $c:tt) => {
        $crate::$m! { embed $cx $n };
        $crate::ඞ_macro_quote_parse_windowed! { $m $cx _ _ _ _ _ $a }
        $crate::ඞ_macro_quote_parse_windowed! { $m $cx _ _ _ _ $a $b }
        $crate::ඞ_macro_quote_parse_windowed! { $m $cx _ _ _ $a $b $c }
    };
    ($m:ident $cx:tt $_0:tt {@} {@} $a:tt $b:tt $c:tt) => {};
    ($m:ident $cx:tt $_0:tt {@} $t:tt $_3:tt $_4:tt $_5:tt) => {
        $crate::$m! { reserved $cx $t }
    };
    ($m:ident $cx:tt $a:tt $b:tt {@} $_3:tt $_4:tt $_5:tt) => {
        $crate::ඞ_macro_quote_parse_windowed! { $m $cx $a $b _ _ _ _ }
    };
    ($m:ident $cx:tt $a:tt $b:tt $c:tt {@} $_4:tt $_5:tt) => {
        $crate::ඞ_macro_quote_parse_windowed! { $m $cx $a $b $c _ _ _ }
    };
    ($m:ident $cx:tt $a:tt $b:tt $c:tt $d:tt {@} $_5:tt) => {
        $crate::ඞ_macro_quote_parse_windowed! { $m $cx $a $b $c $d _ _ }
    };
    ($m:ident $cx:tt $a:tt $b:tt $c:tt $d:tt $e:tt {@}) => {
        $crate::ඞ_macro_quote_parse_windowed! { $m $cx $a $b $c $d $e _ }
    };
    ($m:ident $cx:tt $_0:tt $_1:tt $_2:tt $_3:tt $_4:tt $t:tt) => {
        $crate::$m! { tt $cx $t }
    };
}

// #[doc(hidden)]
// #[macro_export]
// macro_rules! ඞ_macro_quote_window6 {}

#[doc(hidden)]
#[macro_export]
macro_rules! ඞ_macro_quote_matrixed {
    (
        $m:ident
        $cx:tt
        { $($a:tt)* }
        { $($b:tt)* }
        { $($c:tt)* }
        { $($d:tt)* }
        { $($e:tt)* }
        { $($f:tt)* }
    ) => {
        $(
            $crate::ඞ_macro_quote_parse_windowed! { $m $cx $a $b $c $d $e $f }
        )*
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
            $($arm)*
            $(
                ($q:ident $s:lifetime {$p}) => {
                    m::push_punct(
                        $q,
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
        }

        #[doc(hidden)]
        pub use ඞ_macro_quote_tt_impl_ as ඞ_macro_quote_tt_impl;
    };
}

for_all_punct_seqs!(
    def_quote_tt,
    arms = {
        ($q:ident $s:lifetime _) => {};
        ($q:ident $s:lifetime {_}) => {
            m::push_underscore($q);
        };
        ($q:ident $s:lifetime ()) => {
            m::push_empty_group(proc_macro::Delimiter::Parenthesis);
        };
        ($q:ident $s:lifetime {}) => {
            m::push_empty_group(proc_macro::Delimiter::Brace);
        };
        ($q:ident $s:lifetime []) => {
            m::push_empty_group(proc_macro::Delimiter::Bracket);
        };
        ($q:ident $s:lifetime {($($t:tt)*)}) => {
            $crate::TokenQueue::open_group($q, proc_macro::Delimiter::Parenthesis);
            $crate::ඞ_macro_extend_quote_impl! { $q $s $($t)* };
            $crate::TokenQueue::close_and_enqueue_group($q);
        };
        ($q:ident $s:lifetime {{$($t:tt)*}}) => {
            $crate::TokenQueue::open_group($q, proc_macro::Delimiter::Brace);
            $crate::ඞ_macro_extend_quote_impl! { $q $s $($t)* };
            $crate::TokenQueue::close_and_enqueue_group($q);
        };
        ($q:ident $s:lifetime {[$($t:tt)*]}) => {
            $crate::TokenQueue::open_group($q, proc_macro::Delimiter::Bracket);
            $crate::ඞ_macro_extend_quote_impl! { $q $s $($t)* };
            $crate::TokenQueue::close_and_enqueue_group($q);
        };
        ($q:ident $s:lifetime {$id:ident}) => {
            if let TtResult::Err(e) = const { m::parse_ident(stringify!($id)) }.try_extend_tokens($q) {
                break $s m::err(e);
            }
        };
        ($q:ident $s:lifetime {$lit:literal}) => {
            if let TtResult::Err(e) = m::Spec::new(&$lit).ඞ_lit_quote::<{ m::parse_lit_regime(core::stringify!($lit)) }>(
                &$lit,
                core::stringify!($lit),
                $q
            ) {
                break $s m::err(e);
            }
        };
        ($q:ident $s:lifetime {$lt:lifetime}) => {
            if let TtResult::Err(e) = const { m::parse_lifetime(stringify!($lt)) }.try_extend_tokens($q) {
                break $s m::err(e);
            }
        };
    }
);
