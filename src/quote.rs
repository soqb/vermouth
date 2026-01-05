//! See [`quote`](crate::quote!).

use std::{convert::Infallible, error::Error};

use proc_macro::{Punct, Spacing, TokenStream};

use crate::{TokenQueue, TryIntoTokens, TryToTokens, TtError, TtResult, for_all_punct_seqs};

/// Lazy quasi-quoting for Rust source.
///
/// Returns a value (a [`Transcriber`]) implementing [`TryToTokens`] which can be used to build a [`TokenStream`].
///
/// The transcriber does not contain any tokens, but instead owns a closure which appends to a [`TokenQueue`].
///
/// # Interpolation
///
/// The rules for interpolation behave similarly to
/// [a `macro_rules!` transcriber](https://doc.rust-lang.org/nightly/reference/macros-by-example.html#r-macro.decl.transcription),
/// using `$` rather than the `#` which `dtolnay/quote` uses:
/// * `quote! { $foo }` inlines the contents of the variable `foo` into the evaluated token stream.
///   `foo` must implement [`TryToTokens`](crate::TryToTokens).
/// * `quote! { $$ }` evaluates to just `$`.
/// * unlike in `macro_rules!`, a lone `$` which might introduce ambiguity (e.g. `quote! { $ }`)
///   is always rejected.
///
/// # Escaping `$$$`
///
/// Notably, while `$$` escapes `$`, the trifold `$$$` is not supported.
///
/// ```compile_fail
/// # vermouth::ඞ_declare_test!();
/// # use vermouth::quote;
/// quote! {
///     let bills = stringify!($$$);
/// }
/// # .ascribe::<std::convert::Infallible>();
/// ```
///
/// Instead, try importing [`Dr`](crate::Dr), which evaluates to `$`.
///
/// ```
/// # vermouth::ඞ_declare_test!();
/// # use vermouth::quote;
/// use vermouth::Dr;
/// quote! {
///     let bills = stringify!($Dr $Dr $Dr);
/// }
/// # .ascribe::<std::convert::Infallible>();
/// ```
///
/// # Errors
///
/// Formally, `quote` always evaluates to a [`TtResult<TokenStream, E>`](crate::TtResult) for some [error type] `E`.
///
/// [error type]: crate::TryToTokens::Error
///
/// Due to a plurality of error types and the implementation of [`TryToTokens`](crate::TryToTokens),
/// `try_quote` cannot always infer an appropriate value of `E`.
///
/// ```compile_fail
/// # vermouth::ඞ_declare_test!();
/// # use vermouth::quote;
/// // ERROR: type annotations needed.
/// let _ = quote! { ... };
/// ```
///
/// If the default behavior is wrong, try using [`Transcriber::ascribe`] to specify the error type exactly.
/// Usually, [`Infallible`](std::convert::Infallible) is sufficient
/// since [`TtError`](crate::TtError) already encapsulates the errors that arise from e.g. quoting literals.
///
/// ```
/// # vermouth::ඞ_declare_test!();
/// # use vermouth::quote;
/// use std::convert::Infallible;
/// let _ = quote! { ... }.ascribe::<Infallible>();
/// ```
#[cfg_attr(docsrs, doc(cfg(feature = "quote")))]
#[macro_export]
macro_rules! quote {
    ($($t:tt)*) => {
        $crate::Transcriber::from_fn(
            |_q| '_esc: {
                #[allow(unused_imports)]
                use $crate::{TtResult, TryIntoTokens as _, ඞ_macro_exports::{self as m, proc_macro, core, Spec, SpecLiteralQuote as _}};
                $crate::ඞ_macro_extend_quote_impl! { _q '_esc $($t)* };
                TtResult::Ok(())
            },
        )
    };
}

/// A lazily-evaluated sequence of quoted tokens (i.e. what [`quote`](crate::quote!) evaluates to).
///
/// See [the `quote` macro](crate::quote!) for more.
#[derive(Clone, Copy)]
pub struct Transcriber<F>(F);
impl<E, F> Transcriber<F>
where
    E: Error + From<Infallible>,
    F: FnOnce(&mut TokenQueue) -> TtResult<(), E>,
{
    pub fn from_fn(f: F) -> Transcriber<F> {
        Transcriber(f)
    }

    /// Annotates this transcriber with an [error type](crate::quote!#errors).
    ///
    /// This is usually unnecessary but see [`quote`](crate::quote!#errors) for an example.
    pub fn ascribe<EE>(self) -> Transcriber<F>
    where
        // holy shit this actually works lol
        EE: seal::Reflex<This = E>,
    {
        self
    }
}

mod seal {
    pub trait Reflex {
        type This: ?Sized;
    }
    impl<T: ?Sized> Reflex for T {
        type This = T;
    }
}

impl<E, F> TryIntoTokens for Transcriber<F>
where
    E: Error + From<Infallible>,
    F: FnOnce(&mut TokenQueue) -> TtResult<(), E>,
{
    type Error = E;

    fn try_extend_tokens(self, q: &mut TokenQueue) -> TtResult<(), E> {
        (self.0)(q)
    }
}

impl<E, F> TryToTokens for Transcriber<F>
where
    E: Error + From<Infallible>,
    F: Fn(&mut TokenQueue) -> TtResult<(), E>,
{
    type Error = E;

    fn try_extend_tokens_ref(&self, q: &mut TokenQueue) -> TtResult<(), E> {
        (self.0)(q)
    }
}

impl<E, F> TryFrom<Transcriber<F>> for TokenStream
where
    E: Error + From<Infallible>,
    F: FnOnce(&mut TokenQueue) -> TtResult<(), E>,
{
    type Error = TtError<E>;

    fn try_from(value: Transcriber<F>) -> TtResult<TokenStream, E> {
        value.try_into_tokens().map(TokenStream::from)
    }
}

impl<E, F> TryFrom<&Transcriber<F>> for TokenStream
where
    E: Error + From<Infallible>,
    F: Fn(&mut TokenQueue) -> TtResult<(), E>,
{
    type Error = TtError<E>;

    fn try_from(value: &Transcriber<F>) -> TtResult<TokenStream, E> {
        value.try_to_tokens().map(TokenStream::from)
    }
}

/// The dollar doctor. Evaluates to `$`. Useful for escaping.
///
/// See [`quote`](crate::quote!#escaping-) for use cases.
#[derive(Debug, Clone, Copy)]
pub struct Dr;

impl TryIntoTokens for Dr {
    type Error = Infallible;

    fn try_extend_tokens(self, q: &mut TokenQueue) -> TtResult<()> {
        q.push(Punct::new('$', Spacing::Alone));
        Ok(())
    }

    fn queue_size_hint(&self) -> (usize, Option<usize>) {
        (1, Some(1))
    }
}

/// Quotes a single token (either a literal, an ident, or a lifetime) in exactly the format supplied.
#[macro_export]
macro_rules! verbatim {
    ($lt:lifetime) => {
        $crate::ඞ_macro_exports::Verbatim($crate::ඞ_macro_exports::core::stringify!($lt))
    };
    ($id:ident) => {
        $crate::ඞ_macro_exports::Verbatim($crate::ඞ_macro_exports::core::stringify!($id))
    };
    ($lit:literal) => {
        $crate::ඞ_macro_exports::Verbatim($crate::ඞ_macro_exports::core::stringify!($lit))
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! ඞ_macro_inline_quote_impl {
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
    ($q:ident $s:lifetime $) => {
        core::compile_error!("invalid quasi-quoting syntax: `$` cannot trail the input.");
    };
    ($q:ident $s:lifetime $t:tt) => {
        $crate::ඞ_macro_quote_tt_impl! { $q $s {$t} };
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
            help: see `vermouth::quote` for documentation.",
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
    ($) => { 0usize };
    ($t:tt) => {{
        let mut v = 0usize;
        $crate::ඞ_macro_quote_reserve_size_tt! { v {$t} }
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
    ($m:ident $cx:tt {$} {$} {$} $_3:tt $_4:tt $_5:tt) => {
        $crate::$m! { triple_at $cx }
    };
    ($m:ident $cx:tt {$} {$} $a:tt $b:tt $c:tt $d:tt) => {
        $crate::$m! { tt $cx {$} }
        $crate::ඞ_macro_quote_parse_windowed! { $m $cx _ _ _ _ _ $a }
        $crate::ඞ_macro_quote_parse_windowed! { $m $cx _ _ _ _ $a $b }
        $crate::ඞ_macro_quote_parse_windowed! { $m $cx _ _ _ $a $b $c }
        $crate::ඞ_macro_quote_parse_windowed! { $m $cx _ _ $a $b $c $d }
    };
    ($m:ident $cx:tt $_0:tt {$} {$n:ident} {($($t:tt)*)} {*} $a:tt) => {
        $crate::$m! { rep $cx $n $($t)* }
        $crate::ඞ_macro_quote_parse_windowed! { $q $s _ _ _ _ _ $a }
    };
    ($m:ident $cx:tt $_0:tt {$} {$n:ident} {($($t:tt)*)} {$sep:tt} {*}) => {
        $crate::$m! { seprep $cx $n $sep $($t)* }
    };
    ($m:ident $cx:tt $_0:tt {$} {$n:ident} $a:tt $b:tt $c:tt) => {
        $crate::$m! { embed $cx $n };
        $crate::ඞ_macro_quote_parse_windowed! { $m $cx _ _ _ _ _ $a }
        $crate::ඞ_macro_quote_parse_windowed! { $m $cx _ _ _ _ $a $b }
        $crate::ඞ_macro_quote_parse_windowed! { $m $cx _ _ _ $a $b $c }
    };
    ($m:ident $cx:tt $_0:tt {$} {$} $a:tt $b:tt $c:tt) => {};
    ($m:ident $cx:tt $_0:tt {$} $t:tt $_3:tt $_4:tt $_5:tt) => {
        $crate::$m! { reserved $cx $t }
    };
    ($m:ident $cx:tt $a:tt $b:tt {$} $_3:tt $_4:tt $_5:tt) => {
        $crate::ඞ_macro_quote_parse_windowed! { $m $cx $a $b _ _ _ _ }
    };
    ($m:ident $cx:tt $a:tt $b:tt $c:tt {$} $_4:tt $_5:tt) => {
        $crate::ඞ_macro_quote_parse_windowed! { $m $cx $a $b $c _ _ _ }
    };
    ($m:ident $cx:tt $a:tt $b:tt $c:tt $d:tt {$} $_5:tt) => {
        $crate::ඞ_macro_quote_parse_windowed! { $m $cx $a $b $c $d _ _ }
    };
    ($m:ident $cx:tt $a:tt $b:tt $c:tt $d:tt $e:tt {$}) => {
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
