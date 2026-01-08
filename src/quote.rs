//! See [`quote`](crate::quote!).

use proc_macro::{Punct, Spacing, TokenStream};

use crate::{IntoTokens, TokenQueue};

/// Lazy quasi-quoting for Rust source.
///
/// See also [`Transcriber`](crate::Transcriber) and [`TokenQueue`].
///
/// Returns a value (a [`Transcriber`]) implementing [`IntoTokens`] which can be used to build a [`TokenStream`].
///
/// The transcriber does not contain any tokens, but instead owns a closure which appends to a [`TokenQueue`].
///
/// # Interpolation
///
/// The rules for interpolation behave similarly to
/// [a `macro_rules!` transcriber](https://doc.rust-lang.org/nightly/reference/macros-by-example.html#r-macro.decl.transcription),
/// using `$` rather than the `#` which `dtolnay/quote` uses:
/// * `quote! { $foo }` inlines the contents of the variable `foo` into the evaluated token stream.
///   `foo` must implement [`IntoTokens`](crate::IntoTokens).
/// * `quote! { $$ }` evaluates to just `$`.
/// * unlike in `macro_rules!`, a lone `$` which might introduce ambiguity (e.g. `quote! { $ }`)
///   is always rejected.
///
/// # Verbatim Tokens
///
/// `quote` exploits compile-time introspection on token values to dramatically speed up transcription.
/// This is constrains which tokens can be directly quoted somewhat; for instance, the following is rejected.
///
/// ```compile_fail
/// # vermouth::ඞ_declare_test!();
/// # use vermouth::quote;
/// quote! { let my_big_num = 100u256; }
/// # ;
/// ```
///
/// To work around this, use the [`verbatim`](crate::verbatim!) macro.
///
/// ```
/// # vermouth::ඞ_declare_test!();
/// # use vermouth::{quote, verbatim};
/// let v = verbatim!(100u256);
/// quote! { let my_big_num = $v; }
/// # ;
/// ```
///
/// # Escaping `$$$`
///
/// Notably, while `$$` escapes `$`, the trifold `$$$` is not supported.
/// (This is merely a consequence of the linear-time `macro_rules!` implementation of `quote`).
///
/// ```compile_fail
/// # vermouth::ඞ_declare_test!();
/// # use vermouth::quote;
/// quote! {
///     let bills = stringify!($$$);
/// }
/// # ;
/// ```
///
/// Instead, try importing [`Dr`], which evaluates to `$`.
///
/// ```
/// # vermouth::ඞ_declare_test!();
/// # use vermouth::quote;
/// use vermouth::Dr;
/// quote! {
///     let bills = stringify!($Dr $Dr $Dr);
/// }
/// # ;
/// ```
#[cfg_attr(docsrs, doc(cfg(feature = "quote")))]
#[macro_export]
macro_rules! quote {
    ($($t:tt)*) => {
        $crate::Transcriber::from_fn(
            |_q| {
                #[allow(unused_imports)]
                use $crate::{IntoTokens as _, ඞ_macro_exports::{self as m, proc_macro, core, Spec, SpecLiteralQuote as _}};
                $crate::ඞ_macro_extend_quote_impl! { _q $($t)* };
            },
        )
    };
}

/// A lazily-evaluated sequence of quoted tokens (what [`quote`](crate::quote!) evaluates to).
///
/// See also [`quote`](crate::quote!) and [`TokenQueue`].
///
/// [`Transcriber::from_fn`] can be used to manually construct a `Transcriber`, where one is required.
#[must_use = "`Transcriber`s are lazily evaluated. See `TokenQueue::extend_from`."]
#[derive(Clone, Copy)]
pub struct Transcriber<F>(F);
impl<F> Transcriber<F>
where
    // NB: this doesn't stop us passing a `F: Fn(&mut TokenQueue)` since `Fn: FnMut: FnOnce`.
    F: FnOnce(&mut TokenQueue),
{
    /// Creates a new transcriber from a closure modifying a [`TokenQueue`].
    pub fn from_fn(f: F) -> Transcriber<F> {
        Transcriber(f)
    }
}

impl<F> IntoTokens for Transcriber<F>
where
    F: FnOnce(&mut TokenQueue),
{
    fn extend_tokens(self, q: &mut TokenQueue) {
        (self.0)(q)
    }
}

impl<F> From<Transcriber<F>> for TokenStream
where
    F: FnOnce(&mut TokenQueue),
{
    fn from(value: Transcriber<F>) -> TokenStream {
        TokenStream::from(value.into_tokens())
    }
}

/// The dollar doctor. Evaluates to `$`. Useful for escaping.
///
/// See [`quote`](crate::quote!#escaping-) for use cases.
#[derive(Debug, Clone, Copy)]
pub struct Dr;

impl IntoTokens for Dr {
    fn extend_tokens(self, q: &mut TokenQueue) {
        q.push(Punct::new('$', Spacing::Alone));
    }

    fn queue_size_hint(&self) -> (usize, Option<usize>) {
        (1, Some(1))
    }
}

/// Quotes a single token (either a literal, an ident, or a lifetime) in exactly the format supplied.
///
/// This macro expands the range of quotable tokens, at the cost of performance,
/// when compared to [`quote`](crate::quote!). See [the corresponding documentation](crate::quote!#verbatim-tokens).
///
/// For instance, custom numeric suffixes and string prefixes are supported (`100u256` or `w"foobar"`),
/// but this is something like an order of magnitude slower than directly using `quote`,
/// since we are not able to perform compile-time introspection on the tokens.
///
/// See [the reference](https://doc.rust-lang.org/nightly/reference/tokens.html)
/// for the precise lexical structure of tokens.
#[cfg_attr(docsrs, doc(cfg(feature = "quote")))]
#[macro_export]
macro_rules! verbatim {
    ($lt:lifetime) => {
        $crate::ඞ_macro_exports::Verbatim {
            text: $crate::ඞ_macro_exports::core::stringify!($lt),
            kind: $crate::ඞ_macro_exports::ReparseKind::Lifetime,
            location: $crate::ඞ_macro_capture_source_location!(),
        }
    };
    ($id:ident) => {
        $crate::ඞ_macro_exports::Verbatim {
            text: $crate::ඞ_macro_exports::core::stringify!($id),
            kind: $crate::ඞ_macro_exports::ReparseKind::Ident,
            location: $crate::ඞ_macro_capture_source_location!(),
        }
    };
    ($lit:literal) => {
        $crate::ඞ_macro_exports::Verbatim {
            text: $crate::ඞ_macro_exports::core::stringify!($lit),
            kind: $crate::ඞ_macro_exports::ReparseKind::Literal,
            location: $crate::ඞ_macro_capture_source_location!(),
        }
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! ඞ_macro_inline_quote_impl {
    ($q:ident $($t:tt)*) => {
        let mut $q = $crate::TokenQueue::new();
        let _q = &mut $q;
        $crate::ඞ_macro_extend_quote_impl! { _q $($t)* };
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! ඞ_macro_extend_quote_impl {
    ($q:ident) => {};
    ($q:ident $) => {
        core::compile_error!("invalid quasi-quoting syntax: `$` cannot trail the input.");
    };
    ($q:ident $t:tt) => {
        $crate::ඞ_macro_quote_tt_impl! { $q {$t} };
    };
    ($q:ident $($t:tt)*) => {
        // #[cfg(not(debug_assertions))]
        // $crate::TokenQueue::reserve($q, $crate::ඞ_macro_quote_reserve_size! { $($t)* });
        $crate::ඞ_macro_quote_matrixed! {
            ඞ_macro_quote_emit
            $q
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
    (tt $q:ident $t:tt) => {
        $crate::ඞ_macro_quote_tt_impl! { $q $t };
    };
    (embed $q:ident $n:ident) => {
        $n.extend_tokens($q);
    };
    (rep $cx:tt $n:ident $($t:tt)*) => {
        core::compile_error!("i'm working on it trust bro");
    };
    (seprep $cx:tt $n:ident p:tt $($t:tt)*) => {
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
    (seprep $v:ident $n:ident p:tt $($t:tt)*) => {};
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
        $crate::ඞ_macro_quote_parse_windowed! { $q _ _ _ _ _ $a }
    };
    ($m:ident $cx:tt $_0:tt {$} {$n:ident} {($($t:tt)*)} {p:tt} {*}) => {
        $crate::$m! { seprep $cx $n p $($t)* }
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

/// Quotes a single token.
#[macro_export]
#[doc(hidden)]
macro_rules! ඞ_macro_quote_tt_impl {
    ($q:ident _) => {};
    ($q:ident {_}) => {
        m::push_underscore($q);
    };
    ($q:ident {()}) => {
        m::push_empty_group($q, proc_macro::Delimiter::Parenthesis);
    };
    ($q:ident {{}}) => {
        m::push_empty_group($q, proc_macro::Delimiter::Brace);
    };
    ($q:ident {[]}) => {
        m::push_empty_group($q, proc_macro::Delimiter::Bracket);
    };
    ($q:ident {($($t:tt)*)}) => {
        $crate::TokenQueue::open_substream($q);
        $crate::ඞ_macro_extend_quote_impl! { $q $($t)* };
        $crate::TokenQueue::close_substream_and_push_as_group($q, proc_macro::Delimiter::Parenthesis);
    };
    ($q:ident {{$($t:tt)*}}) => {
        $crate::TokenQueue::open_substream($q);
        $crate::ඞ_macro_extend_quote_impl! { $q $($t)* };
        $crate::TokenQueue::close_substream_and_push_as_group($q, proc_macro::Delimiter::Brace);
    };
    ($q:ident {[$($t:tt)*]}) => {
        $crate::TokenQueue::open_substream($q);
        $crate::ඞ_macro_extend_quote_impl! { $q $($t)* };
        $crate::TokenQueue::close_substream_and_push_as_group($q, proc_macro::Delimiter::Bracket);
    };
    ($q:ident {$id:ident}) => {
        $crate::TokenQueue::push($q, const {
            m::parse_ident(stringify!($id), $crate::ඞ_macro_capture_source_location!())
        });
    };
    ($q:ident {$lit:literal}) => {
        m::Spec::new(&$lit).ඞ_lit_quote::<{ m::parse_lit_regime(core::stringify!($lit)) }>(
            &$lit,
            core::stringify!($lit),
            $crate::ඞ_macro_capture_source_location!(),
            $q
        );
    };
    ($q:ident {$lt:lifetime}) => {
        $crate::TokenQueue::push($q, const {
            m::parse_lifetime(stringify!($lt), $crate::ඞ_macro_capture_source_location!())
        });
    };
    ($q:ident {$p:tt}) => {
        m::push_punct(
            $q,
            $crate::punct_decompose!(
                expand = $crate::ඞ_macro_quote_punct_seq,
                fallback = {
                    core::compile_error!(
                        core::concat!(
                            "unrecognised token: ",
                            core::stringify!($p),
                        )
                    );
                },
                $p
            )
        );
    };
}
