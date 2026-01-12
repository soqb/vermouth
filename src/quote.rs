//! See [`quote`](crate::quote!).

use proc_macro::{Punct, Spacing, Span, TokenStream};

// dawg i'm so tired of doing this
#[cfg(doc)]
use crate::{quote, verbatim};

use crate::{IntoTokens, TokenQueue};

/// Lazy quasi-quoting for Rust source.
///
/// See also [`Transcriber`] and [`TokenQueue`].
///
/// Returns a value (a [`Transcriber`]) implementing [`IntoTokens`] which can be used to build a [`TokenStream`].
///
/// The transcriber does not contain any tokens, but instead owns a closure which appends to a [`TokenQueue`].
///
/// # Interpolation
///
/// The rules for interpolation behave similarly to
/// [a `macro_rules!` transcriber](https://doc.rust-lang.org/nightly/reference/macros-by-example.html#r-macro.decl.transcription):
/// * `quote! { $foo }` inlines the contents of the variable `foo` into the evaluated token stream.
///   `foo` must implement [`IntoTokens`].
/// * `quote! { $$ }` evaluates to just `$`.
/// * unlike in `macro_rules!`, a lone `$` which might introduce ambiguity (e.g. `quote! { $ }`)
///   is always rejected.
///
/// This should be familiar if you have used
/// [the `quote` macro from David Tolnay's `quote` crate](https://docs.rs/quote/latest/quote/macro.quote.html),
/// but be aware that we use `$` rather than `#`, as our escape token.
///
/// # Token Fidelity and Verbatim Tokens
///
/// `quote` exploits compile-time introspection on token values to dramatically speed up transcription.
/// This, however, limits how tokens are transcribed in two ways.
///
/// Factoring out a call to the [`verbatim`] macro will solve both issues
/// by deferring to [`TokenStream::from_str`](TokenStream#impl-FromStr-for-TokenStream).
/// This comes at about an order-of-magnitude runtime cost for the token in question only.
///
/// ## 1. A very niche subset of valid tokens is rejected at compile time
///
/// In particular, custom numeric suffixes (`100u256`) and string prefixes (`w"foobar"`)
/// are supported by `verbatim` and not by `quote`, as is any yet-unreserved literal syntax.
/// For instance, the following fails to compile.
///
/// ```compile_fail
/// # vermouth::ඞ_declare_test!();
/// # use vermouth::quote;
/// quote! { let my_big_num = 100u256; }
/// # ;
/// ```
///
/// To work around this limitation, use the [`verbatim`] macro.
///
/// ```
/// # vermouth::ඞ_declare_test!();
/// # use vermouth::{quote, verbatim};
/// let v = verbatim!(100u256);
/// quote! { let my_big_num = $v; }
/// # ;
/// ```
///
/// Note that we explicitly support the edge case where a new type for literal values (e.g. [`f128`])
/// is added to the language and `vermouth` has not (yet 🤞) been updated to support it.
/// We use a compile-time switch to fall back to the same implementation as `verbatim`.
/// This is distinct from the cases above, where both `vermouth`
/// _and_ the Rust compiler fail to recognise a literal which is nevertheless syntactically valid.
///
/// See [the reference](https://doc.rust-lang.org/nightly/reference/tokens.html)
/// for the precise lexical structure of tokens in Rust today.
///
/// [`f128`]: https://github.com/rust-lang/rust/issues/116909
///
/// ## 2. Not all tokens are transcribed exactly as specified
///
/// Since we are limited by the methods which the standard library exposes,
/// we cannot currently guarantee the syntactic form of emitted literal tokens.
/// For example, the following two calls to `quote` are treated as if identical.
///
/// ```
/// # vermouth::ඞ_declare_test!();
/// # use vermouth::quote;
/// quote! { "foobar" }
/// # ;
/// quote! { r###"foobar"### }
/// # ;
/// ```
///
/// Note that we leverage the Rust compiler's literal parsing to ensure
/// that semantic meaning is always exactly preserved,
/// this is a purely syntactic and largely innocuous inconsistency.
/// For instance, `quote` _does_ guarantee that numeric literal suffixes will be respected.
///
/// ```
/// # vermouth::ඞ_declare_test!();
/// # use vermouth::quote;
/// // numeric literal with usize type:
/// quote! { 100usize }
/// # ;
/// // numeric literal with no specified type:
/// quote! { 100 }
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
                $crate::ඞ_macro_quote_extend_impl! { _q $({$t})* };
            },
        )
    };
}

/// A lazily-evaluated sequence of quoted tokens (what [`quote`] evaluates to).
///
/// See also [`quote`] and [`TokenQueue`].
///
/// [`Transcriber::from_fn`] can be used to manually construct a `Transcriber`, where one is required.
#[must_use = "`Transcriber`s are lazily evaluated. See `TokenQueue::extend_from`."]
#[derive(Clone, Copy)]
pub struct Transcriber<F> {
    f: F,
    span: Option<Span>,
}
impl<F> Transcriber<F>
where
    // NB: this doesn't stop us passing a `F: Fn(&mut TokenQueue)` since `Fn: FnMut: FnOnce`.
    F: FnOnce(&mut TokenQueue),
{
    /// Creates a new transcriber from a closure modifying a [`TokenQueue`].
    pub fn from_fn(f: F) -> Transcriber<F> {
        Transcriber { f, span: None }
    }

    /// Unwraps the closure backing this transcriber.
    pub fn into_fn(self) -> F {
        self.f
    }

    /// Annotates all tokens within the transcriber with the given span.
    ///
    /// ```
    /// # vermouth::ඞ_declare_test!();
    /// # use vermouth::{quote, TokenQueue};
    /// # use proc_macro::Span;
    /// #
    /// # let span = Span::call_site();
    /// # #[cfg(any())]
    /// let span: Span = omitted!();
    /// #
    /// let ref mut q = TokenQueue::new();
    /// q.extend_from(quote! { foo / bar }.with_span(span));
    /// ```
    ///
    /// See [`TokenQueue::set_tracked_span`] for more.
    pub fn with_span(mut self, span: Span) -> Transcriber<F> {
        self.span = Some(span);
        self
    }
}

impl<F> IntoTokens for Transcriber<F>
where
    F: FnOnce(&mut TokenQueue),
{
    fn extend_tokens(self, q: &mut TokenQueue) {
        if let Some(span) = self.span {
            q.set_tracked_span(span);
        }

        (self.f)(q);

        #[allow(clippy::redundant_pattern_matching, reason = "dude. lay off it.")]
        if let Some(_) = self.span {
            q.unset_tracked_span();
        }
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
/// See [`quote`](quote#escaping-) for use cases.
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
/// when compared to [`quote`].
/// See [the corresponding documentation](quote#token-fidelity-and-verbatim-tokens).
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
macro_rules! ඞ_macro_quote_extend_impl {
    ($q:ident) => {};
    ($q:ident {$}) => {
        core::compile_error!("invalid quasi-quoting syntax: `$` cannot trail the input.");
    };
    ($q:ident $t:tt) => {
        $crate::ඞ_macro_quote_tt_impl! { $q $t };
    };
    ($q:ident {$} {$n:ident}) => {
        $n.extend_tokens($q);
    };
    ($q:ident $($t:tt)*) => {
        $crate::ඞ_macro_quote_parse_matrix! {
            ඞ_macro_quote_emit
            $q
            { _ _ _ _ _ $($t)* }
            { _ _ _ _ $($t)* _ }
            { _ _ _ $($t)* _ _ }
            { _ _ $($t)* _ _ _ }
            { _ $($t)* _ _ _ _ }
            { $($t)* _ _ _ _ _ }
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
macro_rules! ඞ_macro_quote_parse_window {
    ($m:ident $cx:tt {$} {$} {$} $_3:tt $_4:tt $_5:tt) => {
        $crate::$m! { triple_at $cx }
    };
    ($m:ident $cx:tt {$} {$} $a:tt $b:tt $c:tt $d:tt) => {
        $crate::$m! { tt $cx {$} }
        $crate::ඞ_macro_quote_parse_window! { $m $cx _ _ _ _ _ $a }
        $crate::ඞ_macro_quote_parse_window! { $m $cx _ _ _ _ $a $b }
        $crate::ඞ_macro_quote_parse_window! { $m $cx _ _ _ $a $b $c }
        $crate::ඞ_macro_quote_parse_window! { $m $cx _ _ $a $b $c $d }
    };
    ($m:ident $cx:tt $_0:tt {$} {$n:ident} {($($t:tt)*)} {*} $a:tt) => {
        $crate::$m! { rep $cx $n $($t)* }
        $crate::ඞ_macro_quote_parse_window! { $q _ _ _ _ _ $a }
    };
    ($m:ident $cx:tt $_0:tt {$} {$n:ident} {($($t:tt)*)} {p:tt} {*}) => {
        $crate::$m! { seprep $cx $n p $($t)* }
    };
    ($m:ident $cx:tt $_0:tt {$} {$n:ident} $a:tt $b:tt $c:tt) => {
        $crate::$m! { embed $cx $n };
        $crate::ඞ_macro_quote_parse_window! { $m $cx _ _ _ _ _ $a }
        $crate::ඞ_macro_quote_parse_window! { $m $cx _ _ _ _ $a $b }
        $crate::ඞ_macro_quote_parse_window! { $m $cx _ _ _ $a $b $c }
    };
    ($m:ident $cx:tt $_0:tt {$} {$} $a:tt $b:tt $c:tt) => {};
    ($m:ident $cx:tt $_0:tt {$} $t:tt $_3:tt $_4:tt $_5:tt) => {
        $crate::$m! { reserved $cx $t }
    };
    ($m:ident $cx:tt $a:tt $b:tt {$} $_3:tt $_4:tt $_5:tt) => {
        $crate::ඞ_macro_quote_parse_window! { $m $cx $a $b _ _ _ _ }
    };
    ($m:ident $cx:tt $a:tt $b:tt $c:tt {$} $_4:tt $_5:tt) => {
        $crate::ඞ_macro_quote_parse_window! { $m $cx $a $b $c _ _ _ }
    };
    ($m:ident $cx:tt $a:tt $b:tt $c:tt $d:tt {$} $_5:tt) => {
        $crate::ඞ_macro_quote_parse_window! { $m $cx $a $b $c $d _ _ }
    };
    ($m:ident $cx:tt $a:tt $b:tt $c:tt $d:tt $e:tt {$}) => {
        $crate::ඞ_macro_quote_parse_window! { $m $cx $a $b $c $d $e _ }
    };
    ($m:ident $cx:tt $_0:tt $_1:tt $_2:tt $_3:tt $_4:tt $t:tt) => {
        $crate::$m! { tt $cx $t }
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! ඞ_macro_quote_parse_matrix {
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
            $crate::ඞ_macro_quote_parse_window! { $m $cx $a $b $c $d $e $f }
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
        $crate::ඞ_macro_quote_extend_impl! { $q $({$t})* };
        $crate::TokenQueue::close_substream_and_push_as_group($q, proc_macro::Delimiter::Parenthesis);
    };
    ($q:ident {{$($t:tt)*}}) => {
        $crate::TokenQueue::open_substream($q);
        $crate::ඞ_macro_quote_extend_impl! { $q $({$t})* };
        $crate::TokenQueue::close_substream_and_push_as_group($q, proc_macro::Delimiter::Brace);
    };
    ($q:ident {[$($t:tt)*]}) => {
        $crate::TokenQueue::open_substream($q);
        $crate::ඞ_macro_quote_extend_impl! { $q $({$t})* };
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
