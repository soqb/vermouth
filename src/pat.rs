use proc_macro::{Delimiter, Group, Punct, Spacing, TokenTree};

use crate::{Expected, Parser, Result};

/// Performs simple pattern matching on a [`Parser`] stream.
///
/// Often, this trait is interfaced through the `Parser` type itself
/// with [`Parser::eat`].
///
/// # Comparison with [`Parse`](crate::Parse)
///
/// `Parse` and `Pattern` have a symmetric relationship.
/// `Parse` is implemented for the type of the value being parsed,
/// and is associated with a type of additional arguments.
/// `Pattern` is implemented for the type of the arguments,
/// and associated with the type of the value being parsed.
/// This means they have distinctly different effects on inference:
///
/// ```
/// # vermouth::ඞ_declare_test!();
/// #
/// use proc_macro::{Span, TokenStream};
/// use vermouth::{Parser, Parse, Pattern};
///
/// #[derive(Debug, PartialEq)]
/// struct Foo;
/// #[derive(Default, Clone, Copy)]
/// struct FooArgs;
///
/// impl Parse for Foo {
///     type Args<'a> = FooArgs;
///     fn parse_with(_parser: &mut Parser, _args: FooArgs) -> vermouth::Result<Foo> {
///         Ok(Foo)
///     }
/// }
///
/// impl Pattern for FooArgs {
///     type Output = Foo;
///     fn eat(self, _parser: &mut Parser) -> vermouth::Result<Foo> {
///         Ok(Foo)
///     }
/// }
///
/// let ref mut cx: Parser;
/// # let ref mut cx = Parser::new(TokenStream::new(), Span::call_site());
///
/// // For `Pattern::eat` and `Parser::eat` we have to specify
/// // the type of the arguments to help the compiler infer the type being parsed.
/// let args = FooArgs;
/// let pattern_foo = cx.eat(args)?;
///
/// // For `Parse::parse_with` and `Parser::parse_with` we have to specify
/// // the type being parsed to disambiguate the method being called.
/// // Note that because of this, the type of the arguments does not have to be specified.
/// let args = Default::default();
/// let parse_foo: Foo = cx.parse_with(args)?;
///
/// assert_eq!(pattern_foo, parse_foo);
/// # Ok::<_, vermouth::Expected>(())
/// ```
///
/// It is the user's choice to implement `Pattern` or `Parse`
/// and often there is nothing stopping both from being implemented.
pub trait Pattern {
    type Output;

    fn eat(self, cx: &mut Parser) -> Result<Self::Output>;
}

impl Pattern for Delimiter {
    type Output = Group;

    fn eat(self, cx: &mut Parser) -> Result<Self::Output> {
        match cx.nibble() {
            Some(TokenTree::Group(contents)) if contents.delimiter() == self => Ok(contents),
            _ => {
                let delim_str = match self {
                    Delimiter::Parenthesis => "parentheses",
                    Delimiter::Brace => "braces",
                    Delimiter::Bracket => "square brackets",
                    Delimiter::None => "implicit delimiters",
                };
                Err(Expected::noun(cx.gag(1), delim_str))
            }
        }
    }
}

/// Decomposes a larger token into a sequence of `char`s.
///
/// This is most useful for facilitating other macro implementations (like [`punct_pat`]).
///
/// # Examples
///
/// ```rust
/// use vermouth::punct_decompose;
///
/// macro_rules! assert_makes_sense {
///     ($token:tt) => {
///         punct_decompose! {
///             expand = expand_assert_makes_sense,
///             fallback = { compile_error!("encountered malformed token!") },
///             $token
///         }
///     }
/// }
///
/// macro_rules! expand_assert_makes_sense {
///     ($token:tt $($char:literal)*) => {
///         let s = String::from_iter([ $($char),* ]);
///         assert_eq!(stringify!($token), s);
///     }
/// }
///
/// assert_makes_sense!(&&);
/// assert_makes_sense!(@);
/// assert_makes_sense!(..);
/// ```
///
/// [`punct_pat`]: crate::punct_pat
#[macro_export]
macro_rules! punct_decompose {
    (expand = $expand:ident, fallback = $_:tt, &)   => { $expand!(& '&') };
    (expand = $expand:ident, fallback = $_:tt, &&)  => { $expand!(&& '&' '&') };
    (expand = $expand:ident, fallback = $_:tt, &=)  => { $expand!(&= '&' '=') };
    (expand = $expand:ident, fallback = $_:tt, @)   => { $expand!(@ '@') };
    (expand = $expand:ident, fallback = $_:tt, ^)   => { $expand!(^ '^') };
    (expand = $expand:ident, fallback = $_:tt, ^=)  => { $expand!(^= '^' '=') };
    (expand = $expand:ident, fallback = $_:tt, :)   => { $expand!(: ':') };
    (expand = $expand:ident, fallback = $_:tt, ,)   => { $expand!(, ',') };
    (expand = $expand:ident, fallback = $_:tt, .)   => { $expand!(. '.') };
    (expand = $expand:ident, fallback = $_:tt, ..)  => { $expand!(.. '.' '.') };
    (expand = $expand:ident, fallback = $_:tt, ...) => { $expand!(... '.' '.' '.') };
    (expand = $expand:ident, fallback = $_:tt, ..=) => { $expand!(..= '.' '.' '=') };
    (expand = $expand:ident, fallback = $_:tt, =)   => { $expand!(= '=') };
    (expand = $expand:ident, fallback = $_:tt, ==)  => { $expand!(== '=' '=') };
    (expand = $expand:ident, fallback = $_:tt, =>)  => { $expand!(=> '=' '>') };
    (expand = $expand:ident, fallback = $_:tt, >=)  => { $expand!(>= '>' '=') };
    (expand = $expand:ident, fallback = $_:tt, >)   => { $expand!(> '>') };
    (expand = $expand:ident, fallback = $_:tt, <-)  => { $expand!(<- '<' '-') };
    (expand = $expand:ident, fallback = $_:tt, <=)  => { $expand!(<= '<' '=') };
    (expand = $expand:ident, fallback = $_:tt, <)   => { $expand!(< '<') };
    (expand = $expand:ident, fallback = $_:tt, -)   => { $expand!(- '-') };
    (expand = $expand:ident, fallback = $_:tt, -=)  => { $expand!(-= '-' '=') };
    (expand = $expand:ident, fallback = $_:tt, !=)  => { $expand!(!= '!' '=') };
    (expand = $expand:ident, fallback = $_:tt, !)   => { $expand!(! '!') };
    (expand = $expand:ident, fallback = $_:tt, |)   => { $expand!(| '|') };
    (expand = $expand:ident, fallback = $_:tt, |=)  => { $expand!(|= '|' '=') };
    (expand = $expand:ident, fallback = $_:tt, ||)  => { $expand!(|| '|' '|') };
    (expand = $expand:ident, fallback = $_:tt, ::)  => { $expand!(:: ':' ':') };
    (expand = $expand:ident, fallback = $_:tt, %)   => { $expand!(% '%') };
    (expand = $expand:ident, fallback = $_:tt, %=)  => { $expand!(%= '%' '=') };
    (expand = $expand:ident, fallback = $_:tt, +)   => { $expand!(+ '+') };
    (expand = $expand:ident, fallback = $_:tt, +=)  => { $expand!(+= '+' '=') };
    (expand = $expand:ident, fallback = $_:tt, #)   => { $expand!(# '#') };
    (expand = $expand:ident, fallback = $_:tt, ?)   => { $expand!(? '?') };
    (expand = $expand:ident, fallback = $_:tt, ->)  => { $expand!(-> '-' '>') };
    (expand = $expand:ident, fallback = $_:tt, ;)   => { $expand!(; ';') };
    (expand = $expand:ident, fallback = $_:tt, <<)  => { $expand!(<< '<' '<') };
    (expand = $expand:ident, fallback = $_:tt, <<=) => { $expand!(<<= '<' '<' '=') };
    (expand = $expand:ident, fallback = $_:tt, >>)  => { $expand!(>> '>' '>') };
    (expand = $expand:ident, fallback = $_:tt, >>=) => { $expand!(>>= '>' '>' '=') };
    (expand = $expand:ident, fallback = $_:tt, /)   => { $expand!(/ '/') };
    (expand = $expand:ident, fallback = $_:tt, /=)  => { $expand!(/= '/' '=') };
    (expand = $expand:ident, fallback = $_:tt, *)   => { $expand!(* '*') };
    (expand = $expand:ident, fallback = $_:tt, *=)  => { $expand!(*= '*' '=') };
    (expand = $expand:ident, fallback = $_:tt, ~)   => { $expand!(~ '~') };
    (expand = $expand:ident, fallback = $_:tt, _)   => { $expand!(_ '_') };

    (expand = $_:ident, fallback = { $($fallback:tt)* }, $($tt:tt)+) => {
        $($fallback)*
    }
}

/// Expands a macro with every distinct (supported) sequence of punctuation.
#[macro_export]
macro_rules! for_all_punct_seqs {
    ($expand:ident) => {
        $expand! {
            &
            &&
            &=
            @
            ^
            ^=
            :
            ,
            .
            ..
            ...
            ..=
            =
            ==
            =>
            >=
            >
            <-
            <=
            <
            -
            -=
            !=
            !
            |
            |=
            ||
            ::
            %
            %=
            +
            +=
            #
            ?
            ->
            ;
            <<
            <<=
            >>
            >>=
            /
            /=
            *
            *=
            ~
            _
        }
    };
}

/// A pattern representing a sequence of [`Punct`]s.
///
/// [`Punct`]: proc_macro::Punct
#[derive(Clone, Copy)]
pub struct PunctPat<const N: usize> {
    chars: [char; N],
    name: &'static str,
}

impl<const N: usize> PunctPat<N> {
    #[doc(hidden)]
    pub fn new(chars: [char; N], name: &'static str) -> Self {
        Self { chars, name }
    }
}

impl<const N: usize> Pattern for PunctPat<N> {
    type Output = [Punct; N];
    fn eat(self, cx: &mut Parser) -> Result<[Punct; N]> {
        let Some((&last, rest)) = self.chars.split_last() else {
            unreachable!("`PunctPat`s should have at least one element");
        };

        let erf = |err| Expected::lit(err, self.name);

        // should move to `MaybeUninit::assume_init_array`
        // but without even that api, unsafety is a bit overwhelming here.
        let mut puncts = std::array::from_fn(|_| Punct::new('#', Spacing::Alone));

        for (i, &c) in rest.iter().enumerate() {
            let p = cx.eat_punct_with_spacing(c, Spacing::Joint).map_err(erf)?;
            puncts[i] = p;
        }

        let p = cx.eat_punct(last).map_err(erf)?;
        puncts[puncts.len() - 1] = p;
        Ok(puncts)
    }
}

#[macro_export]
#[doc(hidden)]
macro_rules! ඞ_punct_pat_def {
    ($tt:tt $($char:literal)+) => {
        $crate::PunctPat::new(
            [ $($char),* ],
            stringify!($tt),
        )
    };
}

/// Transforms a sequence of punctuation (like `&&` or `..=`) into a [`PunctPat`].
///
/// The subsequent `PunctPat` implements [`Pattern`],
/// and so can be used with methods like [`Parser::eat`].
///
/// # Examples
///
/// ```
/// # vermouth::ඞ_declare_test!();
/// # use proc_macro::{TokenStream, TokenTree, Span, Punct, Spacing::{Joint, Alone}};
/// use vermouth::{Parser, punct_pat};
///
/// let ref mut cx: Parser;
/// # let stream = TokenStream::from_iter([
/// #     TokenTree::from(Punct::new('.', Joint)),
/// #     TokenTree::from(Punct::new('.', Joint)),
/// #     TokenTree::from(Punct::new('=', Alone)),
/// # ]);
/// # let ref mut cx = Parser::new(stream, Span::call_site());
/// assert_eq!(
///     cx.eat(punct_pat!(..=)),
///     Ok(()),
/// );
/// ```
#[macro_export]
macro_rules! punct_pat {
    ($($t:tt)+) => {{
        #[allow(unused_imports)]
        use $crate::ඞ_punct_pat_def;
        $crate::punct_decompose!(
            expand = ඞ_punct_pat_def,
            fallback = { ::core::compile_error!("unrecognised token") },
            $($t)+
        )
    }};
}
