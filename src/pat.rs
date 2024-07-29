use proc_macro::{Delimiter, Group, Spacing, TokenTree};

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
/// # fn main() -> vermouth::Result<()> {
/// #
/// # // evil little hack to display a line which isn't compiled.
/// # #[cfg(any())]
/// let ref mut cx: Parser = unimplemented!();
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
/// # Ok(())
/// # }
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
                    Delimiter::Bracket => "brackets",
                    Delimiter::None => "implicit delimiters",
                };
                Err(Expected::noun(cx.gag(1), delim_str))
            }
        }
    }
}

/// Decomposes a larger token into a sequence of `char`s.
#[macro_export]
macro_rules! punct_decompose {
    (expand = $macro:ident, fallback = $_:tt, &)   => { $macro!(& '&') };
    (expand = $macro:ident, fallback = $_:tt, &&)  => { $macro!(&& '&' '&') };
    (expand = $macro:ident, fallback = $_:tt, &=)  => { $macro!(&= '&' '=') };
    (expand = $macro:ident, fallback = $_:tt, @)   => { $macro!(@ '@') };
    (expand = $macro:ident, fallback = $_:tt, ^)   => { $macro!(^ '^') };
    (expand = $macro:ident, fallback = $_:tt, ^=)  => { $macro!(^= '^' '=') };
    (expand = $macro:ident, fallback = $_:tt, :)   => { $macro!(: ':') };
    (expand = $macro:ident, fallback = $_:tt, ,)   => { $macro!(, ',') };
    (expand = $macro:ident, fallback = $_:tt, .)   => { $macro!(. '.') };
    (expand = $macro:ident, fallback = $_:tt, ..)  => { $macro!(.. '.' '.') };
    (expand = $macro:ident, fallback = $_:tt, ...) => { $macro!(... '.' '.' '.') };
    (expand = $macro:ident, fallback = $_:tt, ..=) => { $macro!(..= '.' '.' '=') };
    (expand = $macro:ident, fallback = $_:tt, =)   => { $macro!(= '=') };
    (expand = $macro:ident, fallback = $_:tt, ==)  => { $macro!(== '=' '=') };
    (expand = $macro:ident, fallback = $_:tt, =>)  => { $macro!(=> '=' '>') };
    (expand = $macro:ident, fallback = $_:tt, >=)  => { $macro!(>= '>' '=') };
    (expand = $macro:ident, fallback = $_:tt, >)   => { $macro!(> '>') };
    (expand = $macro:ident, fallback = $_:tt, <-)  => { $macro!(<- '<' '-') };
    (expand = $macro:ident, fallback = $_:tt, <=)  => { $macro!(<= '<' '=') };
    (expand = $macro:ident, fallback = $_:tt, <)   => { $macro!(< '<') };
    (expand = $macro:ident, fallback = $_:tt, -)   => { $macro!(- '-') };
    (expand = $macro:ident, fallback = $_:tt, -=)  => { $macro!(-= '-' '=') };
    (expand = $macro:ident, fallback = $_:tt, !=)  => { $macro!(!= '!' '=') };
    (expand = $macro:ident, fallback = $_:tt, !)   => { $macro!(! '!') };
    (expand = $macro:ident, fallback = $_:tt, |)   => { $macro!(| '|') };
    (expand = $macro:ident, fallback = $_:tt, |=)  => { $macro!(|= '|' '=') };
    (expand = $macro:ident, fallback = $_:tt, ||)  => { $macro!(|| '|' '|') };
    (expand = $macro:ident, fallback = $_:tt, ::)  => { $macro!(:: ':' ':') };
    (expand = $macro:ident, fallback = $_:tt, %)   => { $macro!(% '%') };
    (expand = $macro:ident, fallback = $_:tt, %=)  => { $macro!(%= '%' '=') };
    (expand = $macro:ident, fallback = $_:tt, +)   => { $macro!(+ '+') };
    (expand = $macro:ident, fallback = $_:tt, +=)  => { $macro!(+= '+' '=') };
    (expand = $macro:ident, fallback = $_:tt, #)   => { $macro!(# '#') };
    (expand = $macro:ident, fallback = $_:tt, ?)   => { $macro!(? '?') };
    (expand = $macro:ident, fallback = $_:tt, ->)  => { $macro!(-> '-' '>') };
    (expand = $macro:ident, fallback = $_:tt, ;)   => { $macro!(; ';') };
    (expand = $macro:ident, fallback = $_:tt, <<)  => { $macro!(<< '<' '<') };
    (expand = $macro:ident, fallback = $_:tt, <<=) => { $macro!(<<= '<' '<' '=') };
    (expand = $macro:ident, fallback = $_:tt, >>)  => { $macro!(>> '>' '>') };
    (expand = $macro:ident, fallback = $_:tt, >>=) => { $macro!(>>= '>' '>' '=') };
    (expand = $macro:ident, fallback = $_:tt, /)   => { $macro!(/ '/') };
    (expand = $macro:ident, fallback = $_:tt, /=)  => { $macro!(/= '/' '=') };
    (expand = $macro:ident, fallback = $_:tt, *)   => { $macro!(* '*') };
    (expand = $macro:ident, fallback = $_:tt, *=)  => { $macro!(*= '*' '=') };
    (expand = $macro:ident, fallback = $_:tt, ~)   => { $macro!(~ '~') };
    (expand = $macro:ident, fallback = $_:tt, _)   => { $macro!(_ '_') };

    (expand = $macro:ident, fallback = { $($fallback:tt)* }, $($tt:tt)+) => {
        $($fallback)*
    }
}

#[macro_export]
macro_rules! for_all_punct_combos {
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
pub struct PunctPat<'a> {
    chars: &'a [char],
    name: &'static str,
}

impl<'a> PunctPat<'a> {
    #[doc(hidden)]
    pub fn new(chars: &'a [char], name: &'static str) -> Self {
        Self { chars, name }
    }
}

impl<'a> Pattern for PunctPat<'a> {
    type Output = ();
    fn eat(self, cx: &mut Parser) -> Result<()> {
        let Some((&last, rest)) = self.chars.split_last() else {
            unreachable!("`PunctPat`s should have at least one element");
        };

        let erf = |err| Expected::lit(err, self.name);
        for &c in rest {
            cx.eat_punct_with_spacing(c, Spacing::Joint).map_err(erf)?;
        }
        cx.eat_punct(last).map_err(erf).map(drop)
    }
}

#[macro_export]
#[doc(hidden)]
macro_rules! ඞ_punct_pat_def {
        ($tt:tt $($char:literal)+) => {
            $crate::PunctPat::new(
                &[ $($char),* ],
                stringify!($tt),
            )
        };
    }

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
