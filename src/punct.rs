//! Combinators for handling sequences of [`Punct`](proc_macro::Punct)s (e.g. `&&=`).

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
///     ($t:tt $($char:literal)*) => {
///         let chars: &[char] = &[ $($char),* ];
///         let string = String::from_iter(chars);
///         assert_eq!(stringify!($t), string);
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
    (expand = $($expand:ident)::*, fallback = $_:tt, =)   => { $($expand)::*!(= '=') };
    (expand = $($expand:ident)::*, fallback = $_:tt, <)   => { $($expand)::*!(< '<') };
    (expand = $($expand:ident)::*, fallback = $_:tt, <=)  => { $($expand)::*!(<= '<' '=') };
    (expand = $($expand:ident)::*, fallback = $_:tt, ==)  => { $($expand)::*!(== '=' '=') };
    (expand = $($expand:ident)::*, fallback = $_:tt, !=)  => { $($expand)::*!(!= '!' '=') };
    (expand = $($expand:ident)::*, fallback = $_:tt, >=)  => { $($expand)::*!(>= '>' '=') };
    (expand = $($expand:ident)::*, fallback = $_:tt, >)   => { $($expand)::*!(> '>') };
    (expand = $($expand:ident)::*, fallback = $_:tt, &&)  => { $($expand)::*!(&& '&' '&') };
    (expand = $($expand:ident)::*, fallback = $_:tt, ||)  => { $($expand)::*!(|| '|' '|') };
    (expand = $($expand:ident)::*, fallback = $_:tt, !)   => { $($expand)::*!(! '!') };
    (expand = $($expand:ident)::*, fallback = $_:tt, ~)   => { $($expand)::*!(~ '~') };
    (expand = $($expand:ident)::*, fallback = $_:tt, +)   => { $($expand)::*!(+ '+') };
    (expand = $($expand:ident)::*, fallback = $_:tt, -)   => { $($expand)::*!(- '-') };
    (expand = $($expand:ident)::*, fallback = $_:tt, *)   => { $($expand)::*!(* '*') };
    (expand = $($expand:ident)::*, fallback = $_:tt, /)   => { $($expand)::*!(/ '/') };
    (expand = $($expand:ident)::*, fallback = $_:tt, %)   => { $($expand)::*!(% '%') };
    (expand = $($expand:ident)::*, fallback = $_:tt, ^)   => { $($expand)::*!(^ '^') };
    (expand = $($expand:ident)::*, fallback = $_:tt, &)   => { $($expand)::*!(& '&') };
    (expand = $($expand:ident)::*, fallback = $_:tt, |)   => { $($expand)::*!(| '|') };
    (expand = $($expand:ident)::*, fallback = $_:tt, <<)  => { $($expand)::*!(<< '<' '<') };
    (expand = $($expand:ident)::*, fallback = $_:tt, >>)  => { $($expand)::*!(>> '>' '>') };
    (expand = $($expand:ident)::*, fallback = $_:tt, +=)  => { $($expand)::*!(+= '+' '=') };
    (expand = $($expand:ident)::*, fallback = $_:tt, -=)  => { $($expand)::*!(-= '-' '=') };
    (expand = $($expand:ident)::*, fallback = $_:tt, *=)  => { $($expand)::*!(*= '*' '=') };
    (expand = $($expand:ident)::*, fallback = $_:tt, /=)  => { $($expand)::*!(/= '/' '=') };
    (expand = $($expand:ident)::*, fallback = $_:tt, %=)  => { $($expand)::*!(%= '%' '=') };
    (expand = $($expand:ident)::*, fallback = $_:tt, ^=)  => { $($expand)::*!(^= '^' '=') };
    (expand = $($expand:ident)::*, fallback = $_:tt, &=)  => { $($expand)::*!(&= '&' '=') };
    (expand = $($expand:ident)::*, fallback = $_:tt, |=)  => { $($expand)::*!(|= '|' '=') };
    (expand = $($expand:ident)::*, fallback = $_:tt, <<=) => { $($expand)::*!(<<= '<' '<' '=') };
    (expand = $($expand:ident)::*, fallback = $_:tt, >>=) => { $($expand)::*!(>>= '>' '>' '=') };
    (expand = $($expand:ident)::*, fallback = $_:tt, @)   => { $($expand)::*!(@ '@') };
    (expand = $($expand:ident)::*, fallback = $_:tt, .)   => { $($expand)::*!(. '.') };
    (expand = $($expand:ident)::*, fallback = $_:tt, ..)  => { $($expand)::*!(.. '.' '.') };
    (expand = $($expand:ident)::*, fallback = $_:tt, ...) => { $($expand)::*!(... '.' '.' '.') };
    (expand = $($expand:ident)::*, fallback = $_:tt, ..=) => { $($expand)::*!(..= '.' '.' '=') };
    (expand = $($expand:ident)::*, fallback = $_:tt, ,)   => { $($expand)::*!(, ',') };
    (expand = $($expand:ident)::*, fallback = $_:tt, ;)   => { $($expand)::*!(; ';') };
    (expand = $($expand:ident)::*, fallback = $_:tt, :)   => { $($expand)::*!(: ':') };
    (expand = $($expand:ident)::*, fallback = $_:tt, ::)  => { $($expand)::*!(:: ':' ':') };
    (expand = $($expand:ident)::*, fallback = $_:tt, ->)  => { $($expand)::*!(-> '-' '>') };
    (expand = $($expand:ident)::*, fallback = $_:tt, <-)  => { $($expand)::*!(<- '<' '-') };
    (expand = $($expand:ident)::*, fallback = $_:tt, =>)  => { $($expand)::*!(=> '=' '>') };
    (expand = $($expand:ident)::*, fallback = $_:tt, #)   => { $($expand)::*!(# '#') };
    (expand = $($expand:ident)::*, fallback = $_:tt, ?)   => { $($expand)::*!(? '?') };
    (expand = $($expand:ident)::*, fallback = $_:tt, _)   => { $($expand)::*!(_ '_') };

    (expand = $_0:ident $(::$_1:ident)*, fallback = { $($fallback:tt)* }, $($tt:tt)+) => {
        $($fallback)*
    };
}

/// Expands a macro with every distinct (supported) sequence of punctuation.
#[macro_export]
macro_rules! for_all_punct_seqs {
    ($expand:ident$(, $($args:tt)*)?) => {
        $expand! {
            $($($args)*,)?
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
