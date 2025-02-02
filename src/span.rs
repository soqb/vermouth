use std::ops::{Deref, DerefMut};

use proc_macro::{Ident, Span, TokenTree};

/// Enables conversion between different span formats.
///
/// # Implementors
///
/// This trait is notably implemented for:
/// * [`Span`] itself
/// * [`Spanned<T>`]
/// * [`TokenTree`]
pub trait ToSpan {
    /// Converts a type into a [span](Span).
    fn span(&self) -> Span;
}

impl ToSpan for Span {
    #[inline]
    fn span(&self) -> Span {
        *self
    }
}

impl ToSpan for TokenTree {
    #[inline]
    fn span(&self) -> Span {
        self.span()
    }
}

impl<T> ToSpan for Spanned<T> {
    #[inline]
    fn span(&self) -> Span {
        self.span
    }
}

/// A simple wrapper which provides span information for any value.
///
/// Often it is preferrable to include span information on types directly,
/// but this type can be useful when the contained values aren't *explicitly*
/// linked to syntax.
///
/// # Examples
///
/// ```
/// # vermouth::ඞ_declare_test!();
/// #
/// use proc_macro::{Ident, Span};
/// use vermouth::{Spanned, Expected, ToSpan};
///
/// fn is_continue_kw(kw: Spanned<&str>) -> bool {
///     kw == "continue"
/// }
///
/// let ident = Ident::new("continue", Span::call_site());
/// let spanned: Spanned<String> = ident.into();
/// assert!(is_continue_kw(spanned.as_deref()));
/// ```
pub struct Spanned<T> {
    inner: T,
    span: Span,
}

impl<R, T: PartialEq<R>> PartialEq<R> for Spanned<T> {
    fn eq(&self, other: &R) -> bool {
        self.inner == *other
    }
}

impl From<Ident> for Spanned<String> {
    fn from(value: Ident) -> Self {
        Spanned::new(value.span(), value.to_string())
    }
}

impl<T: Clone> Clone for Spanned<T> {
    fn clone(&self) -> Self {
        Self {
            inner: self.inner.clone(),
            span: self.span,
        }
    }
}

impl<T: Copy> Copy for Spanned<T> {}

impl<T> Spanned<T> {
    /// Creates a new [`Spanned`] object.
    pub fn new(span: impl ToSpan, inner: T) -> Self {
        Self {
            inner,
            span: span.span(),
        }
    }

    /// Unwraps the contained value of a [`Spanned`] object.
    pub fn into_inner(self) -> T {
        self.inner
    }
    /// [Dereferences] the value stored in this `Spanned` value.
    ///
    /// [Dereferences]: Deref
    pub fn as_deref(&self) -> Spanned<&T::Target>
    where
        T: Deref,
    {
        Spanned::new(self.span, self.deref())
    }

    /// [Mutably dereferences] the value stored in this `Spanned` value.
    ///
    /// [Mutably dereferences]: DerefMut
    pub fn as_deref_mut(&mut self) -> Spanned<&mut T::Target>
    where
        T: DerefMut,
    {
        Spanned::new(self.span, self.deref_mut())
    }
}

impl<T> From<(Span, T)> for Spanned<T> {
    fn from((span, inner): (Span, T)) -> Self {
        Self::new(span, inner)
    }
}

impl<T> Deref for Spanned<T> {
    type Target = T;

    #[inline]
    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<T> DerefMut for Spanned<T> {
    #[inline]
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}
