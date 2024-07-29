pub struct Punctuated<T, S> {
    pairs: Vec<(T, S)>,
    trailing: Option<T>,
}

impl<T, S> Punctuated<T, S> {
    pub fn new() -> Self {
        Self {
            pairs: Vec::new(),
            trailing: None,
        }
    }

    fn assert_even(&self) {
        if self.trailing.is_some() {
            panic!("expected trailing punctuation where none was found");
        }
    }

    fn assert_odd(&self) {
        if self.trailing.is_none() {
            panic!("found trailing punctuation where none was expected");
        }
    }

    pub fn push_pair(&mut self, value: T, sep: S) {
        self.assert_even();
        self.pairs.push((value, sep));
    }

    pub fn push_value(&mut self, value: T) {
        self.assert_even();
        self.trailing = Some(value);
    }

    pub fn push_sep(&mut self, sep: S) {
        self.assert_odd();
        self.pairs.push((self.trailing.take().unwrap(), sep));
    }

    pub fn pop_sep(&mut self) -> Option<S> {
        if self.trailing.is_none() {
            let Some((value, sep)) = self.pairs.pop() else {
                return None;
            };

            self.trailing = Some(value);
            Some(sep)
        } else {
            None
        }
    }
}

impl<T, S: Default> Punctuated<T, S> {
    pub fn push(&mut self, value: T) {
        if self.trailing.is_some() {
            self.push_sep(S::default());
        }
        self.trailing = Some(value);
    }
}

impl<T, S> Extend<(T, S)> for Punctuated<T, S> {
    fn extend<I: IntoIterator<Item = (T, S)>>(&mut self, iter: I) {
        self.assert_even();
        self.pairs.extend(iter);
    }
}
impl<T, S: Default> Extend<T> for Punctuated<T, S> {
    fn extend<I: IntoIterator<Item = T>>(&mut self, iter: I) {
        if self.trailing.is_some() {
            self.push_sep(S::default());
        }

        self.pairs
            .extend(iter.into_iter().map(|v| (v, S::default())))
    }
}

impl<T, S> IntoIterator for Punctuated<T, S> {
    type Item = (T, Option<S>);
    type IntoIter = PairsOwned<T, S>;

    fn into_iter(self) -> Self::IntoIter {
        PairsOwned {
            inner: self.pairs.into_iter(),
            trailing: self.trailing,
        }
    }
}
impl<'a, T, S> IntoIterator for &'a Punctuated<T, S> {
    type Item = (&'a T, Option<&'a S>);
    type IntoIter = Pairs<'a, T, S>;

    fn into_iter(self) -> Self::IntoIter {
        Pairs {
            inner: self.pairs.iter(),
            trailing: self.trailing.as_ref(),
        }
    }
}

impl<'a, T, S> IntoIterator for &'a mut Punctuated<T, S> {
    type Item = (&'a mut T, Option<&'a mut S>);
    type IntoIter = PairsMut<'a, T, S>;

    fn into_iter(self) -> Self::IntoIter {
        PairsMut {
            inner: self.pairs.iter_mut(),
            trailing: self.trailing.as_mut(),
        }
    }
}

pub struct Pairs<'a, T, S> {
    inner: std::slice::Iter<'a, (T, S)>,
    trailing: Option<&'a T>,
}

impl<'a, T, S> Iterator for Pairs<'a, T, S> {
    type Item = (&'a T, Option<&'a S>);

    fn next(&mut self) -> Option<Self::Item> {
        match self.inner.next() {
            Some((v, s)) => Some((v, Some(s))),
            None => self.trailing.take().map(|v| (v, None)),
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.inner.size_hint()
    }
}

impl<'a, T, S> ExactSizeIterator for Pairs<'a, T, S> {}

pub struct PairsMut<'a, T, S> {
    inner: std::slice::IterMut<'a, (T, S)>,
    trailing: Option<&'a mut T>,
}

impl<'a, T, S> Iterator for PairsMut<'a, T, S> {
    type Item = (&'a mut T, Option<&'a mut S>);

    fn next(&mut self) -> Option<Self::Item> {
        match self.inner.next() {
            Some((v, s)) => Some((v, Some(s))),
            None => self.trailing.take().map(|v| (v, None)),
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.inner.size_hint()
    }
}

impl<'a, T, S> ExactSizeIterator for PairsMut<'a, T, S> {}

pub struct PairsOwned<T, S> {
    inner: std::vec::IntoIter<(T, S)>,
    trailing: Option<T>,
}
impl<T, S> Iterator for PairsOwned<T, S> {
    type Item = (T, Option<S>);

    fn next(&mut self) -> Option<Self::Item> {
        match self.inner.next() {
            Some((v, s)) => Some((v, Some(s))),
            None => self.trailing.take().map(|v| (v, None)),
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.inner.size_hint()
    }
}

impl<T, S> ExactSizeIterator for PairsOwned<T, S> {}
