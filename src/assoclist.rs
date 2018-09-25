// Bare bones implementation of an association list. We use an assoc list
// instead of a std::collections::HashMap since currently performance is not a
// concern and to make the compiled web assembly as small as possible

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct AssocList<K: PartialEq, V: Clone> {
    pub inner: Vec<(K, V)>
}

impl<K: PartialEq, V: Clone> AssocList<K, V> {
    pub fn empty() -> AssocList<K, V> {
        AssocList { inner: Vec::new() }
    }

    pub fn from_vec(pairs: Vec<(K, V)>) -> AssocList<K, V> {
        AssocList { inner: pairs }
    }

    pub fn push(&mut self, key: K, val: V) {
        self.inner.push((key, val));
    }

    pub fn pop(&mut self) {
        self.inner.pop();
    }

    pub fn peek(&self) -> Option<&(K, V)> {
        self.inner.last()
    }

    pub fn lookup(&self, item: &K) -> Option<V> {
        for (key, val) in self.inner.iter().rev() {
            if key == item {
                return Some(val.clone());
            }
        }
        None
    }
}
