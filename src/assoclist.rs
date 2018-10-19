// Bare bones implementation of an association list. We use an assoc list
// instead of a std::collections::HashMap since currently performance is not a
// concern and to make the compiled web assembly as small as possible
use errors::TypeError;
use eval::{Eval, EvalStep};
use std::fmt;
use syntax::{Kind, Substitutable, Term, Type};
use typecheck::simple::Resolve;

pub type TermContext = AssocList<String, Term>;
pub type TypeContext = AssocList<String, Type>;
pub type KindContext = AssocList<String, Kind>;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct AssocList<K: PartialEq, V: Clone> {
    pub inner: Vec<(K, V)>,
}

impl<K: PartialEq, V: Clone> AssocList<K, V> {
    pub const fn empty() -> AssocList<K, V> {
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

impl fmt::Display for AssocList<String, Box<Type>> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            self.inner
                .iter()
                .map(|(k, v)| format!("{}: {}", k, v))
                .collect::<Vec<String>>()
                .join(", ")
        )
    }
}

impl<T: Clone + Substitutable<T>> Substitutable<T>
    for AssocList<String, Box<T>>
{
    fn applysubst(self, varname: &str, var: &T) -> AssocList<String, Box<T>> {
        AssocList::from_vec(
            self.inner
                .into_iter()
                .map(|(field, box val)| {
                    (field, Box::new(val.applysubst(varname, var)))
                }).collect(),
        )
    }
}

// TODO: how to avoid code duplication in these impls...?
impl<T, Err> Eval<T, Err> for AssocList<String, Box<T>>
where
    T: Clone + Eval<T, Err>,
{
    fn eval(&self, ctx: &mut AssocList<String, T>) -> Result<Self, Err> {
        let result: Result<Vec<_>, _> = self
            .inner
            .iter()
            .map(|(key, ref val)| {
                val.eval(ctx).map(|t| (key.clone(), Box::new(t)))
            }).collect();
        result.map(|vec| AssocList::from_vec(vec))
    }
}

impl<T, Err> EvalStep<T, Err> for AssocList<String, Box<T>>
where
    T: Clone + EvalStep<T, Err>,
{
    // TODO: should probably step one field at a time instead of stepping them
    // all forward at once
    fn eval_step(&self, ctx: &mut AssocList<String, T>) -> Result<Self, Err> {
        let result: Result<Vec<_>, _> = self
            .inner
            .iter()
            .map(|(key, ref val)| {
                val.eval_step(ctx).map(|t| (key.clone(), Box::new(t)))
            }).collect();
        result.map(|vec| AssocList::from_vec(vec))
    }
}

impl Resolve for AssocList<String, Box<Type>> {
    fn resolve(&self, ctx: &mut TypeContext) -> Result<Self, TypeError> {
        let result: Result<Vec<_>, _> = self
            .inner
            .iter()
            .map(|(s, val)| val.resolve(ctx).map(|t| (s.clone(), Box::new(t))))
            .collect();
        result.map(|vec| AssocList::from_vec(vec))
    }
}
