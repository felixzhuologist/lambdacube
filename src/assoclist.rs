use errors::TypeError;
use eval::{Eval, EvalStep};
use std::fmt;
use syntax::{Kind, Substitutable, Term, Type};
use typecheck::simple::Resolve;

// we esssentially need to store 4 things:
// 1. the value of term variables (e.g. "x" has value 3)
// 2. the type of term variables (e.g. "x" has type Int)
// 3. the value of type variables (e.g. "X" represents an Int)
// 4. the kind of type variables (e.g. "X" has kind *)
// items 2 and 3 both have the same type (String -> Type) and their set of valid
// keys are disjoint so we store both of them in TypeContext. this might be a bit
// confusing but it keeps the code simpler for now (I think)
pub type TermContext = AssocList<String, Term>;
pub type TypeContext = AssocList<String, Type>;
pub type KindContext = AssocList<String, Kind>;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct AssocList<K: PartialEq, V: Clone> {
    pub inner: Vec<(K, V)>,
}

impl<K: Clone + PartialEq, V: Clone> AssocList<K, V> {
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

    pub fn remove(&mut self, item: &K) -> Option<V> {
        self.find(item).map(|idx| self.inner.remove(idx).1)
    }

    pub fn lookup(&self, item: &K) -> Option<V> {
        self.find(item).map(|idx| self.inner[idx].1.clone())
    }

    fn find(&self, item: &K) -> Option<usize> {
        self.inner
            .iter()
            .rev()
            .position(|(key, _)| key == item)
            .map(|idx| self.inner.len() - 1 - idx)
    }

    pub fn map_val<T, E, F>(&self, mut func: F) -> Result<AssocList<K, T>, E>
    where
        T: Clone,
        F: FnMut(&V) -> Result<T, E>,
    {
        let result: Result<Vec<(K, T)>, E> = self
            .inner
            .iter()
            .map(|(key, val)| func(val).map(|res| (key.clone(), res)))
            .collect();
        result.map(|vec| AssocList::from_vec(vec))
    }
}

// TODO: how to avoid the code duplication here?
impl AssocList<String, Term> {
    pub fn map_typecheck(
        &self,
        tc: fn(&Term, &mut TypeContext) -> Result<Type, TypeError>,
        ctx: &mut TypeContext,
    ) -> Result<AssocList<String, Type>, TypeError> {
        self.map_val(|ty| tc(ty, ctx))
    }
}

impl AssocList<String, Term> {
    pub fn map_typecheck_kind(
        &self,
        tc: fn(&Term, &mut TypeContext, &mut KindContext)
            -> Result<Type, TypeError>,
        type_ctx: &mut TypeContext,
        kind_ctx: &mut KindContext,
    ) -> Result<AssocList<String, Type>, TypeError> {
        self.map_val(|ty| tc(ty, type_ctx, kind_ctx))
    }
}

impl fmt::Display for AssocList<String, Term> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            self.inner
                .iter()
                .map(|(k, v)| format!("{}={}", k, v))
                .collect::<Vec<String>>()
                .join(", ")
        )
    }
}

impl fmt::Display for AssocList<String, Type> {
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

impl<T: Clone + Substitutable<T>> Substitutable<T> for AssocList<String, T> {
    fn applysubst(self, varname: &str, var: &T) -> AssocList<String, T> {
        AssocList::from_vec(
            self.inner
                .into_iter()
                .map(|(field, val)| (field, val.applysubst(varname, var)))
                .collect(),
        )
    }
}

impl<T, Err> Eval<T, Err> for AssocList<String, T>
where
    T: Clone + Eval<T, Err>,
{
    fn eval(&self, ctx: &mut AssocList<String, T>) -> Result<Self, Err> {
        self.map_val(|t| t.eval(ctx))
    }
}

impl<T, Err> EvalStep<T, Err> for AssocList<String, T>
where
    T: Clone + EvalStep<T, Err>,
{
    // TODO: should probably step one field at a time instead of stepping them
    // all forward at once
    fn eval_step(&self, ctx: &mut AssocList<String, T>) -> Result<Self, Err> {
        self.map_val(|t| t.eval_step(ctx))
    }
}

impl Resolve for AssocList<String, Type> {
    fn resolve(&self, ctx: &mut TypeContext) -> Result<Self, TypeError> {
        self.map_val(|t| t.resolve(ctx))
    }
}
