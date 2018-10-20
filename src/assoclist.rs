// Bare bones implementation of an association list. We use an assoc list
// instead of a std::collections::HashMap since currently performance is not a
// concern and to make the compiled web assembly as small as possible
use errors::TypeError;
use eval::{Eval, EvalStep};
use std::fmt;
use std::str::FromStr;
use syntax::{Kind, Substitutable, Term, Type};
use typecheck::simple::Resolve;

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

    pub fn lookup(&self, item: &K) -> Option<V> {
        for (key, val) in self.inner.iter().rev() {
            if key == item {
                return Some(val.clone());
            }
        }
        None
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

// the second element is an option because we might not care what the sort
// is during evaluation and just want to add a value to the context
pub type TermContext = AssocList<String, (Term, Option<Type>)>;
pub type TypeContext = AssocList<String, (Type, Option<Kind>)>;

pub trait Context<Val, Sort> {
    fn get_val(&self, name: &String) -> Option<Val>;
    fn add_val(&self, name: &String, val: Val);
    fn get_sort(&self, name: &String) -> Option<Sort>;
    fn add_sort(&self, name: &String, sort: Sort);
}

impl<Val, Sort> Context<Val, Sort> for AssocList<String, (Val, Option<Sort>)>
where
    Val: Clone + FromStr,
    <Val as FromStr>::Err: fmt::Debug,
    Sort: Clone
{
    fn get_val(&self, name: &String) -> Option<Val> {
        self.lookup(name).map(|p| p.0)
    }

    fn add_val(&self, name: &String, val: Val) {
        self.push(name.clone(), (val, None))
    }

    fn get_sort(&self, name: &String) -> Option<Sort> {
        self.lookup(name).map(|p| p.1.unwrap())
    }

    fn add_sort(&self, name: &String, sort: Sort) {
        self.push(name.clone(), (name.parse().unwrap(), Some(sort)))
    }
}

impl AssocList<String, (Term, Option<Type>)> {
    pub fn map_typecheck(
        &self,
        tc: fn(&Term, &mut TermContext, &mut TypeContext) -> Result<Type, TypeError>,
        term_ctx: &mut TermContext,
        type_ctx: &mut TypeContext,
    ) -> Result<AssocList<String, Type>, TypeError> {
        self.map_val(|(t, _)| tc(&t.unwrap(), term_ctx, type_ctx))
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

impl<T, S, Err> Eval<T, S, Err> for AssocList<String, (T, S)>
where
    T: Clone + Eval<T, S, Err>,
    S: Clone,
{
    fn eval(&self, ctx: &mut AssocList<String, (T, Option<S>)>) -> Result<Self, Err> {
        self.map_val(|(t, _)| t.eval(ctx))
    }
}

impl<T, S, Err> EvalStep<T, S, Err> for AssocList<String, (T, S)>
where
    T: Clone + EvalStep<T, S, Err>,
    S: Clone,
{
    // TODO: should probably step one field at a time instead of stepping them
    // all forward at once
    fn eval_step(&self, ctx: &mut AssocList<String, (T, Option<S>)>) -> Result<Self, Err> {
        self.map_val(|(t, _)| t.eval_step(ctx))
    }
}

impl Resolve for AssocList<String, Type> {
    fn resolve(&self, ctx: &mut TypeContext) -> Result<Self, TypeError> {
        self.map_val(|t| t.resolve(ctx))
    }
}
