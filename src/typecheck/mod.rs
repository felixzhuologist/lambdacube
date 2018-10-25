/// takes a typechecking function that doesn't care about kinds and defines a
/// typecheck_top function that takes in an unused KindContext arg and just calls
/// the input typechecker
#[macro_export]
macro_rules! export_kindless_typechecker(
    ($typecheck:ident) => (
    pub fn typecheck_top (
        term: &Term,
        type_ctx: &mut ::assoclist::TypeContext,
        _kind_ctx: &mut ::assoclist::KindContext,
    ) -> Result<::syntax::ty::Type, ::errors::TypeError> {
        $typecheck(term, type_ctx)
    }
));

pub mod fomega;
pub mod fomsub;
pub mod fsub;
pub mod hm;
pub mod omega;
pub mod simple;
pub mod sub;
pub mod substructural;
pub mod sysf;
