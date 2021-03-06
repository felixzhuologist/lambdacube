#[cfg(test)]
pub mod tests {
    use assoclist::AssocList;
    use grammar;
    use syntax::Term::*;
    use syntax::{ArithOp, Binder, Type};

    #[test]
    fn check_parse() {
        assert_eq!(
            grammar::TermParser::new()
                .parse("fun (x: Bool) -> 0")
                .unwrap(),
            Box::new(Abs("x".to_string(), Type::Bool, Box::new(Int(0))))
        );

        assert_eq!(
            grammar::TermParser::new().parse("{a=2, b=true}").unwrap(),
            Box::new(Record(AssocList::from_vec(vec![
                ("a".into(), Int(2)),
                ("b".into(), Bool(true)),
            ])))
        );
    }

    #[test]
    fn check_arith_parse() {
        assert_eq!(
            grammar::TermParser::new().parse("1 + 2 + 3").unwrap(),
            Box::new(Arith(
                Box::new(Int(1)),
                ArithOp::Add,
                Box::new(Arith(
                    Box::new(Int(2)),
                    ArithOp::Add,
                    Box::new(Int(3))
                ))
            ))
        );

        assert_eq!(
            grammar::TermParser::new().parse("1 * 2 + 3").unwrap(),
            Box::new(Arith(
                Box::new(Arith(
                    Box::new(Int(1)),
                    ArithOp::Mul,
                    Box::new(Int(2))
                )),
                ArithOp::Add,
                Box::new(Int(3)),
            ))
        );
    }

    #[test]
    fn check_type_parse() {
        assert_eq!(
            grammar::TypeParser::new().parse("Int").unwrap(),
            Box::new(Type::Int)
        );

        assert_eq!(
            grammar::TypeParser::new().parse("Int -> Bool").unwrap(),
            Box::new(Type::Arr(Box::new(Type::Int), Box::new(Type::Bool)))
        );

        assert_eq!(
            grammar::TypeParser::new()
                .parse("Int -> (Bool -> Bool)")
                .unwrap(),
            Box::new(Type::Arr(
                Box::new(Type::Int),
                Box::new(Type::Arr(Box::new(Type::Bool), Box::new(Type::Bool)))
            ))
        );

        assert_eq!(
            grammar::TypeParser::new()
                .parse("{a: Int, b: Bool}")
                .unwrap(),
            Box::new(Type::Record(AssocList::from_vec(vec![
                ("a".into(), Type::Int),
                ("b".into(), Type::Bool),
            ])))
        );

        assert!(
            grammar::TypeParser::new()
                .parse("forall X . (X -> X)")
                .is_ok()
        );
        assert!(
            grammar::TypeParser::new()
                .parse("forall X, Y <: Int . (X -> Y)")
                .is_ok()
        );
        assert!(
            grammar::TypeParser::new()
                .parse("forall X: * -> *, Y . (X -> Y)")
                .is_ok()
        );
        assert!(
            grammar::TypeParser::new()
                .parse("tyfun (X: * -> *) => X -> X")
                .is_ok()
        );
        assert!(
            grammar::TypeParser::new()
                .parse("(tyfun (X: *) => X) Int")
                .is_ok()
        );
        assert!(
            grammar::TypeParser::new()
                .parse("tyfun (A: *) (B: *) => forall X . (A -> B -> X) -> X")
                .is_ok()
        );
    }

    #[test]
    fn check_binder_parse() {
        assert_eq!(
            grammar::BinderParser::new()
                .parse("type IntFunc = Int -> Int")
                .unwrap(),
            Binder::TyBind(
                "IntFunc".into(),
                Type::Arr(Box::new(Type::Int), Box::new(Type::Int))
            )
        );

        assert_eq!(
            grammar::BinderParser::new()
                .parse("let myrec = {a=2}")
                .unwrap(),
            Binder::VarBind(
                "myrec".into(),
                Record(AssocList::from_vec(vec![("a".into(), Int(2))]))
            )
        );

        assert!(grammar::BinderParser::new().parse("let x = 3").is_ok());
        assert!(grammar::BinderParser::new().parse("let f x = x").is_ok());
        assert!(
            grammar::BinderParser::new()
                .parse("let g (x: Int) y = y + x")
                .is_ok()
        );

        assert!(
            grammar::BinderParser::new()
                .parse("open mymod as bla: Bla")
                .is_ok()
        )
    }

    #[test]
    fn check_univ() {
        assert!(
            grammar::TermParser::new()
                .parse("fun[X] (x: X) -> x")
                .is_ok()
        );
        assert!(grammar::TermParser::new().parse("polyfunc[X]").is_ok());
        assert!(grammar::TermParser::new().parse("polyfunc[X] 0").is_ok());
        assert!(
            grammar::TermParser::new()
                .parse("polyfunc[T, U, V] 0")
                .is_ok()
        );
        assert!(
            grammar::TermParser::new()
                .parse("fun[X, Y] (x: X) (y: Y) -> x")
                .is_ok()
        );
        assert!(grammar::TermParser::new().parse("myfunc[X, Y] x y").is_ok());
    }

    #[test]
    pub fn check_exis() {
        let module = "module sig
                type Counter
                val new : Counter
                val get : Counter -> Int
                val inc : Counter -> Counter
            end";

        assert!(grammar::TypeParser::new().parse(module).is_ok());

        let module_binder = "type CounterADT = module sig
                type Counter
                val new : Counter
                val get : Counter -> Int
                val inc : Counter -> Counter
            end";
        assert!(grammar::BinderParser::new().parse(module_binder).is_ok());

        let pack = "module ops
            type Int
            val new = 1
            val get = fun (x: Int) -> x
            val inc = fun (x: Int) -> x + 1
        end as (CounterADT)";
        assert!(grammar::TermParser::new().parse(pack).is_ok());

        let pack_binder = "let counterADT = module ops
            type Int
            val new = 1
            val get = fun (x: Int) -> x
            val inc = fun (x: Int) -> x + 1
        end as (CounterADT)";
        assert!(grammar::BinderParser::new().parse(pack_binder).is_ok());

        assert!(grammar::TermParser::new().parse("counterADT").is_ok());
        assert!(grammar::IdentParser::new().parse("counter").is_ok());
        assert!(grammar::TyIdentParser::new().parse("Counter").is_ok());
        assert!(grammar::TermParser::new().parse("counter.new").is_ok());
        let unpack = "open counterADT as counter: Counter in counter.new";
        assert!(grammar::TermParser::new().parse(unpack).is_ok());
    }

    #[test]
    fn multiarg() {
        assert!(grammar::TermParser::new().parse("fun x -> x").is_ok());
        assert!(grammar::TermParser::new().parse("fun x y -> x + y").is_ok());
        assert!(
            grammar::TermParser::new()
                .parse("fun x (y: Int) -> x + y")
                .is_ok()
        );
    }

    #[test]
    fn test_bounded_univ() {
        assert!(
            grammar::TermParser::new()
                .parse("fun[X <: Int] (x: X) -> x")
                .is_ok()
        );
        assert!(grammar::TermParser::new().parse("polyfunc[X]").is_ok());
        assert!(grammar::TermParser::new().parse("polyfunc[X] 0").is_ok());
        assert!(
            grammar::TermParser::new()
                .parse("polyfunc[T, U, V] 0")
                .is_ok()
        );
        assert!(
            grammar::TermParser::new()
                .parse("fun[X, Y <: {a: Int}] (x: X) (y: Y) -> x")
                .is_ok()
        );
        assert!(grammar::TermParser::new().parse("myfunc[X, Y] x y").is_ok());
    }

    #[test]
    fn test_kinded_univ() {
        assert!(
            grammar::TermParser::new()
                .parse("fun[X: *] (x: X) -> x")
                .is_ok()
        );
    }

    #[test]
    fn kinds() {
        assert!(grammar::KindParser::new().parse("*").is_ok());
        assert!(grammar::KindParser::new().parse("(*)").is_ok());
        assert!(grammar::KindParser::new().parse("* -> *").is_ok());
        assert!(grammar::KindParser::new().parse("* -> (* -> *)").is_ok());
    }

    #[test]
    fn substructural() {
        assert!(grammar::TermParser::new().parse("lin 0").is_ok());
        assert!(
            grammar::TermParser::new()
                .parse("lin {a=lin 0, b=true}")
                .is_ok()
        );
        assert!(
            grammar::TermParser::new()
                .parse("fun (x: Int) -> x + 1")
                .is_ok()
        );
        assert!(
            grammar::TypeParser::new()
                .parse("lin (Int -> lin Bool)")
                .is_ok()
        );
        assert!(grammar::TypeParser::new().parse("lin (lin Bool").is_err());
        assert!(
            grammar::TypeParser::new()
                .parse("lin {x: lin Int, b: Bool}")
                .is_ok()
        );
    }
}
