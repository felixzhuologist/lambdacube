import React from 'react';

class CodeBlock extends React.Component {
    constructor(props) {
        super(props);
    }

    runSnippet() {
        this.props.evalLine(this.refs.code.innerText);
    }

    render() {
        return (
            <div className="codeblock">
                <pre>
                    <code ref="code" className="language-typezoo">
                        {`${this.props.code}`}
                    </code>
                </pre>
                <a className="run-snippet" onClick={() => this.runSnippet()}>
                    Run
                </a>
            </div>
        )
    }
}

const Simple = (props) => {
    const intExample = `(13 - 2) * 4 - 6 % 3`;
    const boolExample = `false or (true and false)`;
    const letExample = `let x = 1 in x + 1`;
    const letExample2 = `let x = 3 in let y = 5 in x - y`;
    const ifExample = `if true then 1 else 0`;
    const recExample = `let info = {age=30, is_tall=false} in info.age`;
    const simpleFunc = `fun (x: Bool) -> if x then 1 else 0`;
    const letFunc =
`let add = fun (x: Int) (y: Int) -> x + y in
let addone = add 1 in
addone 20`;

    return (
        <div className="sidebar">
            <h6>Simply Typed Lambda Calculus</h6>

            The language we'll be using is a small extension of the {' '}
            <a
                href="https://en.wikipedia.org/wiki/Simply_typed_lambda_calculus"
                target="_blank">
                simply typed lambda calculus
            </a>, with an {' '}
            <a
                href="https://en.wikipedia.org/wiki/ML_(programming_language)"
                target="_blank">
                ML
            </a>
            -like syntax. It's a very simple language so that the amount of
            syntax you need to know is small - here's an overview of what you
            can do:<br /><br />

            The two primitive types are {' '}<code>Int</code>:
            <CodeBlock evalLine={props.evalLine} code={intExample} />
            and {' '}<code>Bool</code>:
            <CodeBlock evalLine={props.evalLine} code={boolExample} /><br />

            You can set local variables using a {' '}<code>let</code> statement:
            <CodeBlock evalLine={props.evalLine} code={letExample} />
            Remember that the variable you define is only in scope for whatever
            code you write after {' '}<code>in</code>! If you want to define
            multiple variables, you can use multiple let statements:
            <CodeBlock evalLine={props.evalLine} code={letExample2} /><br />

            You can define a function using the {' '}<code>fun</code> keyword:
            <CodeBlock evalLine={props.evalLine} code={simpleFunc} />
            Note that each argument must be wrapped in parentheses and be
            annotated with a type. You can add multiple arguments to a function,
            which will automatically be curried:
            <CodeBlock evalLine={props.evalLine} code={letFunc} /><br />

            To conditionally evaluate some code, you can use an {' '}<code>
            if then/else</code> expression
            <CodeBlock evalLine={props.evalLine} code={ifExample} /><br />

            Finally, there is also a built in record data structure, which
            maps keys to values:
            <CodeBlock evalLine={props.evalLine} code={recExample} /><br />

            That's all there is to it! Feel free to play around in the REPL on
            the right, or try and write some more complicated programs in the
            text editor. Once you feel comfortable with the syntax, pick a type
            feature from the dropdown to start learning about types!
        </div>
    )
}

const Subtyping = (props) => {
    const grades = `let grades = {english=80, math=60, art=72}`;
    const avg =
`let avg = fun (grades: {english: Int, math: Int}) ->
    (grades.english + grades.math) / 2`;
    const result = `avg grades`

    return (
        <div className="sidebar">
            <h6>Subtyping</h6>

            Let's say we were working with the grades of students of a class, which
            we represent as a record
            <CodeBlock evalLine={props.evalLine} code={grades} />
            and we wanted to write a function that returns the average of the only
            the english and math classes
            <CodeBlock evalLine={props.evalLine} code={avg} />
            If you run this code under the simply typed lambda calculus, you'll notice
            that calling this function on the record we defined earlier leads to a
            type error: {' '}<code>{`Expected argument of type
            {english: Int, math: Int} but got {english: Int, math: Int, art: Int}
            instead`} </code>!<br /><br />

            This seems silly - in a sense, we're providing the
            function more than what it needs which is causing it to fail. This
            is where subtyping comes in - type systems with subtyping are able to
            "forget" about certain aspects of a type if it has more than what we
            need. If we now try to run this code, we get the result as expected:
            <CodeBlock evalLine={props.evalLine} code={result} /><br />

            More formally, this is because we define a subtyping relationship,
            usually denoted as {' '}<code>{`{english: Int, math: Int, art: Int} <:
            {english: Int, math: Int}`}</code>, which says that whenever we see
            the type on the left of the {' '}<code>{`<:`}</code>, it's ok to replace it
            with the type on the right.<br /><br />

            The concept of subtyping is central to object-oriented programming
            languages, which let you use a subclass wherever its parent class is
            expected
        </div>
    )
}

const HM = (props) => {
    const toint =
`let bool_to_int (b: Bool) =
    if b then 1 else 0`;
    const doubleint =
`let double_int (f: Int -> Int) (x: Int) =
    f (f x)`;
    const doublebool =
`let double_bool (f: Bool -> Bool) (x: Bool) =
    f (f x)`;
    const tointInfer =
`let bool_to_int b =
    if b then 1 else 0`;
    const infer = `fun z y -> z (y true)`;
    const doublepoly =
`let id_int = fun (x: Int) -> x in
let id_bool = fun (x: Bool) -> x in
let double = fun f x -> f (f x) in
double id_int 0`;

    return (
        <div className="sidebar">
            <h6>Hindley-Milner</h6>

            So far you might have been annoyed with two things about our toy
            programming language. <br/><br/>

            The first one is the need to annotate all of
            the arguments of functions with types, even when the type is obvious!
            For example, in the following function
            <CodeBlock evalLine={props.evalLine} code={toint} />
            it should be clear that the argument must be a {' '}<code>Bool</code>,
            because otherwise it would be a type error!<br/><br/>

            The second one is that if we wanted to write a function that can
            operate on multiple types, we need to define two versions:
            <CodeBlock evalLine={props.evalLine} code={doubleint} />
            <CodeBlock evalLine={props.evalLine} code={doublebool} /><br/>

            Luckily, both of these things are addressed in the {' '}
            <a href="https://en.wikipedia.org/wiki/Hindley–Milner_type_system"
               target="_blank"
            >
            Hindley-Milner
            </a>
            {' '} type system! By using a type inference algorithm, we are able to
            deduce the types of arguments:
            <CodeBlock evalLine={props.evalLine} code={tointInfer} />
            An important aspect of this type inference algorithm is that it will
            infer the most general type possible. For example, in the following
            function:
            <CodeBlock evalLine={props.evalLine} code={infer} />
            the type checker is able to deduce that {' '}<code>y</code> must take a
            {' '}<code>Bool</code> and that {' '}<code>z</code> must take the output
            type of {' '}<code>y</code> as input, but besides that keep the types
            as general as possible using the {' '}<code>X?</code> placeholder values.
            <br/><br/>

            Hindley-Milner solves the second issue by allowing a form of parametric
            polymorphism (also called referred to as "generics" or just "polymorphism"
            depending on which language you're coming from), called let-polymorphism.
            Using let-polymorphism, we can create a version of our
            {' '}<code>double</code> function that works on multiple types:
            <CodeBlock evalLine={props.evalLine} code={doublepoly} />
        </div>
    )
}

const SysF = (props) => {
    const universal = `type Id = ∀X . X -> X`;
    const idfunc = `let id = fun[T] (x: T) -> x`;
    const tyapp = `let f = id[Int] in f 0`;
    const impredic = `fun (x: forall X . X -> X) -> x[Int] 0`;
    const exist =
`∃Counter.
    new: Counter,
    get: (Counter -> Int),
    inc: (Counter -> Counter)`;
    const counterty =
`type CounterADT = module sig
    type Counter
    val new : Counter
    val get : Counter -> Int
    val inc : Counter -> Counter
end`;
    const counterimpl =
`let counterADT = module ops
    type Int
    val new = 1
    val get = fun (x: Int) -> x
    val inc = fun (x: Int) -> x + 1
end as (CounterADT)`;
    const counterEx =
`open counterADT as counter: Counter in
    counter.get (counter.inc counter.new)`;
    const counterimplrec =
`let counterADT = module ops
    type {inner: Int}
    val new = {inner=1}
    val get = fun (x: {inner: Int}) -> x.inner
    val inc = fun (x: {inner: Int}) -> {inner=x.inner + 1}
end as (CounterADT)`;

    return (
        <div className="sidebar">
            <h6>System F</h6>

            System F is a commonly studied type systems that extends the simply
            typed lambda calculus with two features: universally quantified types and
            existentially quantified types.<br/><br/>

            Universally quantified types (or universal types for short) get their
            name from the universal quantifier in logic. In fact, you can find a
            universal quantifier in the type itself:
            <CodeBlock evalLine={props.evalLine} code={universal} />
            this type describes a value that can take any type {' '}<code>X</code>{' '}
            and return a function from {' '}<code>X -> X</code>. The simplest
            example of a value with such a type would be the identity function:
            <CodeBlock evalLine={props.evalLine} code={idfunc} />
            If we apply {' '}<code>id</code> to a type, say {' '}<code>Int</code>,
            then we get a function that operates on ints:
            <CodeBlock evalLine={props.evalLine} code={tyapp} /><br/>

            If you have already played with Hindley-Milner, you may be wondering
            what the advantage of universal types are over the let polymorphism
            we saw earlier. The type of polymorphism in System F is what is known
            as <i>impredicative</i> polymorphism (as opposed
            to the <i>predicative</i> polymorphism of HM).
            What this essentially means is that System F has "first-class"
            polymorphism where we can pass around values with universal types:
            <CodeBlock evalLine={props.evalLine} code={impredic} />
            which isn't possible in Hindley-Milner. This extra expressiveness
            comes at a cost however - type inference has been proven to be
            undecidable for System F. This is a tradeoff that
            programming languages with parametric polymorphism must
            deal with: we can give up first class polymorphism to get full type
            inference, or we can take first class polymorphism but settle for
            less powerful type inference<br/><br/>

            Existential types also get their name from the corresponding logical
            quantifier, but the meaning may be less obvious. Here's an example
            of an existential type:
            <CodeBlock evalLine={props.evalLine} code={exist} />
            what this says is that there exists <i>some</i> type
            {' '}<code>Counter</code>{' '} that we as the user don't have access to,
            with functions {' '}<code>new, get</code>,  and {' '}<code>inc</code>{' '}
            that can operate on that type. The key is that a existential types
            provide a mechanism for abstracting away the inner representation
            and implementation of an interface, which lets us define abstract
            data types. <br/><br/>

            The actual syntax for defining the type we mentioned above looks
            like the following (we give it the name {' '}<code>CounterADT</code>
            {' '} to be able to refer to it later)
            <CodeBlock evalLine={props.evalLine} code={counterty} />
            and a possible implementation would be
            <CodeBlock evalLine={props.evalLine} code={counterimpl} />
            Now that we have an implementation of our {' '}<code>CounterADT</code>,
            we need to explicitly "unpack" it into a variable (which will hold
            the concrete implementation) and a type variable (which is a
            placeholder for the abstract type hidden inside the existential type)
            using the {' '}<code>open in</code>{' '} language construct
            <CodeBlock evalLine={props.evalLine} code={counterEx} /><br/>

            The benefit of an existential type is that we could decide to change
            the implementation of the abstract data type - let's say we represent
            the {' '}<code>Counter</code>{' '} using a {' '}<code>Record</code>
            {' '} instead of an {' '}<code>Int</code>{' '}
            <CodeBlock evalLine={props.evalLine} code={counterimpl} />
            the interface that is exposed to the rest of the world would
            remain the same, and our code from before would work with our new
            implementation
            <CodeBlock evalLine={props.evalLine} code={counterEx} /><br/>
        </div>
    )
}

const Higher = (props) => {
    const tyfun = `type Ra = tyfun (X: *) => {a: X}`;
    const badterm = `true 0`;
    const badty = `type Bad = Bool Int`;

    return (
        <div className="sidebar">
            <h6>Higher Order Types</h6>

            A type operator is a function that takes in a type and returns a type.
            In order to distinguish from regular functions, we can define a type
            operator using {' '}<code>tyfun</code>{' '} instead of {' '}<code>fun</code>{' '}
            <CodeBlock evalLine={props.evalLine} code={tyfun} />
            We also use a fat arrow instead of a regular arrow to denote that the
            function returns a type and not a value.<br/><br/>

            Similarly to how we need to typecheck terms to prevent nonsensical
            terms like
            <CodeBlock evalLine={props.evalLine} code={badterm} /><br />
            with the introduction of type operators we need to prevent nonsensical
            types like
            <CodeBlock evalLine={props.evalLine} code={badty} /><br />
            We do this by introducing the equivalent of a higher order "type" for
            types, called kinds. All types we have seen so far like {' '}<code>Int</code>{' '}
            are of kind {' '}<code>*</code>{' '} (pronounced "star"), since they
            do not take any arguments. A type operator that accepts one argument
            and returns a type of kind star would have kind
            {' '}<code>* -> *</code>{' '}, and a type operator that accepts a star
            argument and returns a {' '}<code>* -> *</code>{' '} would have type
            {' '}<code>* -> * -> *</code>{' '}, and so on...
        </div>
    )
}

const FSub = (props) => {
    const boundeduni =
`let f = fun[T <: {a: Int}] (x: T) ->
    {a=x.a + 1, b=x}`;
    const call = `f[{a: Int, b: Int}] {a=1, b=2}`;
    return (
        <div className="sidebar">
            <h6>System F Sub</h6>

            Although subtyping and the quantifier types from system F can coexist
            perfectly, by combining them we can obtain a more expressive type
            system with bounded quantification. Here's an example
            <CodeBlock evalLine={props.evalLine} code={boundeduni} />
            In the above code, we specify that the function can take any type
            {' '}<code>T</code>{' '} so long as it's a subtype of
            {' '}<code>{`{a: Int}`}</code>{' '}. This is more powerful than just
            using subtyping, since we don't "forget" any of the information about
            {' '}<code>T</code>. For example, if we call the function like so
            <CodeBlock evalLine={props.evalLine} code={call} />
            our result type is still contains the {' '}<code>b</code>{' '} field
            which would be lost otherwise.<br/><br/>
        </div>
    )
}

const FOmega = (props) => {
    const typair =
`type Pair = tyfun (A: *) (B: *) =>
    ∀X: *. (A -> B -> X) -> X`
    const pairfunc =
`let pair = fun[A: *, B: *] (x: A) (y: B) ->
    (fun[X: *] (z: A -> B -> X) -> z x y)`;
    const fst =
`let fst = fun[A: *, B: *] (p: Pair A B) ->
    p[A] (fun (x: A) (y: B) -> x)`;
    const snd =
`let snd = fun[A: *, B: *] (p: Pair A B) ->
    p[B] (fun (x: A) (y: B) -> y)`;
    const usage1 =
`let p = pair[Int, Bool] 1 true in
    fst[Int, Bool] p`;
    const usage2 =
`let p = pair[Int, Bool] 1 true in
    snd[Int, Bool] p`;

    return (
        <div className="sidebar">
            <h6>System F Omega</h6>

            In system F omega, type operators and the quanitifier types interact in
            in the expected way, so there isn't too much new to show here. Instead,
            let's take advantage of the features to add a pair data structure to
            our language using {' '}
            <a href="google.com"
               target="_blank">
            Church Pairs
            </a>.
            First we can define a type operator that will create a pair type with
            the given type parameters:
            <CodeBlock evalLine={props.evalLine} code={typair} />
            then we can define our pair function,
            <CodeBlock evalLine={props.evalLine} code={pairfunc} />
            a getter function for the first element of the pair,
            <CodeBlock evalLine={props.evalLine} code={fst} />
            and one for the second element of the pair
            <CodeBlock evalLine={props.evalLine} code={snd} />
            We can now use our new pair
            <CodeBlock evalLine={props.evalLine} code={usage1} />
            <CodeBlock evalLine={props.evalLine} code={usage2} />
        </div>
    )
}

const FOmegaSub = (props) => {
    const counterty =
`type Counter = module sig
    type T
    val state : T
    val methods : {
        get: T -> Int,
        inc: T -> T
    }
end`;
    const counterimpl =
`let counter = module ops
    type {x: Nat}
    val state = {x=5}
    val methods = {
        get = fun (r: {x: Nat}) -> r.x,
        inc = fun (r: {x: Nat}) -> {x=r.x + 1}
    }
end as (Counter)`;

    return (
        <div className="sidebar">
            <h6>System F Omega Sub</h6>

            With subtyping, polymorphism, and type operators all together, we
            can do some pretty exciting things. Let's look at an example
            program which leverages all of these features (based on ch. 32 of
            TAPL).<br/><br/>

            Let's start with the counter abstract data type we defined in the
            section on system F. This time, to make it more "class" like, we
            separate out the state and the methods of the module.
            <CodeBlock evalLine={props.evalLine} code={counterty} />
            and we'll implement it with a record as the concrete type
            <CodeBlock evalLine={props.evalLine} code={counterimpl} />
        </div>
    )
}

const Linear = (props) => {
    const linex = `lin 1 + 1`;
    const validrec = `lin {linear=lin 0, unrestricted=0}`;
    const badrec = `{linear=lin 0, unrestricted=0}`;
    const usezero = `let x = lin 0 in 20`;
    const useonce = `let x = lin 0 in x + 20`;
    const usetwice = `let x = lin 10 in x + x`;
    return (
        <div className="sidebar">
            <h6>Linear Types</h6>

            Linear types belong to a family of type systems known as
            substructural type systems, which are type systems with types that
            can restrict the number of times variables are used as well as the
            order that they are used in. Linear types restrict a variable to be
            used exactly once, but places no restrictions on the order.
            <br/><br/>

            In this current implementation, values are unrestricted (you can
            use them as many times as you want) by default, but you can declare
            a linear value using the {' '}<code>lin</code>{' '} keyword
            <CodeBlock evalLine={props.evalLine} code={linex} /><br/>
            For unrestricted nested values (like records and functions), all
            inner values must be unrestricted as well, but linear nested values
            can have either linear or unrestricted values. For example,
            the follow record is valid
            <CodeBlock evalLine={props.evalLine} code={validrec} />
            but this one is not
            <CodeBlock evalLine={props.evalLine} code={badrec} /><br/>

            Once you've declared a linear variable, the typechecker will ensure
            that it only gets used once. For example, try running the following
            programs and seeing if they typecheck as you would expect
            <CodeBlock evalLine={props.evalLine} code={usezero} />
            <CodeBlock evalLine={props.evalLine} code={useonce} />
            <CodeBlock evalLine={props.evalLine} code={usetwice} /><br />

            So why is it useful to restrict the number of times a variable is
            used through the type system? There are a number of applications of
            linear types (and substructural types in general). One is that
            making large heap allocated values linear removes the need for a
            garbage collector - if the value is linear the compiler knows it can
            deallocate the value right after it's used. Another is the way to
            enforce state transitions at compile time using types. For example,
            if we want to start listening on a socket, we first need to bind it
            to an address before we can start listening on it. If we
            define the socket states using linear types, and use functions that
            return linear types:
            <pre>
                <code className="language-typezoo">
{`bind: lin UnboundSocket -> lin BoundSocket
listen: lin BoundSocket -> lin ListeningSocket`}
                </code>
            </pre>
            it makes it impossible to call {' '}<code>listen</code>{' '} on an
            unbound socket, since the unbound socket gets consumed when we bind
            it. The same strategy can be used to prevent trying to close a file
            that is already closed, or use a generator or stream that has already
            been consumed.
        </div>
    )
}

const sidebars = {
    simple: Simple,
    subtyping: Subtyping,
    hm: HM,
    sysf: SysF,
    higher: Higher,
    'subtyping-sysf': FSub,
    'higher-sysf': FOmega,
    'higher-subtyping-sysf': FOmegaSub,
    // TODO: this shouldn't be a valid combination
    'higher-subtyping': FOmegaSub,
    linear: Linear,
};

export default sidebars;
