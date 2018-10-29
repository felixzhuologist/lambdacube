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

const SysF = () => (
    <div className="sidebar">
        <h6>System F</h6>

    </div>
)

const Higher = () => (
    <div className="sidebar">
        <h6>Higher Order Types</h6>

    </div>
)

const FSub = () => (
    <div className="sidebar">
        <h6>System F Sub</h6>

    </div>
)

const FOmega = () => (
    <div className="sidebar">
        <h6>System F Omega</h6>

    </div>
)

const FOmegaSub = () => (
    <div className="sidebar">
        <h6>System F Omega Sub</h6>

    </div>
)

const Linear = () => (
    <div className="sidebar">
        <h6>Linear Types</h6>

    </div>
)

const sidebars = {
    simple: Simple,
    subtyping: Subtyping,
    hm: HM,
    sysf: SysF,
    higher: Higher,
    ['subtyping-sysf']: FSub,
    ['higher-sysf']: FOmega,
    ['higher-subtyping-sysf']: FOmegaSub,
    // TODO: this shouldn't be a valid combination
    ['higher-subtyping']: FOmegaSub,
    linear: Linear,
};

export default sidebars;
