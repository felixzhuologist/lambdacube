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
            <h6>Typed Lambda Calculus</h6>

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

const Subtyping = () => (
    <div className="sidebar">
        <h6>Subtyping</h6>

    </div>
)

const HM = () => (
    <div className="sidebar">
        <h6>Hindley-Milner</h6>

    </div>
)

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
