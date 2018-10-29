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
    const intExample = `1 + 1`;

    return (
        <div className="sidebar">
            <h6>Typed Lambda Calculus</h6>

            There are two primitive types, {' '}<code>Int</code> and
            {' '} <code>Bool</code>:

            <CodeBlock evalLine={props.evalLine} code={intExample} />
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
