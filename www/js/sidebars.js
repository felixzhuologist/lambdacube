import React from 'react';

const Simple = () => (
    <div className="sidebar">
        <h6>Typed Lambda Calculus</h6>

        There are two primitive types, {' '}<code>Int</code> and
        {' '} <code>Bool</code>:

        <div className="codeblock">
        <pre>
            <code className="language-typezoo" id="int1">
{`1 + 1
true and false
as
let x = true in (if x then 0 else 1)
fun (x: Int) -> x + 1`}
            </code>
        </pre>
        <a className="run-snippet">Run</a>
        </div>
    </div>
)

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
