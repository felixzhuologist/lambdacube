const lib = import('./lambdacube');
import "@babel/polyfill";
import React from 'react';
import ReactDOM from 'react-dom';
import Select from 'react-select';

const Navbar = (props) => (
    <nav className="navbar navbar-expand-lg justify-content-between">
      <div className="navbrand">
        <span className="navbar-brand mb-0 h1" style={{ fontSize: '1.5em' }}>
            Type Zoo
        </span>
      </div>
      <div className="navactions">
        <ul className="navbar-nav">
          <li className="nav-item" style={{ width: '30em' }}>
            <Select
                isMulti
                placeholder="Pick some type features"
                closeMenuOnSelect={false}
                escapeClearsValue={true}
                hideSelectedOptions={false}
                onChange={(enabled) => { props.updateOptions(enabled); }}
                name="features"
                options={props.features}
                className="basic-multi-select"
                classNamePrefix="select"
            />
          </li>
        </ul>
      </div>
    </nav>
)

class App extends React.Component {
    constructor(props) {
        super(props);
        this.state = {
            features: [
                { id: 0, value: 'subtyping', isDisabled: false, label: 'Subtyping' },
                { id: 1, value: 'hm', isDisabled: false, label: 'Hindley-Milner' },
                { id: 2, value: 'sysf', isDisabled: false, label: 'System F' },
                { id: 3, value: 'higher', isDisabled: false, label: 'Higher order types' },
                { id: 4, value: 'dependent', isDisabled: true, label: 'Dependent types' },
                { id: 5, value: 'linear', isDisabled: false, label: 'Linear types' }
            ],
            sidebarId: 'simple',
        };

        // each element of combinations represents a set of features that can be used
        // together. e.g. [true, false, true] means that feature 0 and 2 can be used
        // together
        this.combinations = [
            [true, false, true, false, false, false],
            [false, true, false, false, false, false],
            [true, false, true, true, false, false],
            [false, false, false, false, false, true],
        ];
        this.jqconsole = null;
    }

    componentDidMount() {
        this.jqconsole = $('#console').jqconsole('', '❯❯❯ ');
        this.startPrompt();

        Prism.highlightAll();
    }

    componentDidUpdate() {
        this.jqconsole.Reset();
        this.startPrompt();
        Prism.highlightAll();
    }

    startPrompt() {
        this.jqconsole.Prompt(true, (input) => {
            let result = input ? this.props.eval_line(input) + '\n' : '';
            this.jqconsole.Write(result, 'jqconsole-output');
            this.startPrompt();
        })
    }

    updateOptions(enabled) {
        this.props.set_typechecker(this.serializeFeatures(enabled));

        const valid = this.combinations.filter(vals =>
            enabled.every(({ id }) => vals[id]));
        console.log(enabled.map(f => f.value).sort().join('-') || 'simple');
        this.setState((state) => ({
            sidebarId: enabled.map(f => f.value).sort().join('-') || 'simple',
            features: state.features.map((feature, i) => ({
                ...feature,
                isDisabled: !valid.some(combo => combo[i]),
            }))
        }))
    }

    serializeFeatures(enabled) {
        return enabled.map(({ id }) => 1 << id).reduce((l, r) => l | r, 0);
    }

    render() {
        let Sidebar = this.props.sidebars[this.state.sidebarId];
        return ([
            <Navbar
                features={this.state.features}
                updateOptions={(enabled) => this.updateOptions(enabled)}
                key="nav"/>,
            <div className="row h-75" key="body">
                <div className="col-3">
                    <Sidebar />
                </div>
                <div className="col" style={{ paddingLeft: 0 }}>
                    <Editor />
                </div>
                <div className="col">
                    <div id="console"></div>
                </div>
            </div>
        ])
    }
}

const SidebarSimple = () => (
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

const SidebarSubtyping = () => (
    <div className="sidebar">
        <h6>Subtyping</h6>

    </div>
)

const SidebarHM = () => (
    <div className="sidebar">
        <h6>Hindley-Milner</h6>

    </div>
)

const SidebarSysF = () => (
    <div className="sidebar">
        <h6>System F</h6>

    </div>
)

const SidebarHigher = () => (
    <div className="sidebar">
        <h6>Higher Order Types</h6>

    </div>
)

const SidebarFSub = () => (
    <div className="sidebar">
        <h6>System F Sub</h6>

    </div>
)

const SidebarFOmega = () => (
    <div className="sidebar">
        <h6>System F Omega</h6>

    </div>
)

const SidebarFOmegaSub = () => (
    <div className="sidebar">
        <h6>System F Omega Sub</h6>

    </div>
)

const SidebarLinear = () => (
    <div className="sidebar">
        <h6>Linear Types</h6>

    </div>
)

const Editor = () => (
    <div className="editor">
        <textarea id="batch-input"></textarea>
    </div>
)

const sidebars = {
    simple: SidebarSimple,
    subtyping: SidebarSubtyping,
    hm: SidebarHM,
    sysf: SidebarSysF,
    higher: SidebarHigher,
    ['subtyping-sysf']: SidebarFSub,
    ['higher-sysf']: SidebarFOmega,
    ['higher-subtyping-sysf']: SidebarFOmegaSub,
    // TODO: this shouldn't be a valid combination
    ['higher-subtyping']: SidebarFOmegaSub,
    linear: SidebarLinear,
};

const run = async () => {

const { eval_line, eval_program, set_typechecker } = await lib;

ReactDOM.render(
    <App
        set_typechecker={set_typechecker}
        eval_line={eval_line}
        sidebars={sidebars}
    />,
    document.getElementById('root')
)

TLN.append_line_numbers('batch-input');
}

run();
