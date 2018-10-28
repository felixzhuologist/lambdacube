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
                // TODO: calling jqconsole.Reset() onChange causes the
                // closeMenuOnSelect behaviour to not work, but this doesn't
                // handle all possible updates either (e.g. delete a label)
                onMenuClose={() => { props.resetConsole(); }}
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
        Prism.highlightAll();
    }

    startPrompt() {
        this.jqconsole.Prompt(true, (input) => {
            let result = input ? this.props.evalLine(input) + '\n' : '';
            this.jqconsole.Write(result, 'jqconsole-output');
            this.startPrompt();
        })
    }

    updateOptions(enabled) {
        this.props.setTypechecker(this.serializeFeatures(enabled));

        const valid = this.combinations.filter(vals =>
            enabled.every(({ id }) => vals[id]));
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

    evalProgram(code) {
        this.jqconsole.Reset();
        this.jqconsole.Write(this.props.evalProgram(code) + '\n', 'jqconsole-output');
        this.startPrompt();
    }

    render() {
        let Sidebar = this.props.sidebars[this.state.sidebarId];
        return ([
            <Navbar
                features={this.state.features}
                updateOptions={(enabled) => this.updateOptions(enabled)}
                resetConsole={() => {
                    this.jqconsole.Reset();
                    this.startPrompt();
                }}
                key="nav"/>,
            <div className="row" key="body" style={{ height: 'calc(100% - 124px)' }}>
                <div className="col-4" style={{ paddingLeft: '3%' }}>
                    <Sidebar />
                </div>
                <div className="col-4">
                    <Editor evalProgram={(code) => this.evalProgram(code)} />
                </div>
                <div className="col-4">
                    <div id="console"></div>
                </div>
            </div>,
            <footer key="footer">
                <div className="row">
                    <div style={{ margin: 'auto' }}>
                        <a
                            target="_blank"
                            href='http://www.recurse.com'
                            title='Made with love at the Recurse Center'
                            style={{ float: 'right' }}>
                            <img src='https://bit.ly/2EPoFFg' height='20px'/>
                        </a>
                    </div>
                </div>
            </footer>
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

// A prism syntax highlighted textarea based on https://bit.ly/2D6KmPz
// TODO: work beyond 20 lines
class Editor extends React.Component {
    constructor(props) {
        super(props);
        this.state = { text: '' }
    }

    componentDidUpdate() {
        Prism.highlightAll();
    }

    handleChange(event) {
        this.setState({ text: event.target.value });
    }

    // indent instead of unfocus when tab is pressed
    handleKeyDown(event) {
        if (event.keyCode !== 9) {
            return;
        }
        event.preventDefault();
        let text = this.state.text;
        let start = event.target.selectionStart;
        let end = event.target.selectionEnd;
        this.setState(
            { text: text.substring(0, start) + '\t' + text.substring(end) },
            () => {
                this.refs.input.selectionStart = start + 1;
                this.refs.input.selectionEnd = start + 1;
            }
        )
    }

    // TODO: would it be more robust to use setCaretPosition rather than CSS to
    // keep the caret at the right spot?
    render() {
        return (
            <div className="editor">
                <pre id="editor" className="line-numbers">
                    <code className="language-typezoo">
                        {this.state.text}
                    </code>
                    <textarea
                        ref="input"
                        spellCheck={false}
                        id="editor-content"
                        value={this.state.text}
                        onKeyDown={(e) => this.handleKeyDown(e)}
                        onChange={(e) => this.handleChange(e)}
                    ></textarea>
                    <a
                        className="run-snippet"
                        onClick={() => this.props.evalProgram(this.state.text)}
                    >
                        Run
                    </a>
                </pre>
            </div>
        )
    }
}

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
            setTypechecker={set_typechecker}
            evalLine={eval_line}
            evalProgram={eval_program}
            sidebars={sidebars}
        />,
        document.getElementById('root')
    )
}

run();
