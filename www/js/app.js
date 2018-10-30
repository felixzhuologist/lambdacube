import React from 'react';
import Editor from './editor';
import Navbar from './navbar';

// TODO: friendlier error messages when trying to evaluate a type, or add
// something like haskell's :t and :k
class App extends React.Component {
    constructor(props) {
        super(props);
        this.state = {
            features: [
                { id: 0, value: 'subtyping', isDisabled: false, label: 'Subtyping' },
                { id: 1, value: 'hm', isDisabled: false, label: 'Hindley-Milner' },
                { id: 2, value: 'sysf', isDisabled: false, label: 'System F' },
                { id: 3, value: 'higher', isDisabled: false, label: 'Higher order types' },
                { id: 4, value: 'dependent', isDisabled: true, label: 'Dependent types (coming soon)' },
                { id: 5, value: 'linear', isDisabled: false, label: 'Linear types' },
                { id: 6, value: 'effect', isDisabled: true, label: 'Effect types (coming soon)' }
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

    evalLine(code) {
        if (code === '') {
            return;
        }
        let result = this.props.evalLine(code) + '\n';
        this.jqconsole.SetPromptText(code);
        this.jqconsole.AbortPrompt();
        this.jqconsole.Write(result, 'jqconsole-output');
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
                <div className="col-4" style={{ paddingLeft: '3%', paddingRight: 0 }}>
                    <Sidebar evalLine={(code) => this.evalLine(code)} />
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

export default App;
