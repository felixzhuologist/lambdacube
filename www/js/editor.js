import React from 'react';

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

export default Editor;
