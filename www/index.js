import "@babel/polyfill";
import React from 'react';
import ReactDOM from 'react-dom';
import PropTypes from 'prop-types';

const Repl = (props) => (
    <div>
        <textarea id="repl-input">
        </textarea>
        <input
            type="submit"
            value="run"
            onClick={() => {
                const result = document.getElementById("result");
                const input = document.getElementById("repl-input").value;
                result.innerHTML = props.eval_code(input);
            }}
        />
        <p id="result" />
    </div>
)

Repl.propTypes = {
    eval_code: PropTypes.func.isRequired
};

const run = async () => {
const lib = import('./lambdacube');

const { eval_code } = await lib;
ReactDOM.render(
    <Repl eval_code={eval_code} />,
    document.getElementById('root')
)

}

run();
