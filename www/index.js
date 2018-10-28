const lib = import('./lambdacube');
import "@babel/polyfill";
import React from 'react';
import ReactDOM from 'react-dom';
import App from './js/app';
import sidebars from './js/sidebars';

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
