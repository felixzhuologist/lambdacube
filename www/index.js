import "@babel/polyfill";
import React from 'react';
import ReactDOM from 'react-dom';

const run = async () => {
const lib = import('./lambdacube');

const { eval_code } = await lib;
console.log(eval_code("22 * 10 + 3"));
ReactDOM.render(
    <p>hello</p>,
    document.getElementById('root')
)

}

run();
