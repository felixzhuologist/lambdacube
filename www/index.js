const lib = import('./lambdacube');

const run = async () => {

const { eval_simple } = await lib;

const submitButton = document.getElementById('repl-submit')
submitButton.addEventListener('click', event => {
    const result = document.getElementById("result");
    const input = document.getElementById("repl-input").value;
    result.innerHTML = eval_simple(input);
})

}

run();
