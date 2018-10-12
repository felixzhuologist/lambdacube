const lib = import('./lambdacube');
const run = async () => {

const { eval_simple } = await lib;

const jqconsole = $('#console').jqconsole('', '❯❯❯ ');
const startPrompt = () => {
    jqconsole.Prompt(true, (input) => {
        jqconsole.Write(eval_simple(input) + '\n', 'jqconsole-output');
        startPrompt();
    })
};

const submitButton = document.getElementById('batch-submit')
submitButton.addEventListener('click', event => {
    const input = document.getElementById("batch-input").value;
    jqconsole.Reset();
    jqconsole.Write(eval_simple(input) + '\n', 'jqconsole-output');
    startPrompt();
});

TLN.append_line_numbers('batch-input');
startPrompt();
}

run();
