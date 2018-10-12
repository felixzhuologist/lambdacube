const lib = import('./lambdacube');
const run = async () => {

const { eval_line, eval_program } = await lib;

const jqconsole = $('#console').jqconsole('', '❯❯❯ ');
const startPrompt = () => {
    jqconsole.Prompt(true, (input) => {
        jqconsole.Write(eval_line(input) + '\n', 'jqconsole-output');
        startPrompt();
    })
};

const submitButton = document.getElementById('batch-submit')
submitButton.addEventListener('click', event => {
    const input = document.getElementById("batch-input").value;
    jqconsole.Reset();
    jqconsole.Write(eval_program(input) + '\n', 'jqconsole-output');
    startPrompt();
});

TLN.append_line_numbers('batch-input');
startPrompt();
}

run();
