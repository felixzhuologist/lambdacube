const lib = import('./lambdacube');
const run = async () => {

const { eval_line, eval_program, set_typechecker } = await lib;

const featureSelector = $('#feature-selector');
const features = [
    { value: 'subtyping', enabled: false, display: 'Subtyping' },
    { value: 'hm', enabled: false, display: 'Hindley-Milner' },
    { value: 'universal', enabled: false, display: 'Universal types' },
    { value: 'existential', enabled: false, display: 'Existential types' },
    { value: 'higher', enabled: false, display: 'Higher order types' },
    { value: 'dependent', enabled: false, display: 'Dependent types' },
    { value: 'linear', enabled: false, display: 'Linear types' },
];
// each element of combinations represents a set of features that can be used
// together. e.g. [true, false, true] means that feature 1 and 3 can be used
// together
const combinations = [
    [true, false, false, false, false, false, false],
    [false, true, false, false, false, false, false],
];

const updateOptions = () => {
    // list of indices of enabled features
    const enabled = Object.keys(features)
        .map((feature, i) => ({ enabled: features[feature].enabled, idx: i }))
        .filter(({ enabled, idx }) => enabled)
        .map(({ enabled, idx }) => idx);

    const valid = combinations.filter(vals => enabled.every(idx => vals[idx]));

    const options = $('#feature-selector > option');
    for (let i = 0; i < features.length; i++) {
        options[i].disabled = !valid.some(combo => combo[i]);
    }
    featureSelector.selectpicker('refresh');
}

const initOptions = () => {
    features.forEach((feature) => {
        featureSelector.append(new Option(feature.display, feature.value));
    });
    updateOptions();
    featureSelector.selectpicker('refresh');
}

const serialize_features = () => {
    let result = 0;
    features.forEach((feature, idx) => {
        if (feature.enabled) {
            result |= (1 << idx);
        }
    });
    return result;
}

featureSelector[0].addEventListener('change', event => {
    selected = featureSelector.val();
    features.forEach(feature => {
        feature.enabled = selected.some(val => feature.value === val);
    })
    // TODO: debounce this?
    set_typechecker(serialize_features());
    jqconsole.Reset();
    startPrompt();
    updateOptions();
});

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
initOptions();
}

run();
