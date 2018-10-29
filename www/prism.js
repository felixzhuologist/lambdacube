// for some reason aliases don't seem to work
Prism.languages.typezoo = {
    'number': /\b[0-9]+\b/,
    'keyword': /\b(?:let|open|as|type|if|then|else|in|lin|ops|module|lambda|fun|λ|sig|val|end|∀|forall|poly)\b/,
    'boolean': /\b(?:false|true)\b/,
    // operator
    'symbol': /\band|or|not|=|->|=>|<:|\+|-|<|<=|>|>=|%|\/|\*\b/,
    // type
    'string': /\b[A-Z]\w*|\btyfun/,
    'variable': /[a-z_]\w*/,
    'punctuation': /[(){}\[\].,:;]/
};
