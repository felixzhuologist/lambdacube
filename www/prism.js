// TODO: get this actually working
Prism.languages.typezoo = {
    'number': /\b[0-9]+/,
    'type': {
        pattern: /\b[A-Z]\w*\b/,
        alias: 'function'
    },
    'variable': /\b[a-z_]\w*/,
    'keyword': /\b(?:let|open|as|type|if|then|else|in|lin|ops|module|lambda|fun|λ|tyfun|sig|val|end|∀|forall|poly)\b/,
    'boolean': /\b(?:false|true)\b/,
    'operator': /\b(?:and|or|not|=|->|=>|<:)\b/,
    'punctuation': /[(){}\[\].,:;]/
};
