Prism.languages.r = {
    'comment': /#.*/,
    'string': {
        pattern: /(['"])(?:\\.|(?!\1)[^\\\r\n])*\1/,
        greedy: true
    },
    'package-prefix': {
        pattern: /\b[a-zA-Z\.][0-9a-zA-Z\._]*(?=:::?)/,
        alias: 'namespace-pkg'
    },
    'namespace-operator': {
        pattern: /:::?/,
        alias: 'operator'
    },
    'native-entry': {
        pattern: /\.(?:Primitive|Internal|Call|External|C|Fortran)\b/,
        alias: 'keyword'
    },
    'percent-operator': {
        // Includes user-defined operators
        // and %%, %*%, %/%, %in%, %o%, %x%
        pattern: /%[^%\s]*%/,
        alias: 'operator'
    },
    'boolean': /\b(?:FALSE|TRUE)\b/,
    'ellipsis': /\.\.(?:\.|\d+)/,
    'number': [
        /\b(?:Inf|NaN)\b/,
        /(?:\b0x[\dA-Fa-f]+(?:\.\d*)?|\b\d+(?:\.\d*)?|\B\.\d+)(?:[EePp][+-]?\d+)?[iL]?/
    ],
    'keyword': /\b(?:NA|NA_character_|NA_complex_|NA_integer_|NA_real_|NULL|break|else|for|function|if|in|next|repeat|while)\b/,
    'operator': /->?>?|<(?:=|<?-)?|[>=!]=?|&&?|\|\|?|[+*\/^$@~]/,
    'punctuation': /[(){}\[\],;]/,
    'bquote': /`[^`\r\n]+`/,
    'binding': /[a-zA-Z\.][0-9a-zA-Z\._]*/
};
