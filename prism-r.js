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


// Token classification hook in Prism
function analyzeTokens(env) {
    const tokens = env.tokens;
    const isC = env.language === 'c' || env.language === 'clike';

    if (isC) {
        for (let i = 0; i < tokens.length; i++) {
            if (tokens[i].type === 'function' || (typeof tokens[i] === 'string' && /^[A-Za-z0-9_]+$/.test(tokens[i]))) {
                // Check if followed by (
                let j = i + 1;
                while (j < tokens.length && typeof tokens[j] === 'string' && /^\s*$/.test(tokens[j])) j++;
                if (j < tokens.length && (tokens[j] === '(' || (tokens[j].content && tokens[j].content === '('))) {
                    if (typeof tokens[i] === 'string') {
                        tokens[i] = new Prism.Token('c-func', tokens[i], 'c-func');
                    } else {
                        tokens[i].type = 'c-func';
                    }
                }
            }
        }
        return;
    }

    // R Token Analysis
    for (let i = 0; i < tokens.length; i++) {
        const tok = tokens[i];

        // 1. Qualified calls: pkg::fun or pkg:::fun
        if (tok.type === 'package-prefix' && (i + 2) < tokens.length) {
            const opTok = tokens[i + 1];
            const funTok = tokens[i + 2];
            if (opTok.type === 'namespace-operator' && funTok.type === 'binding') {
                funTok.type = 'qualified-closure';
                funTok.targetPkg = tok.content;
            }
        }

        // 2. Native entries: .Primitive("foo") or .Internal(foo(...))
        if (tok.type === 'native-entry') {

            const entryName = tok.content;
            let j = i + 1;
            while (j < tokens.length && typeof tokens[j] === 'string' && /^\s*$/.test(tokens[j])) j++;
            if (j < tokens.length && (tokens[j] === '(' || (tokens[j].content && tokens[j].content === '('))) {
                let k = j + 1;
                while (k < tokens.length && typeof tokens[k] === 'string' && /^\s*$/.test(tokens[k])) k++;
                if (k < tokens.length) {
                    const argTok = tokens[k];
                    if (entryName === '.Primitive') {
                        if (argTok.type === 'string') {
                            argTok.type = 'primitive-link';
                            argTok.targetName = argTok.content.replace(/^['"]|['"]$/g, '');
                        }
                    } else if (entryName === '.Internal') {
                        if (argTok.type === 'binding') {
                            argTok.type = 'primitive-link';
                            argTok.targetName = argTok.content;
                        }
                    } else if (entryName === '.Call' || entryName === '.External' || entryName === '.C' || entryName === '.Fortran') {
                        if (argTok.type === 'binding' || argTok.type === 'string') {
                            argTok.type = 'routine-link';
                            argTok.targetName = argTok.content.replace(/^['"]|['"]$/g, '');
                        }
                    }
                }
            }

        }

        // 3. Regular function calls: binding followed by (
        if (tok.type === 'binding') {
            let j = i + 1;
            while (j < tokens.length && typeof tokens[j] === 'string' && /^\s*$/.test(tokens[j])) j++;
            if (j < tokens.length && (tokens[j] === '(' || (tokens[j].content && tokens[j].content === '('))) {
                tok.type = 'closure';
            }
        }
    }

    // console.log("analyzeTokens: end");
    // let toShow = ['primitive-link', 'routine-link', 'qualified-closure'];
    // for (let i = 0; i < tokens.length; i++) {
    //     const tok = tokens[i];
    // 	if (toShow.includes(tok.type)) console.log(tok);
    // }

}

// Token wrapping hook: transform tokens into hyperlinks
function wrapTokenLink(env) {

    if (env.type === 'primitive-link') {
        const sym = env.targetName || env.content.replace(/^['"]|['"]$/g, '');
        if (manifest && manifest.r_primitives && manifest.r_primitives[sym]) {
            const prim = manifest.r_primitives[sym];
            env.tag = 'a';
            env.attributes.href = `display.html?pkg=_core_c&file=${encodeURIComponent(prim.file)}#line.${prim.line}`;
            env.attributes.title = `R Core C Primitive: ${prim.c_symbol} (${prim.file}:${prim.line})`;
            env.attributes.class = 'token-c-link';
        }
        return;
    }

    if (env.type === 'routine-link') {
        const sym = env.targetName || env.content.replace(/^['"]|['"]$/g, '');
        let rt = null;
        if (currentPkgData && currentPkgData.c_routines) {
            rt = currentPkgData.c_routines[sym];
            if (!rt && sym.startsWith('C_')) {
                rt = currentPkgData.c_routines[sym.substring(2)];
            }
        }
        if (rt && rt.file) {
            env.tag = 'a';
            env.attributes.href = `display.html?pkg=${currentPkg}&file=${encodeURIComponent(rt.file)}#line.${rt.line}`;
            env.attributes.title = `C Routine: ${rt.c_symbol} (${rt.file}:${rt.line})`;
            env.attributes.class = 'token-c-link';
        }
        return;
    }

    if (env.type === 'qualified-closure') {
        const targetPkg = env.targetPkg;
        const sym = env.content;
        env.tag = 'a';
        env.attributes.href = `display.html?pkg=${encodeURIComponent(targetPkg)}&sym=${encodeURIComponent(sym)}`;
        env.attributes.title = `Qualified call: ${targetPkg}::${sym}`;
        return;
    }

    if (env.type === 'closure') {
        const sym = env.content;

        // 1. Check local package definitions
        if (currentPkgData && currentPkgData.r_symbols && currentPkgData.r_symbols[sym]) {
            const loc = currentPkgData.r_symbols[sym];
            env.tag = 'a';
            env.attributes.href = `display.html?pkg=${currentPkg}&file=${encodeURIComponent(loc.file)}#line.${loc.line}`;
            env.attributes.title = `${currentPkg} (local): line ${loc.line}`;
            return;
        }

        // 2. Check importFrom in current package
        if (currentPkgData && currentPkgData.import_from && currentPkgData.import_from[sym]) {
            const impPkg = currentPkgData.import_from[sym];
            env.tag = 'a';
            env.attributes.href = `display.html?pkg=${encodeURIComponent(impPkg)}&sym=${encodeURIComponent(sym)}`;
            env.attributes.title = `Imported from ${impPkg}`;
            return;
        }

        // 3. Check core primitive
        if (manifest && manifest.r_primitives && manifest.r_primitives[sym]) {
            const prim = manifest.r_primitives[sym];
            env.tag = 'a';
            env.attributes.href = `display.html?pkg=_core_c&file=${encodeURIComponent(prim.file)}#line.${prim.line}`;
            env.attributes.title = `Base primitive -> C: ${prim.c_symbol}`;
            env.attributes.class = 'token-c-link';
            return;
        }

        // 4. Check dependencies of current package
        if (manifest && manifest.packages && manifest.packages[currentPkg]) {
            const deps = manifest.packages[currentPkg].depends || [];
            for (const d of deps) {
                if (manifest.exports && manifest.exports[sym] && manifest.exports[sym].includes(d)) {
                    env.tag = 'a';
                    env.attributes.href = `display.html?pkg=${encodeURIComponent(d)}&sym=${encodeURIComponent(sym)}`;
                    env.attributes.title = `Exported by dependency '${d}'`;
                    return;
                }
            }
        }

        // 5. Check global exports registry
        if (manifest && manifest.exports && manifest.exports[sym]) {
            let pkgs = manifest.exports[sym];
	    pkgs = Array.isArray(pkgs) ? pkgs : [pkgs];
            const target = pkgs.includes('base') ? 'base' : pkgs[0];
            env.tag = 'a';
            env.attributes.href = `display.html?pkg=${encodeURIComponent(target)}&sym=${encodeURIComponent(sym)}`;
            env.attributes.title = pkgs.length > 1 ? `Exported by: ${pkgs.join(', ')}` : `Exported by ${target}`;
            return;
        }
    }

    // C function call
    if (env.type === 'c-func') {
        const sym = env.content;
        // Check current package C symbols
        if (currentPkgData && currentPkgData.c_symbols && currentPkgData.c_symbols[sym]) {
            const loc = currentPkgData.c_symbols[sym];
            env.tag = 'a';
            env.attributes.href = `display.html?pkg=${currentPkg}&file=${encodeURIComponent(loc.file)}#line.${loc.line}`;
            env.attributes.title = `${currentPkg} C function: line ${loc.line}`;
            return;
        }
        // Check Core C symbols
        if (coreCData && coreCData.c_symbols && coreCData.c_symbols[sym]) {
            const loc = coreCData.c_symbols[sym];
            env.tag = 'a';
            env.attributes.href = `display.html?pkg=_core_c&file=${encodeURIComponent(loc.file)}#line.${loc.line}`;
            env.attributes.title = `R Core C: line ${loc.line}`;
            return;
        }
    }
}

