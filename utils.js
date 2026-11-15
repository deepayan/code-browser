
var manifest = null;
var currentPkg = 'base';
var currentPkgData = null;
var loadedPackages = {};
var coreCData = null;


async function loadManifest() {
    try {
        const resp = await fetch('metadata/manifest.json');
        if (resp.ok) {
            manifest = await resp.json();
        }
    } catch (e) {
        console.error('Failed to load manifest.json', e);
    }
}

async function getPackageData(pkgName) {
    if (loadedPackages[pkgName]) return loadedPackages[pkgName];
    try {
        const resp = await fetch(`metadata/packages/${pkgName}.json`);
        if (resp.ok) {
            const data = await resp.json();
            loadedPackages[pkgName] = data;
            return data;
        }
    } catch (e) {
        console.warn(`Could not load metadata for ${pkgName}`, e);
    }
    return null;
}

function inferPackageFromFile(filepath) {
    if (!filepath) return 'base';
    if (filepath.startsWith('library/')) {
        const parts = filepath.split('/');
        return parts[1];
    }
    if (filepath.startsWith('src/library/')) {
        const parts = filepath.split('/');
        return parts[2];
    }
    if (filepath.startsWith('main/') || filepath.startsWith('appl/') ||
        filepath.startsWith('nmath/') || filepath.startsWith('include/') ||
        filepath.startsWith('src/main/') || filepath.startsWith('src/include/')) {
        return '_core_c';
    }
    return 'base';
}

function getRawGithubUrl(pkg, filepath) {
    const pkgInfo = manifest && manifest.packages ? manifest.packages[pkg] : null;
    if (pkg === '_core_c' || (pkgInfo && pkgInfo.is_base)) {
        let cleanPath = filepath.startsWith('src/') ? filepath.substring(4) : filepath;
        return `https://raw.githubusercontent.com/r-devel/r-svn/trunk/src/${cleanPath}`;
    } else if (pkgInfo && !pkgInfo.is_base) {
        const repo = pkgInfo.repo || `cran/${pkg}`;
        const branch = pkgInfo.branch || 'master';
        let cleanPath = filepath.startsWith('/') ? filepath.substring(1) : filepath;
        return `https://raw.githubusercontent.com/${repo}/${branch}/${cleanPath}`;
    } else {
        // Fallback heuristic
        if (filepath.startsWith('library/') || filepath.startsWith('main/') || filepath.startsWith('appl/') || filepath.startsWith('include/')) {
            return `https://raw.githubusercontent.com/r-devel/r-svn/trunk/src/${filepath}`;
        }
        return `https://raw.githubusercontent.com/cran/${pkg}/master/${filepath}`;
    }
}

async function resolveSymbolAndNavigate(pkg, sym) {
    const data = await getPackageData(pkg);
    if (data && data.r_symbols && data.r_symbols[sym]) {
        const loc = data.r_symbols[sym];
        window.location.href = `display.html?pkg=${pkg}&file=${encodeURIComponent(loc.file)}#line.${loc.line}`;
        return;
    }
    if (data && data.c_symbols && data.c_symbols[sym]) {
        const loc = data.c_symbols[sym];
        window.location.href = `display.html?pkg=${pkg}&file=${encodeURIComponent(loc.file)}#line.${loc.line}`;
        return;
    }
    document.getElementById('error-msg').innerText = `Symbol '${sym}' not found in package '${pkg}'.`;
    document.getElementById('error-msg').style.display = 'block';
}

async function loadFileContent(pkg, filepath, lineNum) {
    document.getElementById('loading').style.display = 'block';
    document.getElementById('error-msg').style.display = 'none';

    const rawUrl = getRawGithubUrl(pkg, filepath);
    const isC = /\.(c|h|cpp|cc|hpp|hh)$/i.test(filepath);
    const lang = isC ? 'c' : 'r';

    try {
	const resp = await fetch(rawUrl);
	if (!resp.ok) {
            throw new Error(`HTTP ${resp.status} fetching ${rawUrl}`);
	}
	const text = await resp.text();

	const pre = document.getElementById('line');
	pre.className = `line-numbers language-${lang}`;
	if (lineNum) {
            pre.setAttribute('data-line', lineNum);
	} else {
            pre.removeAttribute('data-line');
	}

	const codeBlock = document.getElementById('code-block');
	codeBlock.className = `language-${lang}`;
	codeBlock.textContent = text;

	Prism.highlightElement(codeBlock);

	document.getElementById('loading').style.display = 'none';

	// Scroll to highlighted line
	setTimeout(() => {
            if (lineNum) {
		const hl = document.querySelector('.line-highlight') ||
                      document.querySelector(`.line-numbers-rows > span:nth-child(${lineNum})`);
		if (hl) {
                    hl.scrollIntoView({ behavior: 'smooth', block: 'center' });
		}
            }
	}, 150);

    } catch (err) {
        document.getElementById('loading').style.display = 'none';
        const errDiv = document.getElementById('error-msg');
        errDiv.innerHTML = `<strong>Error loading file:</strong> ${err.message}<br><small>URL: <a href="${rawUrl}" target="_blank">${rawUrl}</a></small>`;
        errDiv.style.display = 'block';
    }
}

function navigateToFile() {
    const file = document.getElementById('filename').value.trim();
    const pkg = inferPackageFromFile(file);
    window.location.href = `display.html?pkg=${encodeURIComponent(pkg)}&file=${encodeURIComponent(file)}`;
}

async function init() {
    Prism.hooks.add('after-tokenize', analyzeTokens);
    Prism.hooks.add('wrap', wrapTokenLink);

    await loadManifest();
    coreCData = await getPackageData('_core_c');

    const params = (new URL(document.location)).searchParams;
    let pkg = params.get('pkg');
    let file = params.get('file');
    const sym = params.get('sym');

    // If resolving via symbol query
    if (sym && pkg) {
        await resolveSymbolAndNavigate(pkg, sym);
        return;
    }

    if (!file) {
        file = 'library/tools/R/code2html.R';
    }
    if (!pkg) {
        pkg = inferPackageFromFile(file);
    }

    currentPkg = pkg;
    currentPkgData = await getPackageData(pkg);

    document.getElementById('filename').value = file;
    document.getElementById('pkg-badge').innerText = pkg;

    const pkgInfo = manifest && manifest.packages ? manifest.packages[pkg] : null;
    document.getElementById('repo-badge').innerText = pkgInfo ? pkgInfo.repo : (pkg === '_core_c' ? 'r-devel/r-svn' : 'cran/' + pkg);

    // Check for line number in hash (e.g. #line.46 or #line-46 or #L46)
    let lineNum = null;
    const hash = window.location.hash;
    if (hash) {
        const m = hash.match(/(?:line[.-]|L)(\d+)/i);
        if (m) lineNum = parseInt(m[1], 10);
    }

    await loadFileContent(pkg, file, lineNum);
}
