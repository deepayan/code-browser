#!/usr/bin/env Rscript

## index_builder.R
## Builds structured, lightweight JSON metadata for R source navigation
## Supports:
##   1. R trunk (src/library/* packages + core C runtime)
##   2. Arbitrary CRAN packages (from source directory or tarball)

suppressPackageStartupMessages({
    library(jsonlite)
    library(utils)
    library(tools)
})

## ----------------------------------------------------------------------
## R source file parser
## ----------------------------------------------------------------------

tag_r_file <- function(filepath) {
    p <- tryCatch(parse(filepath, keep.source = TRUE), error = function(e) NULL)
    if (is.null(p) || length(p) == 0) {
        return(data.frame(token = character(), line = integer(), stringsAsFactors = FALSE))
    }
    
    ok_verbs <- c("<-", "=", "<<-", "assign", "delayedAssign", "makeActiveBinding",
                  "setGeneric", "setGroupGeneric", "setMethod", "setClass", "setClassUnion")
    
    extract_expr <- function(expr, default_line = NA_integer_) {
        if (!is.call(expr) || length(expr) < 2L) return(list())
        verb <- as.character(expr[[1L]])[1L]
        srcref <- attr(expr, "srcref")
        line <- if (!is.null(srcref)) srcref[1L] else default_line
        
        if (verb %in% ok_verbs) {
            tok <- utils:::expr2token(expr)
            if (length(tok) == 1L && nzchar(tok)) {
                return(list(list(token = tok, line = line)))
            }
        } else if (verb == "{" || (verb == "local" && length(expr) >= 2L)) {
            body <- if (verb == "local") expr[[2L]] else expr
            if (is.call(body) && as.character(body[[1L]])[1L] == "{") {
                out <- list()
                for (k in seq_along(body)[-1L]) {
                    out <- c(out, extract_expr(body[[k]], default_line = line))
                }
                return(out)
            }
        }
        list()
    }
    
    srcrefs <- attr(p, "srcref")
    all_toks <- list()
    for (i in seq_along(p)) {
        line_i <- if (!is.null(srcrefs[[i]])) srcrefs[[i]][1L] else NA_integer_
        toks_i <- extract_expr(p[[i]], default_line = line_i)
        all_toks <- c(all_toks, toks_i)
    }
    
    if (length(all_toks) == 0) {
        return(data.frame(token = character(), line = integer(), stringsAsFactors = FALSE))
    }
    toks <- sapply(all_toks, `[[`, "token")
    lines <- sapply(all_toks, `[[`, "line")
    keep <- nzchar(toks) & !is.na(lines)
    data.frame(token = toks[keep], line = lines[keep], stringsAsFactors = FALSE)
}


## ----------------------------------------------------------------------
## C / C++ source file parser
## ----------------------------------------------------------------------

tag_c_file <- function(filepath) {
    lines <- readLines(filepath, warn = FALSE)
    if (length(lines) == 0) {
        return(data.frame(token = character(), line = integer(), stringsAsFactors = FALSE))
    }
    
    # 1. Macros: #define NAME
    macros <- regmatches(lines, regexec("^\\s*#\\s*define\\s+([A-Za-z0-9_]+)", lines))
    macro_tokens <- sapply(macros, function(x) if (length(x) > 1) x[2] else NA_character_)
    macro_lines <- which(!is.na(macro_tokens))
    
    # 2. Function definitions
    fn_re <- "^(?:[A-Za-z0-9_]+[\\s*]+)+?([A-Za-z0-9_]+)\\s*\\([^;]*$"
    m <- regmatches(lines, regexec(fn_re, lines, perl = TRUE))
    fn_tokens <- sapply(m, function(x) if (length(x) > 1) x[2] else NA_character_)
    
    # Check for multiline declarations where return type was on previous line
    type_re <- "^(?:static\\s+|attribute_hidden\\s+|attribute_visible\\s+|const\\s+)*(?:SEXP|void|int|double|bool|Rboolean|char\\s*\\*|size_t|R_xlen_t|long|float|unsigned\\s+int)[\\s*]*$"
    for (i in 2:length(lines)) {
        if (is.na(fn_tokens[i])) {
            prev_line <- trimws(lines[i - 1])
            curr_line <- trimws(lines[i])
            if (grepl(type_re, prev_line, perl = TRUE)) {
                m_curr <- regmatches(curr_line, regexec("^([A-Za-z0-9_]+)\\s*\\([^;]*$", curr_line, perl = TRUE))[[1]]
                if (length(m_curr) > 1) {
                    fn_tokens[i] <- m_curr[2]
                }
            }
        }
    }
    
    c_keywords <- c("if", "while", "for", "switch", "return", "else", "case", "sizeof",
                    "static", "typedef", "struct", "union", "enum", "const", "default")
    fn_tokens[fn_tokens %in% c_keywords] <- NA_character_
    
    # Validate function definition has a body { without ; before it
    fn_lines <- which(!is.na(fn_tokens))
    valid_fn <- logical(length(fn_lines))
    for (i in seq_along(fn_lines)) {
        l_idx <- fn_lines[i]
        chunk <- paste(lines[l_idx:min(length(lines), l_idx + 12)], collapse = " ")
        semicolon_pos <- regexpr(";", chunk, fixed = TRUE)
        brace_pos <- regexpr("\\{", chunk)
        if (brace_pos > 0 && (semicolon_pos == -1 || brace_pos < semicolon_pos)) {
            valid_fn[i] <- TRUE
        }
    }
    fn_lines <- fn_lines[valid_fn]
    fn_tokens <- fn_tokens[fn_lines]
    
    all_tokens <- c(macro_tokens[macro_lines], fn_tokens)
    all_lines <- c(macro_lines, fn_lines)
    
    # Exclude macro parameters or dummy names
    exclude <- c("CALLDEF", "CALLDEF_DO", "C_DEF", "EXTDEF", "FDEF", "name", "n")
    keep <- !is.na(all_tokens) & nzchar(all_tokens) & !(all_tokens %in% exclude)
    data.frame(token = all_tokens[keep], line = all_lines[keep], stringsAsFactors = FALSE)
}


## ----------------------------------------------------------------------
## R Core Primitives and Internals extractor (src/main/names.c)
## ----------------------------------------------------------------------

extract_core_primitives <- function(trunk_path, core_c_defs) {
    names_c_path <- file.path(trunk_path, "src", "main", "names.c")
    if (!file.exists(names_c_path)) return(list())
    
    lines <- readLines(names_c_path, warn = FALSE)
    m <- regmatches(lines, regexec("^\\s*\\{\\s*\"([^\"]+)\"\\s*,\\s*([A-Za-z0-9_]+)", lines))
    matches <- do.call(rbind, lapply(m, function(x) if (length(x) == 3) x[2:3] else NULL))
    if (is.null(matches) || nrow(matches) == 0) return(list())
    
    primitives <- list()
    for (i in seq_len(nrow(matches))) {
        r_name <- matches[i, 1]
        c_entry <- matches[i, 2]
        
        target <- core_c_defs[[c_entry]]
        if (!is.null(target)) {
            primitives[[r_name]] <- list(
                c_symbol = c_entry,
                file = target$file,
                line = target$line
            )
        } else {
            # Default to names.c if definition file not found
            primitives[[r_name]] <- list(
                c_symbol = c_entry,
                file = "main/names.c",
                line = 83L
            )
        }
    }
    primitives
}


## ----------------------------------------------------------------------
## Routine registration extractor
## ----------------------------------------------------------------------

extract_routine_registrations <- function(pkg_dir, c_defs, fixes = character()) {
    routines <- list()
    
    # 1. Inspect init.c or *.c files in src/
    src_dir <- file.path(pkg_dir, "src")
    if (!dir.exists(src_dir)) return(routines)
    
    c_files <- list.files(src_dir, pattern = "\\.[ch]$", full.names = TRUE)
    for (cf in c_files) {
        lines <- readLines(cf, warn = FALSE)
        # Patterns like:
        # CALLDEF(cor, 4)
        # CALLDEF_DO(cor, 4) -> do_cor
        # C_DEF(rcont2, 8)
        # {"foo", (DL_FUNC) &c_foo, 2}
        
        # CALLDEF_DO
        m_do <- regmatches(lines, regexec("CALLDEF_DO\\s*\\(\\s*([A-Za-z0-9_]+)", lines))
        for (x in m_do) {
            if (length(x) > 1 && x[2] != "name") {
                r_name <- x[2]
                c_sym <- paste0("do_", r_name)
                loc <- c_defs[[c_sym]] %||% c_defs[[r_name]]
                routines[[r_name]] <- list(c_symbol = c_sym, file = loc$file %||% "", line = loc$line %||% 0L)
            }
        }
        
        # CALLDEF and C_DEF
        m_call <- regmatches(lines, regexec("(?:CALLDEF|C_DEF|EXTDEF|FDEF)\\s*\\(\\s*([A-Za-z0-9_]+)", lines))
        for (x in m_call) {
            if (length(x) > 1 && !(x[2] %in% c("name", "CALLDEF", "C_DEF"))) {
                r_name <- x[2]
                loc <- c_defs[[r_name]]
                routines[[r_name]] <- list(c_symbol = r_name, file = loc$file %||% "", line = loc$line %||% 0L)
            }
        }
        
        # {"r_name", (DL_FUNC) &c_func, ...}
        m_reg <- regmatches(lines, regexec("\"([A-Za-z0-9_]+)\"\\s*,\\s*\\(DL_FUNC\\)\\s*&([A-Za-z0-9_]+)", lines))
        for (x in m_reg) {
            if (length(x) > 2) {
                r_name <- x[2]
                c_sym <- x[3]
                loc <- c_defs[[c_sym]] %||% c_defs[[r_name]]
                routines[[r_name]] <- list(c_symbol = c_sym, file = loc$file %||% "", line = loc$line %||% 0L)
            }
        }
    }
    
    # If fixes (e.g. "C_") specified, add alias C_foo -> foo
    pfx <- fixes[1]
    if (!is.na(pfx) && nzchar(pfx)) {
        for (r_name in names(routines)) {
            alias_name <- paste0(pfx, r_name)
            routines[[alias_name]] <- routines[[r_name]]
        }
    }
    
    routines
}

`%||%` <- function(a, b) if (is.null(a)) b else a


## ----------------------------------------------------------------------
## Index a single package
## ----------------------------------------------------------------------

index_package <- function(pkg_dir, is_base = FALSE, repo = NULL, branch = NULL, base_path = NULL) {
    desc_file <- file.path(pkg_dir, "DESCRIPTION")
    if (!file.exists(desc_file)) {
        desc_file <- file.path(pkg_dir, "DESCRIPTION.in")
    }
    if (!file.exists(desc_file)) stop("Neither DESCRIPTION nor DESCRIPTION.in found in: ", pkg_dir)
    
    dcf <- as.list(read.dcf(desc_file)[1, ])
    pkg_name <- dcf$Package
    cat(sprintf("Indexing package '%s' (%s)...\n", pkg_name, if (is_base) "base R" else "CRAN"))
    
    # Parse dependencies from DESCRIPTION
    parse_deps <- function(field) {
        val <- dcf[[field]]
        if (is.null(val) || !nzchar(val)) return(character())
        raw <- strsplit(val, ",\\s*")[[1]]
        clean <- sapply(strsplit(raw, "\\s*\\("), `[`, 1)
        trimws(clean[clean != "R"])
    }
    
    depends <- parse_deps("Depends")
    imports_desc <- parse_deps("Imports")
    linking_to <- parse_deps("LinkingTo")
    
    # Parse NAMESPACE
    ns_file <- file.path(pkg_dir, "NAMESPACE")
    ns_exports <- character()
    ns_import_from <- list()
    reg_fixes <- character()
    
    if (file.exists(ns_file)) {
        ns <- tryCatch(parseNamespaceFile(basename(pkg_dir), dirname(pkg_dir)), error = function(e) NULL)
        if (!is.null(ns)) {
            ns_exports <- ns$exports
            
            # S3 methods
            if (!is.null(ns$S3methods) && nrow(ns$S3methods) > 0) {
                s3_funs <- paste(ns$S3methods[, 1], ns$S3methods[, 2], sep = ".")
                ns_exports <- unique(c(ns_exports, s3_funs))
            }
            
            # parse importFrom
            for (imp in ns$imports) {
                if (is.list(imp) && length(imp) == 2) {
                    target_pkg <- imp[[1]]
                    for (fn in imp[[2]]) {
                        ns_import_from[[fn]] <- target_pkg
                    }
                }
            }
            
            # Registration fixes (e.g. C_)
            if (!is.null(ns$nativeRoutines) && length(ns$nativeRoutines) > 0) {
                nr <- ns$nativeRoutines[[1]]
                if (!is.null(nr$registrationFixes)) {
                    reg_fixes <- nr$registrationFixes
                }
            }
        }
    }
    
    # Tag R files
    r_dir <- file.path(pkg_dir, "R")
    r_symbols <- list()
    if (dir.exists(r_dir)) {
        r_files <- list.files(r_dir, pattern = "\\.[RrSs]$", full.names = TRUE, recursive = TRUE)
        for (rf in r_files) {
            rel_sub <- substring(rf, nchar(r_dir) + 2L)
            rel_file <- if (is_base && !is.null(base_path) && nzchar(base_path)) {
                file.path(base_path, "R", rel_sub)
            } else {
                file.path("R", rel_sub)
            }
            tags <- tag_r_file(rf)
            if (nrow(tags) > 0) {
                for (k in seq_len(nrow(tags))) {
                    tok <- tags$token[k]
                    # store first definition or primary definition
                    if (is.null(r_symbols[[tok]])) {
                        is_exp <- if (file.exists(ns_file)) (tok %in% ns_exports) else (pkg_name == "base")
                        r_symbols[[tok]] <- list(
                            file = rel_file,
                            line = tags$line[k],
                            exported = is_exp
                        )
                    }
                }
            }
        }
    }
    
    if (!file.exists(ns_file) && pkg_name == "base") {
        ns_exports <- names(r_symbols)
    }
    
    # Tag C/C++ files
    src_dir <- file.path(pkg_dir, "src")
    c_symbols <- list()
    if (dir.exists(src_dir)) {
        c_files <- list.files(src_dir, pattern = "\\.(c|h|cpp|cc|hpp|hh)$", full.names = TRUE, recursive = TRUE)
        for (cf in c_files) {
            rel_sub <- substring(cf, nchar(src_dir) + 2L)
            rel_file <- if (is_base && !is.null(base_path) && nzchar(base_path)) {
                file.path(base_path, "src", rel_sub)
            } else {
                file.path("src", rel_sub)
            }
            c_tags <- tag_c_file(cf)
            if (nrow(c_tags) > 0) {
                for (k in seq_len(nrow(c_tags))) {
                    tok <- c_tags$token[k]
                    if (is.null(c_symbols[[tok]])) {
                        c_symbols[[tok]] <- list(
                            file = rel_file,
                            line = c_tags$line[k]
                        )
                    }
                }
            }
        }
    }
    
    # Routine registrations
    c_routines <- extract_routine_registrations(pkg_dir, c_symbols, fixes = reg_fixes)
    
    # Package metadata payload
    pkg_payload <- list(
        package = pkg_name,
        is_base = is_base,
        repo = repo %||% if (is_base) "r-devel/r-svn" else paste0("cran/", pkg_name),
        branch = branch %||% if (is_base) "trunk" else "master",
        base_path = base_path %||% if (is_base) paste0("library/", pkg_name) else "",
        depends = depends,
        imports = imports_desc,
        import_from = ns_import_from,
        linking_to = linking_to,
        r_symbols = r_symbols,
        c_symbols = c_symbols,
        c_routines = c_routines
    )
    
    out_pkg_file <- file.path("metadata", "packages", paste0(pkg_name, ".json"))
    write_json(pkg_payload, out_pkg_file, auto_unbox = TRUE, pretty = FALSE)
    cat(sprintf("  Saved %s (%d R symbols, %d C symbols, %d native routines)\n",
                out_pkg_file, length(r_symbols), length(c_symbols), length(c_routines)))
    
    list(
        package = pkg_name,
        is_base = is_base,
        repo = pkg_payload$repo,
        branch = pkg_payload$branch,
        base_path = pkg_payload$base_path,
        depends = depends,
        imports = imports_desc,
        linking_to = linking_to,
        exports = ns_exports
    )
}


## ----------------------------------------------------------------------
## Index R Core C Engine (src/main, src/appl, src/nmath, src/include)
## ----------------------------------------------------------------------

index_core_c <- function(trunk_path) {
    cat("Indexing R Core C engine (main, appl, nmath, include)...\n")
    subdirs <- c("main", "appl", "nmath", "include")
    core_c_defs <- list()
    
    for (sd in subdirs) {
        sd_path <- file.path(trunk_path, "src", sd)
        if (!dir.exists(sd_path)) next
        c_files <- list.files(sd_path, pattern = "\\.(c|h)$", full.names = TRUE, recursive = TRUE)
        for (cf in c_files) {
            rel_sub <- substring(cf, nchar(sd_path) + 2L)
            rel_file <- file.path(sd, rel_sub)
            tags <- tag_c_file(cf)
            if (nrow(tags) > 0) {
                for (k in seq_len(nrow(tags))) {
                    tok <- tags$token[k]
                    if (is.null(core_c_defs[[tok]])) {
                        core_c_defs[[tok]] <- list(
                            file = rel_file,
                            line = tags$line[k]
                        )
                    }
                }
            }
        }
    }
    
    payload <- list(
        package = "_core_c",
        is_base = TRUE,
        repo = "r-devel/r-svn",
        branch = "trunk",
        base_path = "",
        c_symbols = core_c_defs
    )
    
    out_core_file <- file.path("metadata", "packages", "_core_c.json")
    write_json(payload, out_core_file, auto_unbox = TRUE, pretty = FALSE)
    cat(sprintf("  Saved %s (%d C definitions)\n", out_core_file, length(core_c_defs)))
    core_c_defs
}


## ----------------------------------------------------------------------
## Main orchestration: Index R Trunk
## ----------------------------------------------------------------------

index_trunk <- function(trunk_path = "/home/deepayan/svn/all/r-project/R/trunk") {
    cat(sprintf("Starting full index of R trunk at %s\n", trunk_path))
    dir.create("metadata/packages", recursive = TRUE, showWarnings = FALSE)
    
    # 1. Index Core C runtime
    core_c_defs <- index_core_c(trunk_path)
    
    # 2. Extract .Primitive & .Internal mapping from names.c
    cat("Extracting primitives and internals from names.c...\n")
    primitives <- extract_core_primitives(trunk_path, core_c_defs)
    cat(sprintf("  Extracted %d primitive / internal mappings\n", length(primitives)))
    
    # 3. Index all packages under src/library
    lib_dir <- file.path(trunk_path, "src", "library")
    pkg_dirs <- list.dirs(lib_dir, recursive = FALSE)
    pkg_dirs <- pkg_dirs[!basename(pkg_dirs) %in% c("profile", "Recommended", "translations")]
    
    pkg_summaries <- list()
    global_exports <- list()
    
    for (pd in pkg_dirs) {
        pkg_name <- basename(pd)
        summary <- index_package(pd, is_base = TRUE,
                                 repo = "r-devel/r-svn",
                                 branch = "trunk",
                                 base_path = paste0("library/", pkg_name))
        pkg_summaries[[pkg_name]] <- summary
        for (exp in summary$exports) {
            global_exports[[exp]] <- c(global_exports[[exp]], pkg_name)
        }
    }
    
    # 4. Build manifest.json
    manifest <- list(
        packages = pkg_summaries,
        exports = global_exports,
        r_primitives = primitives
    )
    write_json(manifest, "metadata/manifest.json", auto_unbox = TRUE, pretty = TRUE)
    cat("Saved metadata/manifest.json\n")
    
    # 5. Build compact taginfo.json for index.html search
    build_search_index()
    cat("Done indexing R trunk!\n")
}


## ----------------------------------------------------------------------
## Add / Index a CRAN package
## ----------------------------------------------------------------------

index_cran_pkg <- function(pkg_name = NULL, source_dir = NULL, tarball = NULL) {
    if (!is.null(tarball)) {
        if (!file.exists(tarball)) stop("Tarball not found: ", tarball)
        tmp <- tempfile(pattern = "cran_pkg_")
        dir.create(tmp)
        untar(tarball, exdir = tmp)
        dirs <- list.dirs(tmp, recursive = FALSE)
        if (length(dirs) != 1) stop("Unexpected tarball layout")
        source_dir <- dirs[1]
        if (is.null(pkg_name)) pkg_name <- basename(source_dir)
    }
    
    if (is.null(source_dir) || !dir.exists(source_dir)) {
        stop("Valid source_dir required")
    }
    if (is.null(pkg_name)) {
        dcf <- as.list(read.dcf(file.path(source_dir, "DESCRIPTION"))[1, ])
        pkg_name <- dcf$Package
    }
    
    dir.create("metadata/packages", recursive = TRUE, showWarnings = FALSE)
    summary <- index_package(source_dir, is_base = FALSE,
                             repo = paste0("cran/", pkg_name),
                             branch = "master",
                             base_path = "")
    
    # Update manifest.json
    manifest_file <- "metadata/manifest.json"
    manifest <- if (file.exists(manifest_file)) fromJSON(manifest_file) else list(packages = list(), exports = list(), r_primitives = list())
    manifest$packages[[pkg_name]] <- summary
    for (exp in summary$exports) {
        manifest$exports[[exp]] <- unique(c(manifest$exports[[exp]], pkg_name))
    }
    write_json(manifest, manifest_file, auto_unbox = TRUE, pretty = TRUE)
    cat(sprintf("Updated metadata/manifest.json with '%s'\n", pkg_name))
    
    # Rebuild search index
    build_search_index()
}


## ----------------------------------------------------------------------
## Compact Search Index for index.html (DataTables)
## ----------------------------------------------------------------------

build_search_index <- function() {
    cat("Building search index taginfo.json...\n")
    pkg_files <- list.files("metadata/packages", pattern = "\\.json$", full.names = TRUE)
    
    records <- list()
    for (pf in pkg_files) {
        pkg_data <- fromJSON(pf)
        pkg_name <- pkg_data$package
        
        # R symbols
        if (!is.null(pkg_data$r_symbols)) {
            sym_names <- names(pkg_data$r_symbols)
            for (sn in sym_names) {
                entry <- pkg_data$r_symbols[[sn]]
                file_path <- entry$file
                line <- entry$line
                href <- sprintf("<a href='display.html?pkg=%s&file=%s#line.%d' target='_blank'>%s</a>",
                                pkg_name, URLencode(file_path), line, file_path)
                records[[length(records) + 1L]] <- list(sn, href, as.character(line), pkg_name)
            }
        }
        
        # C routines / symbols
        if (!is.null(pkg_data$c_symbols)) {
            c_names <- names(pkg_data$c_symbols)
            for (cn in c_names) {
                entry <- pkg_data$c_symbols[[cn]]
                file_path <- entry$file
                line <- entry$line
                href <- sprintf("<a href='display.html?pkg=%s&file=%s#line.%d' target='_blank'>%s</a>",
                                pkg_name, URLencode(file_path), line, file_path)
                records[[length(records) + 1L]] <- list(cn, href, as.character(line), pkg_name)
            }
        }
    }
    
    ans <- list(data = records)
    write_json(ans, "taginfo.json", auto_unbox = TRUE, pretty = FALSE)
    cat(sprintf("Saved taginfo.json with %d indexed symbols\n", length(records)))
}


## ----------------------------------------------------------------------
## Command line dispatch
## ----------------------------------------------------------------------

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0 || "--help" %in% args || "-h" %in% args) {
    cat("Usage:\n")
    cat("  Rscript index_builder.R --trunk=/path/to/R/trunk\n")
    cat("  Rscript index_builder.R --pkg=lattice --tarball=/path/to/lattice.tar.gz\n")
    cat("  Rscript index_builder.R --pkg=lattice --source=/path/to/lattice/source\n")
    cat("  Rscript index_builder.R --build-search-index\n")
} else {
    get_arg <- function(prefix) {
        match <- grep(paste0("^", prefix), args, value = TRUE)
        if (length(match) > 0) sub(paste0("^", prefix, "=?"), "", match[1]) else NULL
    }
    
    trunk_arg <- get_arg("--trunk")
    pkg_arg <- get_arg("--pkg")
    source_arg <- get_arg("--source")
    tarball_arg <- get_arg("--tarball")
    
    if (!is.null(trunk_arg)) {
        index_trunk(trunk_arg)
    } else if (!is.null(tarball_arg)) {
        index_cran_pkg(pkg_name = pkg_arg, tarball = tarball_arg)
    } else if (!is.null(source_arg)) {
        index_cran_pkg(pkg_name = pkg_arg, source_dir = source_arg)
    } else if ("--build-search-index" %in% args) {
        build_search_index()
    } else {
        cat("Unrecognized options. Run with --help.\n")
    }
}
