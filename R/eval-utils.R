### =========================================================================
### Helpers for environments and evaluation
### -------------------------------------------------------------------------

safeEval <- function(expr, envir, enclos, strict=FALSE) {
  expr <- eval(call("bquote", expr, enclos))
  if (strict) {
    enclos <- makeGlobalWarningEnv(expr, envir, enclos)
  }
  eval(expr, envir, enclos)
}

makeGlobalWarningEnv <- function(expr, envir, enclos) {
  globals <- setdiff(all.names(expr, functions=FALSE), names(envir))
  env <- new.env(parent=enclos)
  lapply(globals, function(g) {
    makeActiveBinding(g, function() {
      val <- get(g, enclos)
      warning("Symbol '", g, "' resolved from calling frame; ",
              "escape with .(", g, ") for safety.")
      val
    }, env)
  })
  env
}

evalArg <- function(expr, envir, ..., where=parent.frame()) {
  enclos <- eval(call("top_prenv", expr, where))
  expr <- eval(call("substitute", expr), where)
  safeEval(expr, envir, enclos, ...)
}

normSubsetIndex <- function(i) {
  i <- try(as.logical(i), silent=TRUE)
  if (inherits(i, "try-error"))
    stop("'subset' must be coercible to logical")
  i & !is.na(i)
}

missingArg <- function(arg, where=parent.frame()) {
  eval(call("missing", arg), where)
}

evalqForSubset <- function(expr, envir, ...) {
  if (missingArg(substitute(expr), parent.frame())) {
    TRUE
  } else {
    i <- evalArg(substitute(expr), envir, ..., where=parent.frame())
    normSubsetIndex(i)
  }
}

evalqForSelect <- function(expr, df, ...) {
  if (missingArg(substitute(expr), parent.frame())) {
    TRUE
  } else {
    nl <- as.list(seq_len(ncol(df)))
    names(nl) <- colnames(df)
    evalArg(substitute(expr), nl, ..., where=parent.frame())
  }
}

top_prenv <- function(x, where=parent.frame()) {
  sym <- substitute(x)
  if (!is.name(sym)) {
    stop("'x' did not substitute to a symbol")
  }
  if (!is.environment(where)) {
    stop("'where' must be an environment")
  }
  .Call2("top_prenv", sym, where, PACKAGE="IRanges")
}

top_prenv_dots <- function(...) {
  .Call("top_prenv_dots", environment(), PACKAGE="IRanges")
}

