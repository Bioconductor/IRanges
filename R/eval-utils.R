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

top_prenv <- function(x) {
  sym <- substitute(x)
  if (!is.name(sym)) {
    stop("'x' did not substitute to a symbol")
  }
  .Call2("top_prenv", sym, parent.frame(), PACKAGE="IRanges")
}

top_prenv_dots <- function(...) {
  .Call("top_prenv_dots", environment(), PACKAGE="IRanges")
}
