### =========================================================================
### multisplit()
### -------------------------------------------------------------------------
###


multisplit <- function(x, f) {
  if (!is.list(f) && !is(f, "List"))
    stop("'f' must be a list")
  if (length(x) != length(f))
    stop("Length of 'f' must equal length of 'x'")
  splitAsList(rep(x, elementNROWS(f)), unlist(f, use.names = FALSE))
}

