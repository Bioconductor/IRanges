### =========================================================================
### The stuff in this file should go somewhere else, probably close to
### splitAsList() (currently defined in S4Vectors/R/split-methods.R)
### -------------------------------------------------------------------------
###

## NOT exported.
`splitAsList<-` <- function(x, f, drop = FALSE, ..., value) {
  if (!isTRUEorFALSE(drop))
    stop("'drop' must be TRUE or FALSE")
  if (NROW(x) != length(f))
    stop("Length of 'f' must equal the length of 'x'")
  ind <- splitAsList(seq_len(NROW(x)), f, drop = drop)
  if (length(ind) != length(value))
    stop("Length of 'value' must equal the length of a split on 'f'")
  replaceROWS(x, unlist(ind, use.names=FALSE), unlist(value, use.names = FALSE))
}

setMethod("unsplit", "List", function(value, f, drop = FALSE) {
  value_flat <- unlist(value, use.names = FALSE)
  if (NROW(value_flat) != length(f))
    stop("Length of 'unlist(value)' must equal length of 'f'")
  splitAsList(value_flat, f, drop = drop) <- value
  if (!is.null(ROWNAMES(value_flat))) {
    nms <- relist(ROWNAMES(value_flat), value)
    splitAsList(ROWNAMES(value_flat), f, drop = drop) <- nms
  }
  value_flat
})

setReplaceMethod("split", "Vector", function(x, f, drop = FALSE, ..., value) {
  splitAsList(x, f, drop = drop, ...) <- value
  x
})

