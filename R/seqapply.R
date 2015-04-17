### =========================================================================
### The stuff in this file should go somewhere else, probably close to
### splitAsList() (currently extractList.R)
### -------------------------------------------------------------------------
###

## NOT exported.
`splitAsList<-` <- function(x, f, drop = FALSE, ..., value) {
  if (!isTRUEorFALSE(drop))
    stop("'drop' must be TRUE or FALSE")
  if (length(x) != length(f))
    stop("Length of 'f' must equal the length of 'x'")
  ind <- splitAsList(seq_len(length(x)), f, drop = drop)
  if (length(ind) != length(value))
    stop("Length of 'value' must equal the length of a split on 'f'")
  x[unlist(ind, use.names=FALSE)] <- unlist(value, use.names = FALSE)
  x
}

setMethod("unsplit", "List", function(value, f, drop = FALSE) {
  value_flat <- unlist(value, use.names = FALSE)
  if (length(value_flat) != length(f))
    stop("Length of 'unlist(value)' must equal length of 'f'")
  splitAsList(value_flat, f, drop = drop) <- value
  value_flat
})

setReplaceMethod("split", "Vector", function(x, f, drop = FALSE, ..., value) {
  splitAsList(x, f, drop = drop, ...) <- value
  x
})

