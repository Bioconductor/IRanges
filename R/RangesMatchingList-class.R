### =========================================================================
### RangesMatchingList objects
### -------------------------------------------------------------------------

setClass("RangesMatchingList",
         prototype = prototype(elementClass = "RangesMatching", compressible = FALSE),
         contains = "TypedList")

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Accessors
###

setMethod("space", "RangesMatchingList",
          function(x) {
            space <- names(x)
            if (!is.null(space))
              space <- rep(space, sapply(as.list(x, use.names = FALSE), ncol))
            space
          })

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Constructor
###

RangesMatchingList <- function(...)
{
  TypedList("RangesMatchingList", elements = list(...), compress = FALSE)
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion
###

## return as.matrix as on RangesMatching, except with space column

setMethod("as.matrix", "RangesMatchingList", function(x) {
  cbind(space = space(x), do.call(cbind, lapply(x, as.matrix)))
})

## count up the matches for each query in every matching

setMethod("as.table", "RangesMatchingList", function(x, ...) {
  counts <- unlist(lapply(x, as.table))
  as.table(array(counts, length(counts), list(range = seq_along(counts))))
})

setMethod("t", "RangesMatchingList", function(x) {
  x@elements <- lapply(as.list(x, use.names = FALSE), t)
  x
})

setMethod("ranges", "RangesMatchingList", function(x, query, subject) {
  if (!is(query, "RangesList") || length(query) != length(x))
    stop("'query' must be a RangesList of length equal to that of 'x'")
  if (!is(subject, "RangesList") || length(subject) != length(x))
    stop("'subject' must be a RangesList of length equal to that of 'x'")
  els <- as.list(x, use.names = FALSE)
  queries <- as.list(query, use.names = FALSE)
  subjects <- as.list(subject, use.names = FALSE)
  ans <- do.call(RangesList, lapply(seq_len(length(x)), function(i) {
    ranges(els[[i]], queries[[i]], subjects[[i]])
  }))
  names(ans) <- names(x)
  ans
})

### TODO: many convenience methods
