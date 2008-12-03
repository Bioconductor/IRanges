### =========================================================================
### RangesMatchingList objects
### -------------------------------------------------------------------------

setClass("RangesMatchingList",
         prototype = prototype(elementClass = "RangesMatching"),
         contains = "TypedList")

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Accessors
###

setMethod("space", "RangesMatchingList",
          function(x) {
            space <- names(x)
            if (!is.null(space))
              space <- rep(space, sapply(elements(x), ncol))
            space
          })

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Constructor
###

RangesMatchingList <- function(...)
{
  matchings <- list(...)
  if (!all(sapply(matchings, is, "RangesMatching")))
    stop("all elements in '...' must be instances of 'RangesMatching'")
  NAMES <- names(matchings)
  names(matchings) <- NULL
  new("RangesMatchingList", elements=matchings, NAMES=NAMES)
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion
###

## return as.matrix as on RangesMatching, except with space column

setMethod("as.matrix", "RangesMatchingList", function(x) {
  cbind(space = space(x), do.call("cbind", lapply(x, as.matrix)))
})

## count up the matches for each query in every matching

setMethod("as.table", "RangesMatchingList", function(x, ...) {
  counts <- unlist(lapply(x, as.table))
  as.table(array(counts, length(counts), list(range = seq_along(counts))))
})

setMethod("t", "RangesMatchingList", function(x) {
  x@elements <- lapply(elements(x), t)
  x
})

setMethod("ranges", "RangesMatchingList", function(x, query, subject) {
  if (!is(query, "RangesList") || length(query) != length(x))
    stop("'query' must be a RangesList of length equal to that of 'x'")
  if (!is(subject, "RangesList") || length(subject) != length(x))
    stop("'subject' must be a RangesList of length equal to that of 'x'")
  els <- elements(x)
  queries <- elements(query)
  subjects <- elements(subject)
  ans <- do.call("RangesList", lapply(seq_len(length(x)), function(i) {
    ranges(els[[i]], queries[[i]], subjects[[i]])
  }))
  names(ans) <- names(x)
  ans
})

### TODO: many convenience methods
