### =========================================================================
### RangesMatchingList objects
### -------------------------------------------------------------------------

setClass("RangesMatchingList",
         representation(subjectToQuery = "integer"),
         prototype(elementType = "RangesMatching"),
         contains = "SimpleList")

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Accessors
###

setMethod("space", "RangesMatchingList",
          function(x) {
            space <- names(x)
            if (!is.null(space))
              space <- rep(space, sapply(as.list(x, use.names = FALSE), length))
            space
          })

setMethod("subjectHits", "RangesMatchingList", function(x) {
  as.matrix(x)[,2]
})

setMethod("queryHits", "RangesMatchingList", function(x) {
  as.matrix(x)[,1]
})

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Constructor
###

RangesMatchingList <- function(matchings, subjectNames = NULL)
{
  subjectToQuery <- seq_along(matchings)
  if (!is.null(names(matchings)) && !is.null(subjectNames))
    subjectToQuery <- match(names(matchings), subjectNames)
  newSimpleList("RangesMatchingList", matchings, subjectToQuery = subjectToQuery)
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion
###

## return as.matrix as on RangesMatching, except with space column

setMethod("as.matrix", "RangesMatchingList", function(x) {
  mats <- lapply(x, as.matrix)
  mat <- do.call(rbind, mats)
  rows <- c(0, head(cumsum(lapply(x, nrow)), -1))[x@subjectToQuery]
  cols <- c(0, head(cumsum(lapply(x, ncol)), -1))
  nr <- sapply(mats, nrow)
  mat + cbind(rep(cols, nr), rep(rows, nr))
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
