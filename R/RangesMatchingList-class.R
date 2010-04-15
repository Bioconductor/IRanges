### =========================================================================
### RangesMatchingList objects
### -------------------------------------------------------------------------

setClass("RangesMatchingList",
         representation(subjectOffsets = "integer"),
         prototype(elementType = "RangesMatching"),
         contains = "SimpleList")

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Accessors
###

setMethod("space", "RangesMatchingList",
          function(x) {
            space <- names(x)
            if (!is.null(space))
              space <-
                rep.int(space, sapply(as.list(x, use.names = FALSE), length))
            space
          })

setMethod("subjectHits", "RangesMatchingList", function(x) {
  as.matrix(x)[,2L,drop=TRUE]
})

setMethod("queryHits", "RangesMatchingList", function(x) {
  as.matrix(x)[,1L,drop=TRUE]
})

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Constructor
###

RangesMatchingList <- function(matchings, subject)
{
  subjectOffsets <- c(0L, head(cumsum(sapply(subject, length)), -1))
  subjectToQuery <- seq_along(matchings)
  if (!is.null(names(matchings)) && !is.null(names(subject)))
    subjectToQuery <- match(names(matchings), names(subject))
  subjectOffsets <- subjectOffsets[subjectToQuery]
  newSimpleList("RangesMatchingList", matchings,
                subjectOffsets = subjectOffsets)
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion
###

## return as.matrix as on RangesMatching, with indices adjusted

setMethod("as.matrix", "RangesMatchingList", function(x) {
  mats <- lapply(x, as.matrix)
  mat <- do.call(rbind, mats)
  rows <- c(0L, head(cumsum(sapply(x, nrow)), -1))
  nr <- sapply(mats, nrow)
  mat + cbind(rep.int(rows, nr), rep.int(x@subjectOffsets, nr))
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
