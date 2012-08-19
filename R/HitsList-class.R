### =========================================================================
### HitsList objects
### -------------------------------------------------------------------------

setClass("HitsList",
    contains="SimpleList",
    representation(
        subjectOffsets="integer"
    ),
    prototype(elementType="Hits")
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Accessors
###

setMethod("space", "HitsList",
          function(x) {
            space <- names(x)
            if (!is.null(space))
              space <-
                rep.int(space, sapply(as.list(x, use.names = FALSE), length))
            space
          })

setMethod("subjectHits", "HitsList", function(x) {
  as.matrix(x)[,2L,drop=TRUE]
})

setMethod("queryHits", "HitsList", function(x) {
  as.matrix(x)[,1L,drop=TRUE]
})

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Constructor
###

HitsList <- function(list_of_hits, subject)
{
  subjectOffsets <- c(0L, head(cumsum(sapply(subject, length)), -1))
  subjectToQuery <- seq_along(list_of_hits)
  if (!is.null(names(list_of_hits)) && !is.null(names(subject)))
    subjectToQuery <- match(names(list_of_hits), names(subject))
  subjectOffsets <- subjectOffsets[subjectToQuery]
  newList("HitsList", list_of_hits, subjectOffsets = subjectOffsets)
}

RangesMatchingList <- function(...)
{
    .Defunct("HitsList")
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion
###

## return as.matrix as on Hits, with indices adjusted

setMethod("as.matrix", "HitsList", function(x) {
  mats <- lapply(x, as.matrix)
  mat <- do.call(rbind, mats)
  rows <- c(0L, head(cumsum(sapply(x, queryLength)), -1))
  nr <- sapply(mats, nrow)
  mat + cbind(rep.int(rows, nr), rep.int(x@subjectOffsets, nr))
})

## count up the matches for each query in every matching

setMethod("as.table", "HitsList", function(x, ...) {
  counts <- unlist(lapply(x, as.table))
  as.table(array(counts, length(counts), list(range = seq_along(counts))))
})

setMethod("t", "HitsList", function(x) {
  x@elements <- lapply(as.list(x, use.names = FALSE), t)
  x
})

setMethod("ranges", "HitsList", function(x, query, subject) {
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
