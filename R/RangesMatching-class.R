### =========================================================================
### RangesMatching objects
### -------------------------------------------------------------------------

setClass("RangesMatching",
         representation(matchMatrix = "matrix", DIM = "integer"))


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Accessors
###

setGeneric("matchMatrix", function(x, ...) standardGeneric("matchMatrix"))
setMethod("matchMatrix", "RangesMatching", function(x) x@matchMatrix)

setMethod("dim", "RangesMatching", function(x) {
  x@DIM
})

setMethod("length", "RangesMatching", function(x) {
  nrow(as.matrix(x))
})

setGeneric("subjectHits", function(x, ...) standardGeneric("subjectHits"))

setMethod("subjectHits", "RangesMatching", function(x) {
  matchMatrix(x)[,2L,drop=TRUE]
})

setGeneric("queryHits", function(x, ...) standardGeneric("queryHits"))

setMethod("queryHits", "RangesMatching", function(x) {
  matchMatrix(x)[,1L,drop=TRUE]
})

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion
###

## return a matrix where each row indicates a match (query and subject index)

setMethod("as.matrix", "RangesMatching", function(x) {
  matchMatrix(x)
})

## count up the matches for each query

setMethod("as.table", "RangesMatching", function(x, ...) {
  tabulate(queryHits(x), nrow(x))
})

setMethod("t", "RangesMatching", function(x) {
  m <- matchMatrix(x)[,2:1,drop=FALSE]
  colnames(m) <- rev(colnames(m))
  x@matchMatrix <- m
  x@DIM <- x@DIM[2:1]
  x
})

setMethod("ranges", "RangesMatching", function(x, query, subject) {
  if (!is(query, "Ranges") || length(query) != nrow(x))
    stop("'query' must be a Ranges of length equal to number of queries")
  if (!is(subject, "Ranges") || length(subject) != ncol(x))
    stop("'subject' must be a Ranges of length equal to number of subjects")
  m <- as.matrix(x)
  qstart <- start(query)[m[,1L]]
  qend <- end(query)[m[,1L]]
  sstart <- start(subject)[m[,2L]]
  send <- end(subject)[m[,2L]]
  IRanges(pmax.int(qstart, sstart), pmin.int(send, qend))
})

### TODO: many convenience methods
