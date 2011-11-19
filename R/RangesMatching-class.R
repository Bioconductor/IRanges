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
### Extraction
###

setMethod("[", "RangesMatching",
          function(x, i, j, ... , drop=TRUE)
          {
            if (!missing(j) || length(list(...)) > 0L)
              stop("invalid subsetting")
            if (missing(i))
              return(x)
            if (is(i, "Rle"))
              i <- as.vector(i)
            if (!is.atomic(i))
              stop("invalid subscript type")
            lx <- length(x)
            if (length(i) == 0L) {
              i <- integer(0)
            } else if (is.numeric(i)) {
              if (min(i) < 0L)
                i <- seq_len(lx)[i]
              else if (!is.integer(i))
                i <- as.integer(i)
            } else if (is.logical(i)) {
              if (length(i) > lx)
                stop("subscript out of bounds")
              i <- seq_len(lx)[i]
            } else {
              stop("invalid subscript type")
            }
            x@matchMatrix <- x@matchMatrix[i,]
            x
          }
          )

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion
###

## return a matrix where each row indicates a match (query and subject index)

setMethod("as.matrix", "RangesMatching", function(x) {
  matchMatrix(x)
})

## a list, with an element for each query, containing the subject hits
setMethod("as.list", "RangesMatching",
          function(x, values = seq(ncol(x))) {
            unname(split(values[subjectHits(x)],
                         factor(queryHits(x), levels = seq(nrow(x)))))
          })

setAs("RangesMatching", "list", function(from) as.list(from))
setAs("RangesMatching", "List", function(from) castList(as.list(from)))

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
