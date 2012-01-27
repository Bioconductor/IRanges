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
  .Deprecated("queryLength or subjectLength")
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

setGeneric("subjectLength", function(x, ...) standardGeneric("subjectLength"))

setMethod("subjectLength", "RangesMatching", function(x) {
  x@DIM[2]
})

setGeneric("queryLength", function(x, ...) standardGeneric("queryLength"))

setMethod("queryLength", "RangesMatching", function(x) {
  x@DIM[1]
})

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Extraction
###

setMethod("[", "RangesMatching",
          function(x, i, j, ... , drop=FALSE)
          {
            if (!missing(j) || length(list(...)) > 0L)
              stop("invalid subsetting")
            if (!isTRUEorFALSE(drop))
              stop("'drop' must be TRUE or FALSE")
            if (!missing(i)) {
              iInfo <- .bracket.Index(i, length(x))
              if (!is.null(iInfo[["msg"]]))
                stop(iInfo[["msg"]])
              if (iInfo[["useIdx"]])
                x@matchMatrix <- x@matchMatrix[iInfo[["idx"]],,drop=FALSE]
            }
            if (drop)
              as.matrix(x)
            else x
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
          function(x, values = seq(subjectLength(x))) {
            unname(split(values[subjectHits(x)],
                         factor(queryHits(x), levels = seq(queryLength(x)))))
          })

setAs("RangesMatching", "list", function(from) as.list(from))
setAs("RangesMatching", "List", function(from) castList(as.list(from)))

## count up the matches for each query

setMethod("as.table", "RangesMatching", function(x, ...) {
  tabulate(queryHits(x), queryLength(x))
})


setMethod("t", "RangesMatching", function(x) {
  m <- matchMatrix(x)[,2:1,drop=FALSE]
  colnames(m) <- rev(colnames(m))
  x@matchMatrix <- m
  x@DIM <- x@DIM[2:1]
  x
})

setMethod("ranges", "RangesMatching", function(x, query, subject) {
  if (!is(query, "Ranges") || length(query) != queryLength(x))
    stop("'query' must be a Ranges of length equal to number of queries")
  if (!is(subject, "Ranges") || length(subject) != subjectLength(x))
    stop("'subject' must be a Ranges of length equal to number of subjects")
  m <- as.matrix(x)
  qstart <- start(query)[m[,1L]]
  qend <- end(query)[m[,1L]]
  sstart <- start(subject)[m[,2L]]
  send <- end(subject)[m[,2L]]
  IRanges(pmax.int(qstart, sstart), pmin.int(send, qend))
})

### TODO: many convenience methods
