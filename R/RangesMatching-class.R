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
  matchMatrix(x)[,2L]
})

setGeneric("queryHits", function(x, ...) standardGeneric("queryHits"))

setMethod("queryHits", "RangesMatching", function(x) {
  matchMatrix(x)[,1L]
})

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion
###

## return a matrix where each row indicates a match (query and subject index)

setMethod("as.matrix", "RangesMatching", function(x) {
  mm <- matchMatrix(x)
  ## if (is(mm, "ngCMatrix")) { ## 'i' holds non-zero rows, 'p' delimits cols
  ##   cbind(query = rep(seq_len(ncol(mm)), diff(mm@p)), subject = mm@i+1L)
  ## } else if (is(mm, "lgeMatrix")) {
  ##   mm <- as.matrix(mm)
  ##   cbind(query = col(mm)[mm], subject = row(mm)[mm])
  ## }
  mm
})

## count up the matches for each query

setMethod("as.table", "RangesMatching", function(x, ...) {
  mm <- matchMatrix(x)
  table(factor(mm[,1L], seq_len(ncol(x))))
  ## if (!ncol(mm)) ## as.table does not work for empty arrays
  ##   return(table(integer(), dnn="ranges"))
  ## if (is(mm, "ngCMatrix"))
  ##   counts <- diff(mm@p)
  ## else if (is(mm, "lgeMatrix"))
  ##   counts <- as.integer(colSums(mm))
  ## as.table(array(counts, ncol(mm), list(range = seq_len(ncol(mm)))))
})

setMethod("t", "RangesMatching", function(x) {
  m <- matchMatrix(x)[,2:1,drop=FALSE]
  colnames(m) <- rev(colnames(m))
  x@matchMatrix <- m
  x@DIM <- x@DIM[2:1]
  x
})

setMethod("ranges", "RangesMatching", function(x, query, subject) {
  if (!is(query, "Ranges") || length(query) != ncol(x))
    stop("'query' must be a Ranges of length equal to number of queries")
  if (!is(subject, "Ranges") || length(subject) != nrow(x))
    stop("'subject' must be a Ranges of length equal to number of subjects")
  m <- as.matrix(x)
  q <- query[m[,1L]]
  s <- subject[m[,2L]]
  IRanges(pmax(start(q), start(s)), pmin(end(s), end(q)))
})

### TODO: many convenience methods
