### =========================================================================
### RangesMatching objects
### -------------------------------------------------------------------------

setClass("RangesMatching", representation(matchMatrix = "Matrix"))


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Accessors
###

setGeneric("matchMatrix", function(object, ...) standardGeneric("matchMatrix"))
setMethod("matchMatrix", "RangesMatching", function(object) object@matchMatrix)

## setGeneric("rmatched", function(x, ...) standardGeneric("rmatched"))
## setMethod("rmatched", "RangesMatching", function(x) {
##   mm <- matchMatrix(x)
##   seq_len(nrow(mm)) %in% mm@i
## })

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion
###

## return a matrix where each row indicates a match (query and subject index)

setMethod("as.matrix", "RangesMatching", function(x) {
  mm <- matchMatrix(x)
  if (is(mm, "ngCMatrix")) { ## 'i' holds non-zero rows, 'p' delimits cols
    cbind(query = rep(seq_len(ncol(mm)), diff(mm@p)), subject = mm@i+1L)
  } else if (is(mm, "lgeMatrix")) {
    mm <- as.matrix(mm)
    cbind(query = col(mm)[mm], subject = row(mm)[mm])
  }
})

## count up the matches for each query

setMethod("as.table", "RangesMatching", function(x, ...) {
  mm <- matchMatrix(x)
  if (is(mm, "ngCMatrix"))
    counts <- diff(mm@p)
  else if (is(mm, "lgeMatrix"))
    counts <- as.integer(colSums(mm))
  as.table(array(counts, ncol(mm), list(range = seq_len(ncol(mm)))))
})

setMethod("t", "RangesMatching", function(x) {
  x@matchMatrix <- t(matchMatrix(x))
  x
})

setMethod("ranges", "RangesMatching", function(x, query, subject) {
  m <- as.matrix(x)
  q <- query[m[,1]]
  s <- subject[m[,2]]
  IRanges(pmax(start(q), start(s)), pmin(end(s), end(q)))
})

### TODO: many convenience methods
