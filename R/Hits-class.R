### =========================================================================
### Hits objects
### -------------------------------------------------------------------------

setClass("Hits",
    contains="Vector",
    representation(
        queryHits="integer",     # integer vector of length N
        subjectHits="integer",   # integer vector of length N
        queryLength="integer",   # single integer
        subjectLength="integer"  # single integer
    )
)

### TODO: Drop this class in BioC 2.11
setClass("RangesMatching", contains="Hits")

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Accessors
###

setGeneric("matchMatrix", function(x, ...) standardGeneric("matchMatrix"))
setMethod("matchMatrix", "Hits",
    function(x)
    {
        msg <- c("'matchMatrix' is deprecated.",
                 "Use 'as.matrix' instead.",
                 "See help(\"Deprecated\")")
        .Deprecated(msg=paste(msg, collapse="\n"))
        as.matrix(x)
    }
)

setMethod("dim", "Hits", function(x) {
  .Deprecated("queryLength or subjectLength")
  c(queryLength(x), subjectLength(x))
})

setMethod("length", "Hits", function(x) {
  length(queryHits(x))
})

setGeneric("queryHits", function(x, ...) standardGeneric("queryHits"))

setMethod("queryHits", "Hits", function(x) x@queryHits)

setGeneric("subjectHits", function(x, ...) standardGeneric("subjectHits"))

setMethod("subjectHits", "Hits", function(x) x@subjectHits)

setGeneric("queryLength", function(x, ...) standardGeneric("queryLength"))

setMethod("queryLength", "Hits", function(x) x@queryLength)

setGeneric("subjectLength", function(x, ...) standardGeneric("subjectLength"))

setMethod("subjectLength", "Hits", function(x) x@subjectLength)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Extraction
###

setMethod("[", "Hits",
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
              if (iInfo[["useIdx"]]) {
                x@queryHits <- x@queryHits[iInfo[["idx"]]]
                x@subjectHits <- x@subjectHits[iInfo[["idx"]]]
              }
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

setMethod("as.matrix", "Hits",
    function(x) cbind(queryHits=queryHits(x), subjectHits=subjectHits(x))
)

## a list, with an element for each query, containing the subject hits
setMethod("as.list", "Hits",
          function(x, values = seq(subjectLength(x))) {
            unname(split(values[subjectHits(x)],
                         factor(queryHits(x), levels = seq(queryLength(x)))))
          })

setAs("Hits", "list", function(from) as.list(from))
setAs("Hits", "List", function(from) castList(as.list(from)))
setAs("RangesMatching", "list", function(from) as.list(from))
setAs("RangesMatching", "List", function(from) castList(as.list(from)))

## count up the matches for each query

setMethod("as.table", "Hits", function(x, ...) {
  tabulate(queryHits(x), queryLength(x))
})

### FIXME: this needs a new name given the switch to Vector
setMethod("t", "Hits", function(x) {
  tmp <- x@queryHits
  x@queryHits <- x@subjectHits
  x@subjectHits <- tmp
  tmp <- x@queryLength
  x@queryLength <- x@subjectLength
  x@subjectLength <- tmp
  x
})

setMethod("ranges", "Hits", function(x, query, subject) {
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
