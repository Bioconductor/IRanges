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
                i <- iInfo[["idx"]]
                x@queryHits <- x@queryHits[i]
                x@subjectHits <- x@subjectHits[i]
                x@elementMetadata <- x@elementMetadata[i,,drop=FALSE]
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

## return a matrix where each row indicates a hit (query and subject index)

setMethod("as.matrix", "Hits",
    function(x) cbind(queryHits=queryHits(x), subjectHits=subjectHits(x))
)

setMethod("as.data.frame", "Hits",
    function(x, row.names=NULL, optional=FALSE, ...)
    {
        if (!(is.null(row.names) || is.character(row.names)))
            stop("'row.names' must be NULL or a character vector")
        if (!identical(optional, FALSE) || length(list(...)))
            warning("'optional' and arguments in '...' are ignored")
        data.frame(queryHits=queryHits(x),
                   subjectHits=subjectHits(x),
                   row.names=row.names,
                   check.names=FALSE,
                   stringsAsFactors=FALSE)
    }
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

## count up the hits for each query

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

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### makeAllGroupInnerHits()
###
### NOT exported.

### About 10x faster and uses 4x less memory than my first attempt in pure
### R below.
makeAllGroupInnerHits <- function(group.sizes, hit.type=0L)
{
    if (!is.integer(group.sizes))
        stop("'group.sizes' must be an integer vector")
    if (!isSingleNumber(hit.type))
        stop("'hit.type' must be a single integer")
    if (!is.integer(hit.type))
        hit.type <- as.integer(hit.type)
    .Call2("make_all_group_inner_hits", group.sizes, hit.type,
           PACKAGE="IRanges")
}

### TODO: Remove this.
makeAllGroupInnerHits.old <- function(GS)
{
    NG <- length(GS)  # nb of groups
    ## First Element In group i.e. first elt associated with each group.
    FEIG <- cumsum(c(1L, GS[-NG]))
    GSr <- c(0L, GS[-NG])
    CGSr2 <- cumsum(GSr * GSr)
    GS2 <- GS * GS
    N <- sum(GS)  # length of original vector (i.e. before grouping)

    ## Original Group Size Assignment i.e. group size associated with each
    ## element in the original vector.
    OGSA <- rep.int(GS, GS)  # has length N
    query_hits <- rep.int(seq_len(N), OGSA)
    NH <- length(query_hits)  # same as sum(GS2)

    ## Hit Group Assignment i.e. group associated with each hit.
    HGA <- rep.int(seq_len(NG), GS2)
    ## Hit Group Size Assignment i.e. group size associated with each hit.
    HGSA <- GS[HGA]
    subject_hits <- (0:(NH-1L) - CGSr2[HGA]) %% GS[HGA] + FEIG[HGA]
    new2("Hits", queryHits=query_hits, subjectHits=subject_hits,
                 queryLength=N, subjectLength=N, check=FALSE)
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Show
###

setMethod("show", "Hits", function(object) {
  cat("Hits of length ", length(object), "\n", sep = "")
  cat("queryLength: ", queryLength(object), "\n", sep = "")
  cat("subjectLength: ", subjectLength(object), "\n", sep = "")
  cat(labeledLine("queryHits", queryHits(object), count = FALSE))
  cat(labeledLine("subjectHits", subjectHits(object), count = FALSE))
})

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### TODO: many convenience methods

