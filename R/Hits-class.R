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
    ),
    prototype(
        queryLength=0L,
        subjectLength=0L
    )
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Accessors
###

matchMatrix <- function(...)
{
    msg <- c("'matchMatrix' is defunct.",
             "Use 'as.matrix' instead.",
             "See help(\"Defunct\")")
    .Defunct(msg=paste(msg, collapse="\n"))
}

setMethod("dim", "Hits", function(x) {
  .Defunct("queryLength or subjectLength")
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
              if (is.character(i))
                stop("Cannot subset a Hits object by character")
              if (!(is.logical(i) || (is(i, "Rle") && is.logical(runValue(i))))
                  && any(duplicated(i)))
                stop("Elements in a non-logical 'i' cannot be duplicated, ",
                     "because the Hits object would no longer be a set.")
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

setAs("Hits", "DataFrame", function(from) {
  DataFrame(as.matrix(from),
            if (!is.null(mcols(from))) mcols(from)
            else new("DataFrame", nrows = length(from)))
})

setMethod("as.data.frame", "Hits",
    function(x, row.names=NULL, optional=FALSE, ...)
    {
        if (!(is.null(row.names) || is.character(row.names)))
            stop("'row.names' must be NULL or a character vector")
        if (!identical(optional, FALSE) || length(list(...)))
            warning("'optional' and arguments in '...' are ignored")
        as.data.frame(as(x, "DataFrame"), row.names = row.names)
    }
)

## a list, with an element for each query, containing the subject hits
setMethod("as.list", "Hits",
          function(x, values = seq_len(subjectLength(x))) {
            unname(split(values[subjectHits(x)],
                         factor(queryHits(x),
                                levels = seq_len(queryLength(x)))))
          })

setAs("Hits", "list", function(from) as.list(from))
setAs("Hits", "List", function(from) {
  unname(seqsplit(subjectHits(from),
                  factor(queryHits(from),
                         levels = seq_len(queryLength(from)))))
})

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
### Splitting / relisting.
###

setMethod("splitAsListReturnedClass", "Hits", function(x) "HitsList")


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Set operations
###

compatibleHits <- function(x, y) {
  subjectLength(x) == subjectLength(y) && queryLength(x) == queryLength(y)
}

setMethod("match", c("Hits", "Hits"),
    function(x, table, nomatch=NA_integer_, incomparables=NULL)
    {
        if (!compatibleHits(x, table))
            stop("'x' and 'table' are incompatible by subject and query length")
        if (!is.null(incomparables))
            stop("\"match\" method for Hits objects ",
                 "only accepts 'incomparables=NULL'")
        matchIntegerPairs(queryHits(x), subjectHits(x),
                          queryHits(table), subjectHits(table),
                          nomatch=nomatch)
    }
)

setMethod("setdiff", c("Hits", "Hits"), function(x, y) {
  if (!compatibleHits(x, y))
    stop("'x' and 'y' are incompatible by subject and query length")
  x[!(x %in% y)]
})

setMethod("union", c("Hits", "Hits"),
    function(x, y)
    {
        m <- match(y, x)
        y <- y[is.na(m)]
        q_hits <- c(queryHits(x), queryHits(y))
        s_hits <- c(subjectHits(x), subjectHits(y))
        oo <- orderIntegerPairs(q_hits, s_hits)
        q_hits <- q_hits[oo]
        s_hits <- s_hits[oo]
        new2("Hits",
             queryHits=q_hits, subjectHits=s_hits,
             queryLength=queryLength(x), subjectLength=subjectLength(x),
             check=FALSE)
    }
)

setMethod("intersect", c("Hits", "Hits"), function(x, y) {
  if (!compatibleHits(x, y))
    stop("'x' and 'y' are incompatible by subject and query length")
  x[x %in% y]
})

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Utilities
###

countHits <- tabulate

setGeneric("countSubjectHits",
           function(x, ...) standardGeneric("countSubjectHits"))

setMethod("countSubjectHits", "Hits", function(x) {
  countHits(subjectHits(x), subjectLength(x))
})

setGeneric("countQueryHits", function(x, ...) standardGeneric("countQueryHits"))

setMethod("countQueryHits", "Hits", function(x) {
  countHits(queryHits(x), queryLength(x))
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
  df_show <- capture.output(show(as(object, "DataFrame")))
  cat(paste(tail(df_show, -1), "\n"))
})


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Remap the query and/or subject hits.
###

### Returns 'arg' as a NULL, an integer vector, or a factor.
.normargMap <- function(arg, sidename, old.length)
{
    if (is.null(arg))
        return(arg)
    if (!is.factor(arg)) {
        if (!is.numeric(arg))
            stop("'" , sidename, ".map' must be a vector of integers")
        if (!is.integer(arg))
            arg <- as.integer(arg)
    }
    if (length(arg) != old.length)
        stop("'" , sidename, ".map' must have the length of the ", sidename)
    arg
}

.normargNewLength <- function(arg, sidename, map)
{
    if (!isSingleNumberOrNA(arg))
        stop("'new.", sidename, "Length' must be a single number or NA")
    if (!is.integer(arg))
        arg <- as.integer(arg)
    if (is.null(map)) {
        if (!is.na(arg))
            stop("'new.", sidename, "Length' must be NA ",
                 "when '" , sidename, ".map' is NULL")
        return(arg)
    }
    if (is.factor(map)) {
        if (is.na(arg))
            return(nlevels(map))
        if (arg < nlevels(map))
            stop("supplied 'new.", sidename, "Length' must ",
                 "be >= 'nlevels(", sidename, ".map)'")
        return(arg)
    }
    if (is.na(arg))
        stop("'new.", sidename, "Length' must be specified when ",
             "'" , sidename, ".map' is specified and is not a factor")
    arg
}

remapHits <- function(x, query.map=NULL, new.queryLength=NA,
                         subject.map=NULL, new.subjectLength=NA)
{
    if (!is(x, "Hits"))
        stop("'x' must be a Hits object")
    query.map <- .normargMap(query.map, "query", queryLength(x))
    new.queryLength <- .normargNewLength(new.queryLength,
                                         "query", query.map)
    subject.map <- .normargMap(subject.map, "subject", subjectLength(x))
    new.subjectLength <- .normargNewLength(new.subjectLength,
                                           "subject", subject.map)
    query_hits <- queryHits(x)
    subject_hits <- subjectHits(x)
    if (is.null(query.map)) {
        new.queryLength <- queryLength(x)
    } else {
        if (is.factor(query.map))
            query.map <- as.integer(query.map)
        if (anyMissingOrOutside(query.map, 1L, new.queryLength))
            stop("'query.map' cannot contain NAs, or values that ",
                 "are < 1, or > 'new.queryLength'")
        query_hits <- query.map[query_hits]
    }
    if (is.null(subject.map)) {
        new.subjectLength <- subjectLength(x)
    } else {
        if (is.factor(subject.map))
            subject.map <- as.integer(subject.map)
        if (anyMissingOrOutside(subject.map, 1L, new.subjectLength))
            stop("'subject.map' cannot contain NAs, or values that ",
                 "are < 1, or > 'new.subjectLength'")
        subject_hits <- subject.map[subject_hits]
    }
    not_dup <- !duplicatedIntegerPairs(query_hits, subject_hits)
    query_hits <- query_hits[not_dup]
    subject_hits <- subject_hits[not_dup]
    oo <- orderIntegerPairs(query_hits, subject_hits)
    new("Hits", queryHits=query_hits[oo], subjectHits=subject_hits[oo],
                queryLength=new.queryLength, subjectLength=new.subjectLength)
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### TODO: More convenience methods
###

