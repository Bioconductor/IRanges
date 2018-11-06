### =========================================================================
### findOverlaps (and related) methods
### -------------------------------------------------------------------------
###


## internal generic
setGeneric("process_self_hits",  signature="x", # not exported
    function(x, select=c("all", "first", "last", "arbitrary"),
                drop.self=FALSE, drop.redundant=FALSE)
        standardGeneric("process_self_hits"))

setMethod("process_self_hits", "SortedByQueryHits",
    function(x, select=c("all", "first", "last", "arbitrary"),
                drop.self=FALSE, drop.redundant=FALSE)
    {
        x <- as(x, "SortedByQuerySelfHits")
        select <- match.arg(select)
        if (!isTRUEorFALSE(drop.self))
            stop("'drop.self' must be TRUE or FALSE")
        if (!isTRUEorFALSE(drop.redundant))
            stop("'drop.redundant' must be TRUE or FALSE")
        if (drop.self) {
            self_idx <- which(isSelfHit(x))
            if (length(self_idx) != 0L)
                x <- x[-self_idx]
        }
        if (drop.redundant) {
            redundant_idx <- which(isRedundantHit(x))
            if (length(redundant_idx) != 0L)
                x <- x[-redundant_idx]
        }
        selectHits(x, select=select)
    }
)

setMethod("process_self_hits", "SortedByQueryHitsList",
    function(x, select=c("all", "first", "last", "arbitrary"),
                drop.self=FALSE, drop.redundant=FALSE)
    {
        x <- as(x, "SortedByQuerySelfHitsList")
        select <- match.arg(select)
        ans <- lapply(x, process_self_hits,
                         select, drop.self,  drop.redundant)
        if (select != "all")
            return(IntegerList(ans))
        S4Vectors:::new_SimpleList_from_list("HitsList",
                        ans,
                        subjectOffsets = x@subjectOffsets)
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### findOverlaps()
###
### Find objects in the query that overlap those in the subject.
###

setGeneric("findOverlaps", signature=c("query", "subject"),
    function(query, subject, maxgap=-1L, minoverlap=0L,
             type=c("any", "start", "end", "within", "equal"),
             select=c("all", "first", "last", "arbitrary"),
             ...)
        standardGeneric("findOverlaps")
)

findOverlaps_IntegerRanges <- function(query, subject,
             maxgap=-1L, minoverlap=0L,
             type=c("any", "start", "end", "within", "equal"),
             select=c("all", "first", "last", "arbitrary"))
{
    if (is.integer(query))
        query <- IRanges(query, width=1L)
    type <- match.arg(type)
    select <- match.arg(select)
    findOverlaps_NCList(query, subject,
                        maxgap=maxgap, minoverlap=minoverlap,
                        type=type, select=select)
}

setMethod("findOverlaps", c("IntegerRanges", "IntegerRanges"),
    findOverlaps_IntegerRanges
)

setMethod("findOverlaps", c("integer", "IntegerRanges"),
    findOverlaps_IntegerRanges
)

setMethod("findOverlaps", c("Vector", "missing"),
    function(query, subject, maxgap=-1L, minoverlap=0L,
             type=c("any", "start", "end", "within", "equal"),
             select=c("all", "first", "last", "arbitrary"),
             ...,
             drop.self=FALSE, drop.redundant=FALSE)
    {
        select <- match.arg(select)
        result <- findOverlaps(query, query,
                               maxgap=maxgap, minoverlap=minoverlap,
                               type=match.arg(type), select="all",
                               ...)
        process_self_hits(result, select, drop.self, drop.redundant)
    }
)

# might consider making this the following:
# setMethod("findOverlaps", c("IntegerRangesList", "IntegerRangesList"),
#           function(query, subject, maxgap = -1L, minoverlap = 0L,
#                    type = c("any", "start", "end", "within", "equal"),
#                    select = c("all", "first", "last", "arbitrary"),
#                    drop = FALSE)
#           {
#             findOverlaps(query, NCLists(query),
#               maxgap = maxgap, minoverlap = minoverlap,
#               type = match.arg(type), select = match.arg(select), drop = drop)
#           }
#   )

setMethod("findOverlaps", c("IntegerRangesList", "IntegerRangesList"),
          function(query, subject, maxgap = -1L, minoverlap = 0L,
                   type = c("any", "start", "end", "within", "equal"),
                   select = c("all", "first", "last", "arbitrary"),
                   drop = FALSE)
          {
            type <- match.arg(type)
            select <- match.arg(select)

            query <- as.list(query)
            subject <- as.list(subject)
            origSubject <- subject
            if (!is.null(names(subject)) && !is.null(names(query))) {
              subject <- subject[names(query)]
              names(subject) <- names(query) # get rid of NA's in names
            } else {
              subject <- subject[seq_along(query)]
            }
            ## NULL's are introduced where they do not match
            ## We replace those with empty IRanges
            subject[S4Vectors:::sapply_isNULL(subject)] <- list(IRanges())

            ans <- lapply(seq_len(length(subject)), function(i) {
              findOverlaps(query[[i]], subject[[i]],
                           maxgap = maxgap, minoverlap = minoverlap,
                           type = type, select = select)
            })
            names(ans) <- names(subject)
            if (select == "all") {
              ans <- HitsList(ans, origSubject)
            } else if (drop) {
              off <- head(c(0L, cumsum(sapply(origSubject, length))), -1)
              names(off) <- names(origSubject)
              if (is.null(names(ans)))
                off <- off[seq_along(ans)]
              else
                off <- off[names(ans)]
              ans <-
                unlist(ans, use.names=FALSE) +
                  rep.int(unname(off), sapply(ans, length))
            } else {
              ans <- IntegerList(ans)
            }
            ans
          })

setMethod("findOverlaps", c("RangedData", "RangedData"),
          function(query, subject, maxgap = -1L, minoverlap = 0L,
                   type = c("any", "start", "end", "within", "equal"),
                   select = c("all", "first", "last", "arbitrary"),
                   drop = FALSE)
          {
            .Deprecated(msg=wmsg2(RangedData_is_deprecated_msg))
            findOverlaps(ranges(query), ranges(subject),
                         maxgap = maxgap, minoverlap = minoverlap,
                         type = match.arg(type), select = match.arg(select),
                         drop = drop)
          })

setMethod("findOverlaps", c("RangedData", "IntegerRangesList"),
          function(query, subject, maxgap = -1L, minoverlap = 0L,
                   type = c("any", "start", "end", "within", "equal"),
                   select = c("all", "first", "last", "arbitrary"),
                   drop = FALSE)
          {
            .Deprecated(msg=wmsg2(RangedData_is_deprecated_msg))
            findOverlaps(ranges(query), subject,
                         maxgap = maxgap, minoverlap = minoverlap,
                         type = match.arg(type), select = match.arg(select),
                         drop = drop)
          })

setMethod("findOverlaps", c("IntegerRangesList", "RangedData"),
          function(query, subject, maxgap = -1L, minoverlap = 0L,
                   type = c("any", "start", "end", "within", "equal"),
                   select = c("all", "first", "last", "arbitrary"),
                   drop = FALSE)
          {
            .Deprecated(msg=wmsg2(RangedData_is_deprecated_msg))
            findOverlaps(query, ranges(subject),
                         maxgap = maxgap, minoverlap = minoverlap,
                         type = match.arg(type), select = match.arg(select),
                         drop = drop)
          })

setMethod("findOverlaps", c("Pairs", "missing"),
          function (query, subject, maxgap = -1L, minoverlap = 0L,
                    type = c("any",  "start", "end", "within", "equal"),
                    select = c("all", "first", "last", "arbitrary"), ...) {
              findOverlaps(zipup(query), maxgap=maxgap,
                           minoverlap=minoverlap, type=type, select=select, ...)
          })

setMethod("findOverlaps", c("Pairs", "ANY"),
          function (query, subject, maxgap = -1L, minoverlap = 0L,
                    type = c("any",  "start", "end", "within", "equal"),
                    select = c("all", "first", "last", "arbitrary"), ...) {
              findOverlaps(zipup(query), subject, maxgap=maxgap,
                           minoverlap=minoverlap, type=type, select=select, ...)
          })

setMethod("findOverlaps", c("ANY", "Pairs"),
          function (query, subject, maxgap = -1L, minoverlap = 0L,
                    type = c("any",  "start", "end", "within", "equal"),
                    select = c("all", "first", "last", "arbitrary"), ...) {
              findOverlaps(query, zipup(subject), maxgap=maxgap,
                           minoverlap=minoverlap, type=type, select=select, ...)
          })

setMethod("findOverlaps", c("Pairs", "Pairs"),
          function (query, subject, maxgap = -1L, minoverlap = 0L,
                    type = c("any",  "start", "end", "within", "equal"),
                    select = c("all", "first", "last", "arbitrary"), ...) {
              findOverlaps(zipup(query), zipup(subject),
                           maxgap=maxgap, minoverlap=minoverlap, type=type,
                           select=select, ...)
          })

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### countOverlaps()
###

setGeneric("countOverlaps", signature=c("query", "subject"),
    function(query, subject, maxgap=-1L, minoverlap=0L,
             type=c("any", "start", "end", "within", "equal"),
             ...)
        standardGeneric("countOverlaps")
)

.countOverlaps_default <-
    function(query, subject, maxgap=-1L, minoverlap=0L,
             type=c("any", "start", "end", "within", "equal"),
             ...)
{
    if (is.integer(query))
        query <- IRanges(query, width=1L)
    type <- match.arg(type)
    if (missing(subject)) {
        hits <- findOverlaps(query,
                             maxgap=maxgap, minoverlap=minoverlap,
                             type=type,
                             ...)
    } else {
        hits <- findOverlaps(query, subject,
                             maxgap=maxgap, minoverlap=minoverlap,
                             type=type,
                             ...)
    }
    ans <- countQueryHits(hits)
    names(ans) <- names(query)
    ans
}

setMethod("countOverlaps", c("Vector", "Vector"), .countOverlaps_default)
setMethod("countOverlaps", c("integer", "Vector"), .countOverlaps_default)
setMethod("countOverlaps", c("Vector", "missing"), .countOverlaps_default)

countOverlaps_IntegerRanges <- function(query, subject,
              maxgap=-1L, minoverlap=0L,
              type=c("any", "start", "end", "within", "equal"))
{
    type <- match.arg(type)
    ans <- findOverlaps_NCList(query, subject,
                               maxgap=maxgap, minoverlap=minoverlap,
                               type=type, select="count")
    names(ans) <- names(query)
    ans
}

setMethod("countOverlaps", c("IntegerRanges", "IntegerRanges"),
    countOverlaps_IntegerRanges
)

setMethod("countOverlaps", c("IntegerRangesList", "IntegerRangesList"),
          function(query, subject, maxgap=-1L, minoverlap=0L,
                   type = c("any", "start", "end", "within", "equal"))
          {
              IntegerList(mapply(countOverlaps, query, subject,
                              MoreArgs = list(maxgap = maxgap,
                                      minoverlap = minoverlap,
                                      type = match.arg(type)),
                              SIMPLIFY = FALSE))
          })

.countOverlaps_RangedData_defunct_msg <- c(
    "RangedData objects are deprecated and the \"countOverlaps\" methods ",
    "for RangedData objects are now defunct. ",
    "Please migrate your code to use GRanges or GRangesList objects instead. ",
    "See IMPORTANT NOTE in ?RangedData"
)

setMethod("countOverlaps", c("RangedData", "RangedData"),
          function(query, subject, maxgap = -1L, minoverlap = 0L,
                   type = c("any", "start", "end", "within", "equal"))
          {
              .Defunct(msg=wmsg(.countOverlaps_RangedData_defunct_msg))
              countOverlaps(ranges(query), ranges(subject), maxgap = maxgap,
                            minoverlap = minoverlap, type = match.arg(type))
          })
setMethod("countOverlaps", c("RangedData", "IntegerRangesList"),
          function(query, subject, maxgap = -1L, minoverlap = 0L,
                   type = c("any", "start", "end", "within", "equal"))
          {
              .Defunct(msg=wmsg(.countOverlaps_RangedData_defunct_msg))
              countOverlaps(ranges(query), subject, maxgap = maxgap,
                            minoverlap = minoverlap, type = match.arg(type))
          })
setMethod("countOverlaps", c("IntegerRangesList", "RangedData"),
          function(query, subject, maxgap = -1L, minoverlap = 0L,
                   type = c("any", "start", "end", "within", "equal"))
          {
              .Defunct(msg=wmsg(.countOverlaps_RangedData_defunct_msg))
              countOverlaps(query, ranges(subject), maxgap = maxgap,
                            minoverlap = minoverlap, type = match.arg(type))
          })


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### overlapsAny()
###

### Same args and signature as countOverlaps().
setGeneric("overlapsAny", signature=c("query", "subject"),
    function(query, subject, maxgap=-1L, minoverlap=0L,
             type=c("any", "start", "end", "within", "equal"),
             ...)
        standardGeneric("overlapsAny")
)

.overlapsAny_default <-
    function(query, subject, maxgap=-1L, minoverlap=0L,
             type=c("any", "start", "end", "within", "equal"),
             ...)
{
    if (is.integer(query))
        query <- IRanges(query, width=1L)
    type <- match.arg(type)
    if (missing(subject)) {
        ahit <- findOverlaps(query,
                             maxgap=maxgap, minoverlap=minoverlap,
                             type=type, select="arbitrary",
                             ...)
    } else {
        ahit <- findOverlaps(query, subject,
                             maxgap=maxgap, minoverlap=minoverlap,
                             type=type, select="arbitrary",
                             ...)
    }
    !is.na(ahit)
}

setMethod("overlapsAny", c("Vector", "Vector"), .overlapsAny_default)
setMethod("overlapsAny", c("integer", "Vector"), .overlapsAny_default)
setMethod("overlapsAny", c("Vector", "missing"), .overlapsAny_default)

setMethod("overlapsAny", c("IntegerRangesList", "IntegerRangesList"),
    function(query, subject, maxgap=-1L, minoverlap=0L,
             type=c("any", "start", "end", "within", "equal"),
             ...)
    {
        query <- as.list(query)
        subject <- as.list(subject)
        type <- match.arg(type)
        if (!is.null(names(query)) && !is.null(names(subject))) {
            subject <- subject[names(query)]
            names(subject) <- names(query) # get rid of NA's in names
        } else {
            subject <- subject[seq_along(query)]
        }
        ## NULL's are introduced where they do not match
        ## We replace those with empty IRanges
        subject[S4Vectors:::sapply_isNULL(subject)] <- list(IRanges())
        LogicalList(lapply(structure(seq_len(length(query)),
                                     names = names(query)),
                           function(i)
                             overlapsAny(query[[i]], subject[[i]],
                                         maxgap=maxgap, minoverlap=minoverlap,
                                         type=type,
                                         ...)))
    }
)

### Convenience wrappers for the 3 most common use cases.
`%over%` <- function(query, subject) overlapsAny(query, subject)
`%within%` <- function(query, subject) overlapsAny(query, subject,
                                                   type="within")
`%outside%` <- function(query, subject) !overlapsAny(query, subject)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### subsetByOverlaps()
###

### First 2 arguments are 'x' and 'ranges' like for the
### transcriptsByOverlaps(), exonsByOverlaps(), and cdsByOverlaps() functions
### from the GenomicFeatures package and the snpsByOverlaps() function from
### the BSgenome package.
setGeneric("subsetByOverlaps", signature=c("x", "ranges"),
    function(x, ranges, maxgap=-1L, minoverlap=0L,
             type=c("any", "start", "end", "within", "equal"),
             invert=FALSE, ...)
        standardGeneric("subsetByOverlaps")
)

setMethod("subsetByOverlaps", c("Vector", "Vector"),
    function(x, ranges, maxgap=-1L, minoverlap=0L,
             type=c("any", "start", "end", "within", "equal"), invert=FALSE,
             ...)
    {
        ov_any <- overlapsAny(x, ranges,
                              maxgap=maxgap, minoverlap=minoverlap,
                              type=match.arg(type),
                              ...)
        if (invert)
            ov_any <- !ov_any
        x[ov_any]
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### overlapsRanges()
###
### Extracts the actual regions of intersection between the overlapping ranges.
###

setGeneric("overlapsRanges", signature=c("query", "subject"),
    function(query, subject, hits=NULL, ...) standardGeneric("overlapsRanges")
)

setMethod("overlapsRanges", c("IntegerRanges", "IntegerRanges"),
    function(query, subject, hits=NULL, ...)
    {
        if (is.null(hits)) {
            hits <- findOverlaps(query, subject, ...)
        } else {
            if (!is(hits, "Hits"))
                stop("'hits' must be a Hits object")
            if (length(list(...)) != 0L)
                stop(wmsg("Extra arguments are only accepted when the 'hits' ",
                          "argument is not supplied, in which case they are ",
                          "passed to the internal call to findOverlaps(). ",
                          "See ?overlapsRanges for more information."))
            if (queryLength(hits) != length(query) ||
                subjectLength(hits) != length(subject))
                stop("'hits' is not compatible with 'query' and 'subject'")
        }
        ### Could be replaced by 1-liner:
        ###   pintersect(query[queryHits(hits)], subject[subjectHits(hits)])
        ### but will fail if 'query' or 'subject' is a kind of IntegerRanges
        ### object that cannot be subsetted (e.g. Partitioning object).
        m <- as.matrix(hits)
        qstart <- start(query)[m[,1L]]
        qend <- end(query)[m[,1L]]
        sstart <- start(subject)[m[,2L]]
        send <- end(subject)[m[,2L]]
        IRanges(pmax.int(qstart, sstart), pmin.int(send, qend))
    }
)

setMethod("overlapsRanges", c("IntegerRangesList", "IntegerRangesList"),
    function(query, subject, hits=NULL, ...)
    {
        if (is.null(hits)) {
            hits <- findOverlaps(query, subject, ...)
        } else {
            if (!is(hits, "HitsList"))
                stop("'hits' must be a HitsList object")
            if (length(list(...)) != 0L)
                stop(wmsg("Extra arguments are only accepted when the 'hits' ",
                          "argument is not supplied, in which case they are ",
                          "passed to the internal call to findOverlaps(). ",
                          "See ?overlapsRanges for more information."))
            if (length(hits) != length(query) ||
                length(hits) != length(subject))
                stop("'query', 'subject', and 'hits' must have the same length")
        }
        queries <- as.list(query, use.names = FALSE)
        subjects <- as.list(subject, use.names = FALSE)
        els <- as.list(hits, use.names = FALSE)
        ans <- lapply(seq_len(length(hits)),
            function(i) overlapsRanges(queries[[i]], subjects[[i]], els[[i]]))
        ans <- as(ans, "SimpleIRangesList")
        names(ans) <- names(hits)
        ans
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### poverlaps()
###

setGeneric("poverlaps", signature=c("query", "subject"),
           function(query, subject, maxgap=0L, minoverlap=1L,
                    type=c("any", "start", "end", "within", "equal"),
                    ...)
               standardGeneric("poverlaps")
           )

setMethod("poverlaps", c("IntegerRanges", "IntegerRanges"),
          function(query, subject, maxgap=0L, minoverlap=1L,
                   type=c("any", "start", "end", "within", "equal"))
          {
              stopifnot(isSingleNumber(maxgap))
              stopifnot(isSingleNumber(minoverlap))
              type <- match.arg(type)
              if (type == "any") {
                  query <- query + maxgap
              } else if (type == "within") {
                  if (maxgap > 0L) {
                      warning("'maxgap' is ignored when type=='within'")
                  }
                  return(start(query) >= start(subject) &
                             end(query) <= end(subject) &
                                 width(query) >= minoverlap)
              }
              amount <- pmin(end(query), end(subject)) -
                  pmax(start(query), start(subject)) + 1L
              overlaps <- amount >= minoverlap
              samePos <- function(x, y) {
                  x <= (y + maxgap) & x >= (y - maxgap)
              }
              keep <- switch(type,
                             any = TRUE,
                             start = samePos(start(query), start(subject)),
                             end = samePos(end(query), end(subject)),
                             equal = samePos(start(query), start(subject)) &
                                 samePos(end(query), end(subject)))
             overlaps & keep
          }
          )

setMethod("poverlaps", c("integer", "IntegerRanges"),
          function(query, subject, maxgap=0L, minoverlap=1L,
                   type=c("any", "start", "end", "within", "equal"))
          {
              poverlaps(IRanges(query, width=1L), subject,
                        maxgap=maxgap, minoverlap=minoverlap,
                        type=match.arg(type))
          })

setMethod("poverlaps", c("IntegerRanges", "integer"),
          function(query, subject, maxgap=0L, minoverlap=1L,
                   type=c("any", "start", "end", "within", "equal"))
          {
              poverlaps(query, IRanges(subject, width=1L),
                        maxgap=maxgap, minoverlap=minoverlap,
                        type=match.arg(type))
          })

### Convenience operators for poverlaps()
`%pover%` <- function(query, subject) poverlaps(query, subject)
`%pwithin%` <- function(query, subject) poverlaps(query, subject, type="within")
`%poutside%` <- function(query, subject) !poverlaps(query, subject)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Merge two sets of ranges by overlap into a DataFrame
###

mergeByOverlaps <- function(query, subject, ...) {
  hits <- findOverlaps(query, subject, ...)
  query_df <- as(extractROWS(query, queryHits(hits)), "DataFrame")
  colnames(query_df)[1L] <- deparse(substitute(query))
  subject_df <- as(extractROWS(subject, subjectHits(hits)), "DataFrame")
  colnames(subject_df)[1L] <- deparse(substitute(subject))
  cbind(query_df, subject_df)
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Convenience for dereferencing overlap hits to a Pairs
###

findOverlapPairs <- function(query, subject, ...) {
    hits <- findOverlaps(query, subject, ...)
    if (missing(subject)) {
        subject <- query
    }
    Pairs(query, subject, hits=hits)
}
