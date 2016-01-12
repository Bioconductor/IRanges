### =========================================================================
### findOverlaps (and related) methods
### -------------------------------------------------------------------------
###


## internal generic
setGeneric("processSelfMatching",  signature="x", # not exported
    function(x, select=c("all", "first", "last", "arbitrary"),
                ignoreSelf=FALSE, ignoreRedundant=FALSE)
        standardGeneric("processSelfMatching"))

setMethod("processSelfMatching", "Hits",
    function(x, select=c("all", "first", "last", "arbitrary"),
                ignoreSelf=FALSE, ignoreRedundant=FALSE)
    {
        select <- match.arg(select)
        if (!isTRUEorFALSE(ignoreSelf))
            stop("'ignoreSelf' must be TRUE or FALSE")
        if (!isTRUEorFALSE(ignoreRedundant))
            stop("'ignoreRedundant' must be TRUE or FALSE")
        if (ignoreSelf) {
            self_idx <- which(isSelfHit(x))
            if (length(self_idx) != 0L)
                x <- x[-self_idx]
        }
        if (ignoreRedundant) {
            redundant_idx <- which(isRedundantHit(x))
            if (length(redundant_idx) != 0L)
                x <- x[-redundant_idx]
        }
        selectHits(x, select=select)
    }
)

setMethod("processSelfMatching", "HitsList",
    function(x, select=c("all", "first", "last", "arbitrary"),
                ignoreSelf=FALSE, ignoreRedundant=FALSE)
    {
        select <- match.arg(select)
        ans <- lapply(x, processSelfMatching,
                         select, ignoreSelf,  ignoreRedundant)
        if (select != "all")
            return(IntegerList(ans))
        S4Vectors:::new_SimpleList_from_list("HitsList",
                        ans,
                        subjectOffsets = x@subjectOffsets)
    }
)

setMethod("processSelfMatching", "CompressedHitsList",
    function(x, select=c("all", "first", "last", "arbitrary"),
                ignoreSelf=FALSE, ignoreRedundant=FALSE)
    {
        select <- match.arg(select)
        ans <- processSelfMatching(x@unlistData,
                                   select, ignoreSelf, ignoreRedundant)
        relist(ans, x)
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### findOverlaps()
###
### Find objects in the query that overlap those in the subject.
###

setGeneric("findOverlaps", signature=c("query", "subject"),
    function(query, subject, maxgap=0L, minoverlap=1L,
             type=c("any", "start", "end", "within", "equal"),
             select=c("all", "first", "last", "arbitrary"),
             ...)
        standardGeneric("findOverlaps")
)

findOverlaps_Ranges <- function(query, subject,
             maxgap=0L, minoverlap=1L,
             type=c("any", "start", "end", "within", "equal"),
             select=c("all", "first", "last", "arbitrary"))
{
    type <- match.arg(type)
    select <- match.arg(select)
    findOverlaps_NCList(query, subject,
                        maxgap=maxgap, minoverlap=minoverlap,
                        type=type, select=select)
}

setMethod("findOverlaps", c("Ranges", "Ranges"), findOverlaps_Ranges)

setMethod("findOverlaps", c("Vector", "missing"),
    function(query, subject, maxgap=0L, minoverlap=1L,
             type=c("any", "start", "end", "within", "equal"),
             select=c("all", "first", "last", "arbitrary"),
             ...,
             ignoreSelf=FALSE, ignoreRedundant=FALSE)
    {
        select <- match.arg(select)
        result <- findOverlaps(query, query,
                               maxgap=maxgap, minoverlap=minoverlap,
                               type=match.arg(type), select="all",
                               ...)
        processSelfMatching(result, select, ignoreSelf, ignoreRedundant)
    }
)

setMethod("findOverlaps", c("integer", "Ranges"),
    function(query, subject, maxgap=0L, minoverlap=1L,
             type=c("any", "start", "end", "within", "equal"),
             select=c("all", "first", "last", "arbitrary"))
    {
        findOverlaps(IRanges(query, query), subject,
                     maxgap=maxgap, minoverlap=minoverlap,
                     type=match.arg(type), select=match.arg(select))
    }
)

setMethod("findOverlaps", c("Views", "Views"),
    function(query, subject, maxgap=0L, minoverlap=1L,
             type=c("any", "start", "end", "within", "equal"),
             select=c("all", "first", "last", "arbitrary"))
    {
        findOverlaps(ranges(query), ranges(subject),
                     maxgap=maxgap, minoverlap=minoverlap,
                     type=match.arg(type), select=match.arg(select))
    }
)

setMethod("findOverlaps", c("Views", "Vector"),
    function(query, subject, maxgap=0L, minoverlap=1L,
             type=c("any", "start", "end", "within", "equal"),
             select=c("all", "first", "last", "arbitrary"))
    {
        findOverlaps(ranges(query), subject,
                     maxgap=maxgap, minoverlap=minoverlap,
                     type=match.arg(type), select=match.arg(select))
    }
)

setMethod("findOverlaps", c("Vector", "Views"),
    function(query, subject, maxgap=0L, minoverlap=1L,
             type=c("any", "start", "end", "within", "equal"),
             select=c("all", "first", "last", "arbitrary"))
    {
        findOverlaps(query, ranges(subject),
                     maxgap=maxgap, minoverlap=minoverlap,
                     type=match.arg(type), select=match.arg(select))
    }
)

# might consider making this the following:
# setMethod("findOverlaps", c("RangesList", "RangesList"),
#           function(query, subject, maxgap = 0L, minoverlap = 1L,
#                    type = c("any", "start", "end", "within", "equal"),
#                    select = c("all", "first", "last", "arbitrary"),
#                    drop = FALSE)
#           {
#             findOverlaps(query, NCLists(query),
#               maxgap = maxgap, minoverlap = minoverlap,
#               type = match.arg(type), select = match.arg(select), drop = drop)
#           }
#   )

setMethod("findOverlaps", c("RangesList", "RangesList"),
          function(query, subject, maxgap = 0L, minoverlap = 1L,
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
            subject[sapply(subject, is.null)] <- IRanges()

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


setMethod("findOverlaps", c("ViewsList", "ViewsList"),
    function(query, subject, maxgap=0L, minoverlap=1L,
             type=c("any", "start", "end", "within", "equal"),
             select=c("all", "first", "last", "arbitrary"),
             drop=FALSE)
    {
        findOverlaps(ranges(query), ranges(subject),
                     maxgap=maxgap, minoverlap=minoverlap,
                     type=match.arg(type), select=match.arg(select),
                     drop=drop)
    }
)

setMethod("findOverlaps", c("ViewsList", "Vector"),
    function(query, subject, maxgap=0L, minoverlap=1L,
             type=c("any", "start", "end", "within", "equal"),
             select=c("all", "first", "last", "arbitrary"),
             drop=FALSE)
    {
        findOverlaps(ranges(query), subject,
                     maxgap=maxgap, minoverlap=minoverlap,
                     type=match.arg(type), select=match.arg(select),
                     drop=drop)
    }
)

setMethod("findOverlaps", c("Vector", "ViewsList"),
    function(query, subject, maxgap=0L, minoverlap=1L,
             type=c("any", "start", "end", "within", "equal"),
             select=c("all", "first", "last", "arbitrary"),
             drop=FALSE)
    {
        findOverlaps(query, ranges(subject),
                     maxgap=maxgap, minoverlap=minoverlap,
                     type=match.arg(type), select=match.arg(select),
                     drop=drop)
    }
)

setMethod("findOverlaps", c("RangedData", "RangedData"),
          function(query, subject, maxgap = 0L, minoverlap = 1L,
                   type = c("any", "start", "end", "within", "equal"),
                   select = c("all", "first", "last", "arbitrary"),
                   drop = FALSE)
          {
            findOverlaps(ranges(query), ranges(subject),
                         maxgap = maxgap, minoverlap = minoverlap,
                         type = match.arg(type), select = match.arg(select),
                         drop = drop)
          })

setMethod("findOverlaps", c("RangedData", "RangesList"),
          function(query, subject, maxgap = 0L, minoverlap = 1L,
                   type = c("any", "start", "end", "within", "equal"),
                   select = c("all", "first", "last", "arbitrary"),
                   drop = FALSE)
          {
            findOverlaps(ranges(query), subject,
                         maxgap = maxgap, minoverlap = minoverlap,
                         type = match.arg(type), select = match.arg(select),
                         drop = drop)
          })

setMethod("findOverlaps", c("RangesList", "RangedData"),
          function(query, subject, maxgap = 0L, minoverlap = 1L,
                   type = c("any", "start", "end", "within", "equal"),
                   select = c("all", "first", "last", "arbitrary"),
                   drop = FALSE)
          {
            findOverlaps(query, ranges(subject),
                         maxgap = maxgap, minoverlap = minoverlap,
                         type = match.arg(type), select = match.arg(select),
                         drop = drop)
          })


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### countOverlaps()
###

setGeneric("countOverlaps", signature = c("query", "subject"),
    function(query, subject, maxgap = 0L, minoverlap = 1L,
             type = c("any", "start", "end", "within", "equal"),
             ...)
        standardGeneric("countOverlaps")
)

countOverlaps_Ranges <- function(query, subject,
              maxgap=0L, minoverlap=1L,
              type=c("any", "start", "end", "within", "equal"))
{
    type <- match.arg(type)
    ans <- findOverlaps_NCList(query, subject,
                               maxgap=maxgap, minoverlap=minoverlap,
                               type=type, select="count")
    names(ans) <- names(query)
    ans
}

setMethod("countOverlaps", c("Ranges", "Ranges"), countOverlaps_Ranges)

setMethod("countOverlaps", c("Vector", "Vector"),
    function(query, subject, maxgap = 0L, minoverlap = 1L,
             type = c("any", "start", "end", "within", "equal"),
             ...)
    {
        type <- match.arg(type)
        ov <- findOverlaps(query, subject, maxgap = maxgap,
                           minoverlap = minoverlap, type = type, ...)
        ans <- countQueryHits(ov)
        names(ans) <- names(query)
        ans
    }
)

setMethod("countOverlaps", c("Vector", "missing"),
    function(query, subject, maxgap = 0L, minoverlap = 1L,
             type = c("any", "start", "end", "within", "equal"))
    {
        countOverlaps(query, query, maxgap = maxgap,
                      minoverlap = minoverlap, type = type)
    }
)

setMethod("countOverlaps", c("RangesList", "RangesList"),
          function(query, subject, maxgap = 0L, minoverlap = 1L,
                   type = c("any", "start", "end", "within", "equal"))
          {
              IntegerList(mapply(countOverlaps, query, subject,
                              MoreArgs = list(maxgap = maxgap,
                                      minoverlap = minoverlap,
                                      type = match.arg(type)),
                              SIMPLIFY = FALSE))
          })

setMethod("countOverlaps", c("ViewsList", "ViewsList"),
    function(query, subject, maxgap=0L, minoverlap=1L,
             type=c("any", "start", "end", "within", "equal"))
    {
         countOverlaps(ranges(query), ranges(subject),
                       maxgap=maxgap, minoverlap=minoverlap,
                       type=type)
    }
)

setMethod("countOverlaps", c("ViewsList", "Vector"),
    function(query, subject, maxgap=0L, minoverlap=1L,
             type=c("any", "start", "end", "within", "equal"))
    {
         countOverlaps(ranges(query), subject,
                       maxgap=maxgap, minoverlap=minoverlap,
                       type=type)
    }
)

setMethod("countOverlaps", c("Vector", "ViewsList"),
    function(query, subject, maxgap=0L, minoverlap=1L,
             type=c("any", "start", "end", "within", "equal"))
    {
         countOverlaps(query, ranges(subject),
                       maxgap=maxgap, minoverlap=minoverlap,
                       type=type)
    }
)

setMethod("countOverlaps", c("RangedData", "RangedData"),
          function(query, subject, maxgap = 0L, minoverlap = 1L,
                   type = c("any", "start", "end", "within", "equal"))
          {
              countOverlaps(ranges(query), ranges(subject), maxgap = maxgap,
                            minoverlap = minoverlap, type = match.arg(type))
          })
setMethod("countOverlaps", c("RangedData", "RangesList"),
          function(query, subject, maxgap = 0L, minoverlap = 1L,
                   type = c("any", "start", "end", "within", "equal"))
          {
              countOverlaps(ranges(query), subject, maxgap = maxgap,
                            minoverlap = minoverlap, type = match.arg(type))
          })
setMethod("countOverlaps", c("RangesList", "RangedData"),
          function(query, subject, maxgap = 0L, minoverlap = 1L,
                   type = c("any", "start", "end", "within", "equal"))
          {
              countOverlaps(query, ranges(subject), maxgap = maxgap,
                            minoverlap = minoverlap, type = match.arg(type))
          })


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### overlapsAny()
###

### Same args and signature as countOverlaps() and subsetByOverlaps().
setGeneric("overlapsAny", signature=c("query", "subject"),
    function(query, subject, maxgap=0L, minoverlap=1L,
             type=c("any", "start", "end", "within", "equal"),
             ...)
        standardGeneric("overlapsAny")
)

setMethod("overlapsAny", c("Ranges", "Ranges"),
    function(query, subject, maxgap=0L, minoverlap=1L,
             type=c("any", "start", "end", "within", "equal"),
             ...)
    {
        !is.na(findOverlaps(query, subject,
                            maxgap=maxgap, minoverlap=minoverlap,
                            type=match.arg(type), select="arbitrary",
                            ...))
    }
)

setMethod("overlapsAny", c("Vector", "missing"),
    function(query, subject, maxgap=0L, minoverlap=1L,
             type=c("any", "start", "end", "within", "equal"),
             ...)
    {
        !is.na(findOverlaps(query,
                            maxgap=maxgap, minoverlap=minoverlap,
                            type=match.arg(type), select="arbitrary",
                            ...))
    }
)

setMethod("overlapsAny", c("integer", "Ranges"),
          function(query, subject, maxgap=0L, minoverlap=1L,
                   type=c("any", "start", "end", "within", "equal"),
                   ...)
    {
        !is.na(findOverlaps(query, subject,
                            maxgap=maxgap, minoverlap=minoverlap,
                            type=match.arg(type), select="arbitrary",
                            ...))
    }
)

setMethod("overlapsAny", c("Views", "Views"),
    function(query, subject, maxgap=0L, minoverlap=1L,
             type=c("any", "start", "end", "within", "equal"),
             ...)
    {
        overlapsAny(ranges(query), ranges(subject),
                    maxgap=maxgap, minoverlap=minoverlap,
                    type=match.arg(type),
                    ...)
    }
)

setMethod("overlapsAny", c("Views", "Vector"),
    function(query, subject, maxgap=0L, minoverlap=1L,
             type=c("any", "start", "end", "within", "equal"),
             ...)
    {
        overlapsAny(ranges(query), subject,
                    maxgap=maxgap, minoverlap=minoverlap,
                    type=match.arg(type),
                    ...)
    }
)

setMethod("overlapsAny", c("Vector", "Views"),
    function(query, subject, maxgap=0L, minoverlap=1L,
             type=c("any", "start", "end", "within", "equal"),
             ...)
    {
        overlapsAny(query, ranges(subject),
                    maxgap=maxgap, minoverlap=minoverlap,
                    type=match.arg(type),
                    ...)
    }
)

setMethod("overlapsAny", c("RangesList", "RangesList"),
    function(query, subject, maxgap=0L, minoverlap=1L,
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
        subject[sapply(subject, is.null)] <- IRanges()
        LogicalList(lapply(structure(seq_len(length(query)),
                                     names = names(query)),
                           function(i)
                             overlapsAny(query[[i]], subject[[i]],
                                         maxgap=maxgap, minoverlap=minoverlap,
                                         type=type,
                                         ...)))
    }
)

setMethod("overlapsAny", c("ViewsList", "ViewsList"),
    function(query, subject, maxgap=0L, minoverlap=1L,
             type=c("any", "start", "end", "within", "equal"),
             ...)
    {
        overlapsAny(ranges(query), ranges(subject),
                    maxgap=maxgap, minoverlap=minoverlap,
                    type=match.arg(type),
                    ...)
    }
)

setMethod("overlapsAny", c("ViewsList", "Vector"),
    function(query, subject, maxgap=0L, minoverlap=1L,
             type=c("any", "start", "end", "within", "equal"),
             ...)
    {
        overlapsAny(ranges(query), subject,
                    maxgap=maxgap, minoverlap=minoverlap,
                    type=match.arg(type),
                    ...)
    }
)

setMethod("overlapsAny", c("Vector", "ViewsList"),
    function(query, subject, maxgap=0L, minoverlap=1L,
             type=c("any", "start", "end", "within", "equal"),
             ...)
    {
        overlapsAny(query, ranges(subject),
                    maxgap=maxgap, minoverlap=minoverlap,
                    type=match.arg(type),
                    ...)
    }
)

setMethod("overlapsAny", c("RangedData", "RangedData"),
    function(query, subject, maxgap=0L, minoverlap=1L,
             type=c("any", "start", "end", "within", "equal"),
             ...)
    {
        overlapsAny(ranges(query), ranges(subject),
                    maxgap=maxgap, minoverlap=minoverlap,
                    type=match.arg(type),
                    ...)
    }
)

setMethod("overlapsAny", c("RangedData", "RangesList"),
    function(query, subject, maxgap=0L, minoverlap=1L,
             type=c("any", "start", "end", "within", "equal"),
             ...)
    {
        overlapsAny(ranges(query), subject,
                    maxgap=maxgap, minoverlap=minoverlap,
                    type=match.arg(type),
                    ...)
    }
)

setMethod("overlapsAny", c("RangesList", "RangedData"),
    function(query, subject, maxgap=0L, minoverlap=1L,
             type=c("any", "start", "end", "within", "equal"),
             ...)
    {
        overlapsAny(query, ranges(subject),
                    maxgap=maxgap, minoverlap=minoverlap,
                    type=match.arg(type),
                    ...)
    }
)

### Convenience wrappers for the 3 most common use cases.
`%over%` <- function(query, subject) overlapsAny(query, subject)
`%within%` <- function(query, subject) overlapsAny(query, subject,
                                                   type="within")
`%outside%` <- function(query, subject) !overlapsAny(query, subject)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### poverlaps()
###

setGeneric("poverlaps", signature=c("query", "subject"),
           function(query, subject, maxgap=0L, minoverlap=1L,
                    type=c("any", "start", "end", "within", "equal"))
               standardGeneric("poverlaps")
           )

setMethod("poverlaps", c("Ranges", "Ranges"),
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

setMethod("poverlaps", c("integer", "Ranges"),
          function(query, subject, maxgap=0L, minoverlap=1L,
                   type=c("any", "start", "end", "within", "equal"))
          {
              poverlaps(IRanges(query, width=1L), subject,
                        maxgap=maxgap, minoverlap=minoverlap,
                        type=match.arg(type))
          })

setMethod("poverlaps", c("Ranges", "integer"),
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
### subsetByOverlaps()
###

setGeneric("subsetByOverlaps", signature=c("query", "subject"),
    function(query, subject, maxgap=0L, minoverlap=1L,
             type=c("any", "start", "end", "within", "equal"),
             ...)
        standardGeneric("subsetByOverlaps")
)

setMethod("subsetByOverlaps", c("Vector", "Vector"),
    function(query, subject, maxgap=0L, minoverlap=1L,
             type=c("any", "start", "end", "within", "equal"),
             ...)
    {
        query[overlapsAny(query, subject,
                          maxgap=maxgap, minoverlap=minoverlap,
                          type=match.arg(type),
                          ...)]
    }
)

setMethod("subsetByOverlaps", c("RangedData", "RangedData"),
          function(query, subject, maxgap = 0L, minoverlap = 1L,
                   type = c("any", "start", "end", "within", "equal"))
          {
              query[unlist(!is.na(findOverlaps(ranges(query), ranges(subject),
                                               maxgap = maxgap,
                                               minoverlap = minoverlap,
                                               type = match.arg(type),
                                               select = "arbitrary")),
                           use.names=FALSE),]
          })

setMethod("subsetByOverlaps", c("RangedData", "RangesList"),
          function(query, subject, maxgap = 0L, minoverlap = 1L,
                   type = c("any", "start", "end", "within", "equal"))
          {
              query[unlist(!is.na(findOverlaps(ranges(query), subject,
                                               maxgap = maxgap,
                                               minoverlap = minoverlap,
                                               type = match.arg(type),
                                               select = "arbitrary")),
                           use.names=FALSE),]
          })

setMethod("subsetByOverlaps", c("RangesList", "RangedData"),
          function(query, subject, maxgap = 0L, minoverlap = 1L,
                   type = c("any", "start", "end", "within", "equal"))
          {
              query[!is.na(findOverlaps(query, ranges(subject),
                                        maxgap = maxgap,
                                        minoverlap = minoverlap,
                                        type = match.arg(type),
                                        select = "arbitrary"))]
          })


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
### "ranges" methods for Hits and HitsList objects
###

### Extracts the actual regions of intersection between the overlapping ranges.
### Not much value. Could be replaced by 1-liner:
###   pintersect(query[queryHits(x)], subject[subjectHits(x)])
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

