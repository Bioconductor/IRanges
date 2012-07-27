### =========================================================================
### findOverlaps (and related) methods
### -------------------------------------------------------------------------
###


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### findOverlaps()
###
### Find objects in the query that overlap those in the subject.
###

setGeneric("findOverlaps", signature = c("query", "subject"),
    function(query, subject, maxgap = 0L, minoverlap = 1L,
             type = c("any", "start", "end", "within", "equal"),
             select = c("all", "first", "last", "arbitrary"), ...)
        standardGeneric("findOverlaps")
)

setMethod("findOverlaps", c("Ranges", "IntervalTree"),
          function(query, subject, maxgap = 0L, minoverlap = 1L,
                   type = c("any", "start", "end", "within", "equal"),
                   select = c("all", "first", "last", "arbitrary"))
          {
            if (!isSingleNumber(maxgap) || maxgap < 0L)
              stop("'maxgap' must be a single, non-negative integer")
            if (!isSingleNumber(minoverlap) || minoverlap < 1L)
              stop("'minoverlap' must be a single, positive integer")
            type <- match.arg(type)
            select <- match.arg(select)
            origSelect <- select
            if (type != "any" || minoverlap > 1L)
              select <- "all"
            query <- as(query, "IRanges")
            query_ord <- NULL
            origQuery <- query
            adjust <- maxgap - minoverlap + 1L
            if (adjust > 0L)
              query <-
                resize(query, width(query) + 2L * adjust, fix = "center")
            unsortedQuery <- query
            if (isNotSorted(start(query))) { ## query must be sorted
              query_ord <- sort.list(start(query), method = "quick",
                                     na.last = NA)
              query <- query[query_ord]
            } else {
              query_ord <- seq_len(length(query))
            }
            validObject(query)
            fun <- paste("overlap_", select, sep = "")
            result <- .IntervalTreeCall(subject, fun, query, query_ord)
            if (type != "any" || minoverlap > 1L) {
              m <- as.matrix(result)
              if (minoverlap > 1L) {
                r <- ranges(result, unsortedQuery, subject)
                m <- m[width(r) >= minoverlap, , drop=FALSE]
                ## unname() required because in case 'm' has only 1 row
                ## 'm[ , 1L]' and 'm[ , 2L]' will return a named atomic vector
                result@queryHits <- unname(m[ , 1L])
                result@subjectHits <- unname(m[ , 2L])
              }
              query <- origQuery
              filterMatrix <- function(fun)
                m[abs(fun(query)[m[,1L]] - fun(subject)[m[,2L]]) <= maxgap, ,
                  drop=FALSE]
              if (type == "within") {
                r <- ranges(result, query, subject)
                m <- m[width(query)[m[,1L]] - width(r) <= maxgap, , drop=FALSE]
              } else if (type == "start") {
                m <- filterMatrix(start)
              } else if (type == "end") {
                m <- filterMatrix(end)
              } else if (type == "equal") {
                m <- filterMatrix(start)
                m <- filterMatrix(end)
              }
              if (origSelect != "all") {
                m <- m[!duplicated(m[,1L]), , drop=FALSE]
                result <- rep.int(NA_integer_, length(query))
                ## unname() required because in case 'm' has only 1 row
                ## 'm[,2L]' will return a named atomic vector
                result[m[,1L]] <- unname(m[,2L])
              } else {
                ## unname() required because in case 'm' has only 1 row
                ## 'm[ , 1L]' and 'm[ , 2L]' will return a named atomic vector
                result@queryHits <- unname(m[ , 1L])
                result@subjectHits <- unname(m[ , 2L])
              }
            }
            result
          })

setMethod("findOverlaps", c("Ranges", "Ranges"),
          function(query, subject, maxgap = 0L, minoverlap = 1L,
                   type = c("any", "start", "end", "within", "equal"),
                   select = c("all", "first", "last", "arbitrary"))
          {
            findOverlaps(query, IntervalTree(subject), maxgap = maxgap,
                         minoverlap = minoverlap, type = match.arg(type),
                         select = match.arg(select))
          })

setMethod("findOverlaps", c("ANY", "missing"),
          function(query, subject, maxgap = 0L, minoverlap = 1L,
                   type = c("any", "start", "end", "within", "equal"),
                   select = c("all", "first", "last", "arbitrary"),
                   ignoreSelf = FALSE, ignoreRedundant = FALSE)
          {
            select <- match.arg(select)
            result <- findOverlaps(query, query,
                                   maxgap = maxgap, minoverlap = minoverlap,
                                   type = type, select = "all")
            processSelfMatching(result, select, ignoreSelf, ignoreRedundant)
          })

setMethod("findOverlaps", c("integer", "Ranges"),
          function(query, subject, maxgap = 0L, minoverlap = 1L,
                   type = c("any", "start", "end", "within", "equal"),
                   select = c("all", "first", "last", "arbitrary"))
          {
            findOverlaps(IRanges(query, query), subject, maxgap = maxgap,
                         minoverlap = minoverlap, type = match.arg(type),
                         select = match.arg(select))
          })

setMethod("findOverlaps", c("Views", "Views"),
    function(query, subject, maxgap=0L, minoverlap=1L,
             type=c("any", "start", "end", "within", "equal"),
             select=c("all", "first", "last", "arbitrary"))
    {
        findOverlaps(ranges(query), ranges(subject),
                     maxgap=maxgap, minoverlap=minoverlap,
                     type=type, select=select)
    }
)

setMethod("findOverlaps", c("ANY", "Views"),
    function(query, subject, maxgap=0L, minoverlap=1L,
             type=c("any", "start", "end", "within", "equal"),
             select=c("all", "first", "last", "arbitrary"))
    {
        findOverlaps(query, ranges(subject),
                     maxgap=maxgap, minoverlap=minoverlap,
                     type=type, select=select)
    }
)

setMethod("findOverlaps", c("Views", "ANY"),
    function(query, subject, maxgap=0L, minoverlap=1L,
             type=c("any", "start", "end", "within", "equal"),
             select=c("all", "first", "last", "arbitrary"))
    {
        findOverlaps(ranges(query), subject,
                     maxgap=maxgap, minoverlap=minoverlap,
                     type=type, select=select)
    }
)

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
              findOverlaps(query[[i]], subject[[i]], maxgap = maxgap,
                           minoverlap = minoverlap, type = type,
                           select = select)
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
                     type=type, select=select, drop=drop)
    }
)

setMethod("findOverlaps", c("ANY", "ViewsList"),
    function(query, subject, maxgap=0L, minoverlap=1L,
             type=c("any", "start", "end", "within", "equal"),
             select=c("all", "first", "last", "arbitrary"),
             drop=FALSE)
    {
        findOverlaps(query, ranges(subject),
                     maxgap=maxgap, minoverlap=minoverlap,
                     type=type, select=select, drop=drop)
    }
)

setMethod("findOverlaps", c("ViewsList", "ANY"),
    function(query, subject, maxgap=0L, minoverlap=1L,
             type=c("any", "start", "end", "within", "equal"),
             select=c("all", "first", "last", "arbitrary"),
             drop=FALSE)
    {
        findOverlaps(ranges(query), subject,
                     maxgap=maxgap, minoverlap=minoverlap,
                     type=type, select=select, drop=drop)
    }
)

setMethod("findOverlaps", c("RangedData", "RangedData"),
          function(query, subject, maxgap = 0L, minoverlap = 1L,
                   type = c("any", "start", "end", "within", "equal"),
                   select = c("all", "first", "last", "arbitrary"),
                   drop = FALSE)
          {
            findOverlaps(ranges(query), ranges(subject), maxgap = maxgap,
                         minoverlap = minoverlap, type = match.arg(type),
                         select = match.arg(select), drop = drop)
          })

setMethod("findOverlaps", c("RangedData", "RangesList"),
          function(query, subject, maxgap = 0L, minoverlap = 1L,
                   type = c("any", "start", "end", "within", "equal"),
                   select = c("all", "first", "last", "arbitrary"),
                   drop = FALSE)
          {
            findOverlaps(ranges(query), subject, maxgap = maxgap,
                         minoverlap = minoverlap, type = match.arg(type),
                         select = match.arg(select), drop = drop)
          })

setMethod("findOverlaps", c("RangesList", "RangedData"),
          function(query, subject, maxgap = 0L, minoverlap = 1L,
                   type = c("any", "start", "end", "within", "equal"),
                   select = c("all", "first", "last", "arbitrary"),
                   drop = FALSE)
          {
            findOverlaps(query, ranges(subject), maxgap = maxgap,
                         minoverlap = minoverlap, type = match.arg(type),
                         select = match.arg(select), drop = drop)
          })


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### countOverlaps()
###

setGeneric("countOverlaps",
    function(query, subject, maxgap = 0L, minoverlap = 1L,
             type = c("any", "start", "end", "within", "equal"), ...)
        standardGeneric("countOverlaps")
)

setMethod("countOverlaps", c("Ranges", "Ranges"),
    function(query, subject, maxgap = 0L, minoverlap = 1L,
             type = c("any", "start", "end", "within", "equal"))
    {
        counts <- queryHits(findOverlaps(query, subject, maxgap = maxgap,
                                         minoverlap = minoverlap, type = type))
        structure(tabulate(counts, length(query)), names=names(query))
    }
)

setMethod("countOverlaps", c("Views", "Views"),
    function(query, subject, maxgap=0L, minoverlap=1L,
             type=c("any", "start", "end", "within", "equal"))
    {
         countOverlaps(ranges(query), ranges(subject),
                       maxgap=maxgap, minoverlap=minoverlap,
                       type=type)
    }
)

setMethod("countOverlaps", c("ANY", "Views"),
    function(query, subject, maxgap=0L, minoverlap=1L,
             type=c("any", "start", "end", "within", "equal"))
    {
         countOverlaps(query, ranges(subject),
                       maxgap=maxgap, minoverlap=minoverlap,
                       type=type)
    }
)

setMethod("countOverlaps", c("Views", "ANY"),
    function(query, subject, maxgap=0L, minoverlap=1L,
             type=c("any", "start", "end", "within", "equal"))
    {
         countOverlaps(ranges(query), subject,
                       maxgap=maxgap, minoverlap=minoverlap,
                       type=type)
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

setMethod("countOverlaps", c("ANY", "ViewsList"),
    function(query, subject, maxgap=0L, minoverlap=1L,
             type=c("any", "start", "end", "within", "equal"))
    {
         countOverlaps(query, ranges(subject),
                       maxgap=maxgap, minoverlap=minoverlap,
                       type=type)
    }
)

setMethod("countOverlaps", c("ViewsList", "ANY"),
    function(query, subject, maxgap=0L, minoverlap=1L,
             type=c("any", "start", "end", "within", "equal"))
    {
         countOverlaps(ranges(query), subject,
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
### subsetByOverlaps()
###

setGeneric("subsetByOverlaps",
    function(query, subject, maxgap = 0L, minoverlap = 1L,
             type = c("any", "start", "end", "within", "equal"), ...)
        standardGeneric("subsetByOverlaps")
)

setMethod("subsetByOverlaps", c("ANY", "ANY"),
    function(query, subject, maxgap = 0L, minoverlap = 1L,
             type = c("any", "start", "end", "within", "equal"))
    {
        type <- match.arg(type)
        query[!is.na(findOverlaps(query, subject, maxgap = maxgap,
                                  minoverlap = minoverlap, type = type,
                                  select = "arbitrary"))]
    }
)

setMethod("subsetByOverlaps", c("RangesList", "RangesList"),
          function(query, subject, maxgap = 0L, minoverlap = 1L,
                   type = c("any", "start", "end", "within", "equal"))
          {
              type <- match.arg(type)
              query[!is.na(findOverlaps(query, subject, maxgap = maxgap,
                                        minoverlap = minoverlap, type = type,
                                        select = "arbitrary"))]
          })

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
### match()
###

setGeneric("match",
    function(x, table, nomatch = NA_integer_, incomparables = NULL)
        standardGeneric("match")
)

setMethod("match", c("Ranges", "Ranges"),
    function(x, table, nomatch = NA_integer_, incomparables = NULL)
    {
        if (length(nomatch) != 1)
            stop("'nomatch' must be of length 1") 
        ans <- findOverlaps(x, table, select = "first")
        if (!is.na(nomatch) && anyMissing(ans))
            ans[is.na(ans)] <- nomatch
        ans
    }
)

setMethod("match", c("Views", "Views"),
    function(x, table, nomatch=NA_integer_, incomparables=NULL)
    {
        match(ranges(x), ranges(table),
              nomatch=nomatch, incomparables=incomparables)
    }
)

setMethod("match", c("ANY", "Views"),
    function(x, table, nomatch=NA_integer_, incomparables=NULL)
    {
        match(x, ranges(table),
              nomatch=nomatch, incomparables=incomparables)
    }
)

setMethod("match", c("Views", "ANY"),
    function(x, table, nomatch=NA_integer_, incomparables=NULL)
    {
        match(ranges(x), table,
              nomatch=nomatch, incomparables=incomparables)
    }
)

setMethod("match", c("RangesList", "RangesList"),
          function(x, table, nomatch = NA_integer_, incomparables = NULL)
          {
            if (length(nomatch) != 1)
              stop("'nomatch' must be of length 1")
            ans <- findOverlaps(x, table, select = "first", drop=TRUE)
            if (!is.na(nomatch) && anyMissing(ans))
              ans[is.na(ans)] <- as.integer(nomatch)
            ans
          })

setMethod("match", c("ViewsList", "ViewsList"),
    function(x, table, nomatch=NA_integer_, incomparables=NULL)
    {
        match(ranges(x), ranges(table),
              nomatch=nomatch, incomparables=incomparables)
    }
)

setMethod("match", c("ANY", "ViewsList"),
    function(x, table, nomatch=NA_integer_, incomparables=NULL)
    {
        match(x, ranges(table),
              nomatch=nomatch, incomparables=incomparables)
    }
)

setMethod("match", c("ViewsList", "ANY"),
    function(x, table, nomatch=NA_integer_, incomparables=NULL)
    {
        match(ranges(x), table,
              nomatch=nomatch, incomparables=incomparables)
    }
)

setMethod("match", c("RangedData", "RangedData"),
          function(x, table, nomatch = NA_integer_, incomparables = NULL)
          {
              match(ranges(x), ranges(table), nomatch = nomatch,
                    incomparables = incomparables)
          })

setMethod("match", c("RangedData", "RangesList"),
          function(x, table, nomatch = NA_integer_, incomparables = NULL)
          {
              match(ranges(x), table, nomatch = nomatch,
                    incomparables = incomparables)
          })

setMethod("match", c("RangesList", "RangedData"),
          function(x, table, nomatch = NA_integer_, incomparables = NULL)
          {
              match(x, ranges(table), nomatch = nomatch,
                    incomparables = incomparables)
          })


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### %in%
###

setMethod("%in%", c("Ranges", "Ranges"),
    function(x, table)
    {
        if (!is(x, "IRanges"))
            x <- as(x, "IRanges")
        subject <- IntervalTree(table)
        if (isNotSorted(start(x))) { ## x must be sorted
            x_ord <- order(x)
            x <- x[x_ord]
        } else {
            x_ord <- seq_len(length(x))
        }
        IRanges:::.IntervalTreeCall(subject, "overlap_any", x, x_ord)
    }
)

setMethod("%in%", c("Views", "Views"),
    function(x, table) ranges(x) %in% ranges(table)
)

setMethod("%in%", c("ANY", "Views"),
    function(x, table) x %in% ranges(table)
)

setMethod("%in%", c("Views", "ANY"),
    function(x, table) ranges(x) %in% table
)

setMethod("%in%", c("RangesList", "RangesList"),
          function(x, table)
          {
            x <- as.list(x)
            table <- as.list(table)
            if (!is.null(names(x)) && !is.null(names(table))) {
              table <- table[names(x)]
              names(table) <- names(x) # get rid of NA's in names
            } else {
              table <- table[seq_along(x)]
            }
            ## NULL's are introduced where they do not match
            ## We replace those with empty IRanges
            table[sapply(table, is.null)] <- IRanges()
            LogicalList(lapply(structure(seq_len(length(x)), names = names(x)),
                               function(i) x[[i]] %in% table[[i]]))
          })

setMethod("%in%", c("ViewsList", "ViewsList"),
    function(x, table) ranges(x) %in% ranges(table)
)

setMethod("%in%", c("ANY", "ViewsList"),
    function(x, table) x %in% ranges(table)
)

setMethod("%in%", c("ViewsList", "ANY"),
    function(x, table) ranges(x) %in% table
)

setMethod("%in%", c("RangedData", "RangedData"),
          function(x, table) ranges(x) %in% ranges(table))

setMethod("%in%", c("RangedData", "RangesList"),
          function(x, table) ranges(x) %in% table)

setMethod("%in%", c("RangesList", "RangedData"),
          function(x, table) x %in% ranges(table))

