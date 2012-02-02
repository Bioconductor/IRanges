### =========================================================================
### IntervalTree utilities
### -------------------------------------------------------------------------

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

## internal generic
setGeneric("processSelfMatching",
           function(x, select = c("all", "first", "last", "arbitrary"),
                    ignoreSelf = FALSE, ignoreRedundant = FALSE)
           standardGeneric("processSelfMatching"))

setMethod("processSelfMatching", "Hits",
          function(x, select = c("all", "first", "last", "arbitrary"),
                   ignoreSelf = FALSE, ignoreRedundant = FALSE)
          {
            mat <- as.matrix(x)
            if (ignoreSelf)
              mat <- mat[mat[,1L] != mat[,2L],,drop=FALSE]
            if (ignoreRedundant) {
              norm_mat <- cbind(pmin.int(mat[,1L], mat[,2L]),
                                pmax.int(mat[,1L], mat[,2L]))
              mat <- mat[!duplicated(norm_mat),,drop=FALSE]
            }
            if (select != "all") { # relies on 'mat' sorted by subject
              if (select == "last")
                mat <- mat[seq(nrow(mat), 1),,drop=FALSE]
              .hitsMatrixToVector(mat, queryLength(x))
            } else {
              ## unname() required because in case 'm' has only 1 row
              ## 'm[ , 1L]' and 'm[ , 2L]' will return a named atomic vector
              x@queryHits <- unname(mat[ , 1L])
              x@subjectHits <- unname(mat[ , 2L])
              x
            }
          })

setMethod("processSelfMatching", "HitsList",
          function(x, select = c("all", "first", "last", "arbitrary"),
                   ignoreSelf = FALSE, ignoreRedundant = FALSE)
          {
            select <- match.arg(select)
            ans <- lapply(x, processSelfMatching, select, ignoreSelf,
                          ignoreRedundant)
            if (select != "all")
              IntegerList(ans)
            else
              newSimpleList("HitsList", ans,
                            subjectOffsets = x@subjectOffsets)
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

## not for exporting, just a debugging utility
IntervalTreeDump <- function(object) {
  .IntervalTreeCall(object, "dump")
}
