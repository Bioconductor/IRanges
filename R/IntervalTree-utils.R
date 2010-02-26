### =========================================================================
### IntervalTree utilities
### -------------------------------------------------------------------------

setMethod("findOverlaps", c("Ranges", "IntervalTree"),
          function(query, subject, maxgap = 0L, multiple = TRUE,
                   type = c("any", "start", "end", "within", "equal"),
                   minoverlap = 1L)
          {
            if (!isSingleNumber(maxgap) || maxgap < 0L)
              stop("'maxgap' must be a single, non-negative integer")
            if (!isTRUEorFALSE(multiple))
              stop("'multiple' must be TRUE or FALSE")
            type <- match.arg(type)
            if (!isSingleNumber(minoverlap) || minoverlap < 1L)
              stop("'minoverlap' must be a single, positive integer")
            origMultiple <- multiple
            if (type != "any" || minoverlap > 1L)
              multiple <- TRUE
            query <- as(query, "IRanges")
            query_ord <- NULL
            origQuery <- query
            adjust <- maxgap - minoverlap + 1L
            if (adjust > 0L)
              query <- resize(query, width(query) + 2*adjust, symmetric = TRUE)
            unsortedQuery <- query
            if (isNotSorted(start(query))) { ## query must be sorted
              query_ord <- sort.list(start(query), method = "quick",
                                     na.last = NA)
              query <- query[query_ord]
            } else {
              query_ord <- seq_len(length(query))
            }
            validObject(query)
            if (multiple)
              fun <- "overlap_multiple"
            else
              fun <- "overlap_first"
            result <- .IntervalTreeCall(subject, fun, query, query_ord)
            if (type != "any" || minoverlap > 1L) {
              m <- as.matrix(result)
              if (minoverlap > 1L) {
                r <- ranges(result, unsortedQuery, subject)
                m <- m[width(r) >= minoverlap, , drop=FALSE]
                result@matchMatrix <- m
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
              if (!origMultiple) {
                m <- m[!duplicated(m[,1L]), , drop=FALSE]
                result <- rep.int(NA_integer_, length(query))
                result[m[,1L]] <- m[,2L]
              } else {
                result@matchMatrix <- m
              }
            }
            result
          })

setMethod("findOverlaps", c("Ranges", "Ranges"),
          function(query, subject, maxgap = 0L, multiple = TRUE,
                   type = c("any", "start", "end", "within", "equal"),
                   minoverlap = 1L) {
            findOverlaps(query, IntervalTree(subject), maxgap = maxgap,
                         multiple = multiple, type = type,
                         minoverlap = minoverlap)
          })

setMethod("findOverlaps", c("ANY", "missing"),
          function(query, subject, maxgap = 0L, multiple = TRUE,
                   type = c("any", "start", "end", "within", "equal"),
                   ignoreSelf = FALSE, ignoreRedundant = FALSE, minoverlap = 1L)
          {
            if (!multiple && !ignoreSelf) # silly case
              seq(length(query))
            else {
              result <- findOverlaps(query, query, maxgap = maxgap,
                                     multiple = multiple, type = type,
                                     minoverlap = minoverlap)
              mat <- matchMatrix(result)
              if (ignoreSelf)
                mat <- mat[mat[,1L] != mat[,2L],]
              if (ignoreRedundant) {
                norm_mat <- cbind(pmin(mat[,1L], mat[,2L]),
                                  pmax(mat[,1L], mat[,2L]))
                mat <- mat[!duplicated(norm_mat),]
              }
              result@matchMatrix <- mat
              result
            }
          })

setMethod("findOverlaps", c("integer", "Ranges"),
          function(query, subject, maxgap = 0L, multiple = TRUE,
                   type = c("any", "start", "end", "within", "equal"),
                   minoverlap = 1L)
          {
            findOverlaps(IRanges(query, query), subject, maxgap = maxgap,
                         multiple = multiple, type = type,
                         minoverlap = minoverlap)
          })

## not for exporting, just a debugging utility
IntervalTreeDump <- function(object) {
  .IntervalTreeCall(object, "dump")
}
