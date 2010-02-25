### =========================================================================
### IntervalTree utilities
### -------------------------------------------------------------------------

setMethod("findOverlaps", c("Ranges", "IntervalTree"),
          function(query, subject, maxgap = 0, multiple = TRUE,
                   type = c("any", "start", "end", "within", "equal"))
          {
            if (!IRanges:::isSingleNumber(maxgap) || maxgap < 0)
              stop("'maxgap' must be a non-negative number")
            if (!IRanges:::isTRUEorFALSE(multiple))
              stop("'multiple' must be TRUE or FALSE")
            type <- match.arg(type)
            origMultiple <- multiple
            if (type != "any")
              multiple <- TRUE
            query <- as(query, "IRanges")
            query_ord <- NULL
            origQuery <- query
            if (isNotSorted(start(query))) { ## query must be sorted
              query_ord <- order(query)
              query <- query[query_ord]
            } else {
              query_ord <- seq_len(length(query))
            }
            if (maxgap != 0) {
              query <- shift(query, -maxgap)
              width(query) <- width(query) + 2*maxgap # adds to end (weird...)
            }
            validObject(query)
            if (multiple)
              fun <- "overlap_multiple"
            else
              fun <- "overlap_first"
            result <- IRanges:::.IntervalTreeCall(subject, fun, query, query_ord)
            if (multiple && type != "any") {
              query <- origQuery
              m <- as.matrix(result)
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
          function(query, subject, maxgap = 0, multiple = TRUE,
                   type = c("any", "start", "end", "within", "equal")) {
            findOverlaps(query, IntervalTree(subject), maxgap = maxgap,
                         multiple = multiple, type = type)
          })

setMethod("findOverlaps", c("ANY", "missing"),
          function(query, subject, maxgap = 0, multiple = TRUE,
                   type = c("any", "start", "end", "within", "equal"),
                   ignoreSelf = FALSE, ignoreRedundant = FALSE)
          {
            if (!multiple && !ignoreSelf) # silly case
              seq(length(query))
            else {
              result <- findOverlaps(query, query, maxgap = maxgap,
                                     multiple = multiple, type = type)
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
          function(query, subject, maxgap = 0, multiple = TRUE,
                   type = c("any", "start", "end", "within", "equal"))
          {
            findOverlaps(IRanges(query, query), subject, maxgap = maxgap,
                         multiple = multiple, type = type)
          })

## not for exporting, just a debugging utility
IntervalTreeDump <- function(object) {
  .IntervalTreeCall(object, "dump")
}
