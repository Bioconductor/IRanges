### =========================================================================
### IntervalTree utilities
### -------------------------------------------------------------------------

setMethod("overlap", c("IntervalTree", "Ranges"),
          function(object, query, maxgap = 0, multiple = TRUE)
          {
            query <- as(query, "IRanges")
            query_ord <- NULL
            if (is.unsorted(start(query))) { ## query must be sorted
              query_ord <- order(query)
              query <- query[query_ord]
            }
            if (!isTRUEorFALSE(multiple))
              stop("'multiple' must be logical of length 1")
            if (maxgap != 0) {
              if (!isSingleNumber(maxgap) || maxgap < 0)
                stop("'maxgap' must be a single, non-negative, non-NA number")
              ## Another option would be to do
              ##   query <- unsafe.update(start = start(query) - maxgap,
              ##                          end = end(query) + maxgap)
              ## instead of the 2 lines below. More readable? (but maybe not as
              ## efficient)
              query <- shift(query, -maxgap)
              width(query) <- width(query) + 2*maxgap # adds to end (weird...)
            }
            fun <- "overlap"
            if (multiple)
              fun <- paste(fun, "_multiple", sep = "")
            validObject(query)
            result <- .IntervalTreeCall(object, fun, query)
            if (!is.null(query_ord)) {
              if (multiple) {
                mat <- matchMatrix(result)
                mat[,1] <- query_ord[mat[,1]]
                result@matchMatrix <- mat
              } else {
                query_rev_ord <- integer(length(query_ord))
                query_rev_ord[query_ord] <- seq_along(query_ord)
                result <- result[query_rev_ord]
              }
            }
            result
          })

setMethod("overlap", c("Ranges", "Ranges"),
          function(object, query, maxgap = 0, multiple = TRUE) {
            overlap(IntervalTree(object), query, maxgap, multiple)
          })

setMethod("overlap", c("Ranges", "missing"),
    function(object, query, maxgap = 0, multiple = TRUE)
        overlap(object, object, maxgap, multiple)
)

setMethod("overlap", c("Ranges", "integer"),
          function(object, query, maxgap = 0, multiple = TRUE)
          overlap(object, IRanges(query, query), maxgap, multiple)
          )

## not for exporting, just a debugging utility
IntervalTreeDump <- function(object) {
  .IntervalTreeCall(object, "dump")
}
