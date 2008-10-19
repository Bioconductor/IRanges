### =========================================================================
### IntervalTree utilities
### -------------------------------------------------------------------------

setMethod("overlap", c("IntervalTree", "Ranges"),
          function(object, query, maxgap = 0, multiple = TRUE)
          {
            query <- as(query, "IRanges")
            if (is.unsorted(start(query))) { ## query must be sorted
              stop("query must be in sorted order by start position")
            }
            if (!isTRUEorFALSE(multiple))
              stop("'multiple' must be logical of length 1")
            if (maxgap != 0) {
              if (!isSingleNumber(maxgap) || maxgap < 0)
                stop("'maxgap' must be a single, non-negative, non-NA number")
              start(query) <- start(query) - maxgap # shifts to left
              width(query) <- width(query) + 2*maxgap # adds to end (weird...)
            }
            fun <- "overlap"
            if (multiple)
              fun <- paste(fun, "_multiple", sep = "")
            validObject(query)
            result <- .IntervalTreeCall(object, fun, query)
            validObject(result)
            result
          })

setMethod("overlap", c("Ranges", "Ranges"),
          function(object, query, maxgap = 0, multiple = TRUE) {
            overlap(IntervalTree(object), query, maxgap, multiple)
          })

## not for exporting, just a debugging utility
IntervalTreeDump <- function(object) {
  .IntervalTreeCall(object, "dump")
}
