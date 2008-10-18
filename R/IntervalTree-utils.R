### =========================================================================
### IntervalTree utilities
### -------------------------------------------------------------------------

setMethod("overlap", c("IntervalTree", "IRanges"),
          function(object, query, by = 1, multiple = TRUE)
          {
            validObject(query)
            if (!isTRUEorFALSE(multiple))
              stop("'multiple' must be logical of length 1")
            if (!isSingleNumber(by))
              stop("'by' must be a single number")
            if (!missing(by)) {
              width(query) <- width(query) - (by - 1) * 2
            }
            fun <- "overlap"
            if (multiple)
              fun <- paste(fun, "_multiple", sep = "")
            result <- .IntervalTreeCall(object, fun, query)
            validObject(result)
            result
          })

setMethod("overlap", c("IRanges", "IRanges"),
          function(object, query, multiple = TRUE) {
            overlap(IntervalTree(object), query, multiple)
          })

## not for exporting, just a debugging utility
IntervalTreeDump <- function(object) {
  .IntervalTreeCall(object, "dump")
}
