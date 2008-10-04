### =========================================================================
### IntervalTree utilities
### -------------------------------------------------------------------------

setMethod("overlap", c("IntervalTree", "IRanges"),
          function(object, query, multiple = TRUE)
          {
            validObject(query)
            if (!isTRUEorFALSE(multiple))
              stop("'multiple' must be logical of length 1")
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

setMethod("overlap", c("IRanges", "missing"),
          function(object, query, multiple = TRUE) {
            overlap(object, object, multiple)
          })

## not for exporting, just a debugging utility
IntervalTreeDump <- function(object) {
  .IntervalTreeCall(object, "dump")
}
