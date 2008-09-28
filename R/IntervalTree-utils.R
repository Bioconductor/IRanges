### =========================================================================
### IntervalTree utilities
### -------------------------------------------------------------------------

setMethod("overlap", c("IntervalTree", "IRanges"),
          function(object, query, multiple = TRUE)
          {
            validObject(object)
            validObject(query)
            fun <- "IntervalTree_overlap"
            if (multiple)
              fun <- paste(fun, "_multiple", sep = "")
            if (object@mode == "integer") {
              fun <- paste("Integer", fun, sep = "")
              .Call(fun, object@ptr, query)
            } else stop("unknown interval tree mode: ", object@mode)
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
IntegerIntervalTreeDump <- function(tree) {
  .Call("IntegerIntervalTree_dump", tree@ptr)
}
