### =========================================================================
### IntervalTree utilities
### -------------------------------------------------------------------------

## currently, just a 'match' thing
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
              .Call(fun, object@ptr, ranges)
            } else stop("unknown interval tree mode: ", object@mode)
          })

## not for exporting, just a debugging utility
IntegerIntervalTreeDump <- function(tree) {
  .Call("IntegerIntervalTree_dump", tree)
}
