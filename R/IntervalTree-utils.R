### =========================================================================
### IntervalTree utilities
### -------------------------------------------------------------------------

## currently, just a 'match' thing
setMethod("overlap", c("IntegerIntervalTree", "IRanges"),
          function(object, query)
          {
            validObject(object)
            validObject(query)
            .Call("IntegerIntervalTree_overlap", object, ranges)
          })

## not for exporting, just a debugging utility
IntegerIntervalTreeDump <- function(tree) {
  .Call("IntegerIntervalTree_dump", tree)
}

