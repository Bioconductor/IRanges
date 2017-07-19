### =========================================================================
### Generic functions for squeezing the ranges out of a range-based object
### -------------------------------------------------------------------------


### Extract the ranges as an IRanges object.
setGeneric("ranges", signature="x",
    function(x, use.names=TRUE, use.mcols=FALSE, ...)
        standardGeneric("ranges")
)

### Extract the ranges as an IRangesList object.
setGeneric("rglist", signature="x",
    function(x, use.names=TRUE, use.mcols=FALSE, ...)
        standardGeneric("rglist")
)

### Pairs method.
setMethod("rglist", "Pairs", function(x, use.names=TRUE, use.mcols=FALSE) {
              stopifnot(isTRUEorFALSE(use.mcols))
              rl <- zipup(ranges(first(x)), ranges(second(x)))
              if (!use.mcols) {
                  mcols(rl) <- NULL
              }
              rl
          })

