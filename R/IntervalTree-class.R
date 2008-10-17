### =========================================================================
### IntervalTree objects
### -------------------------------------------------------------------------

setClass("IntervalTree",
         representation(ptr = "externalptr", mode = "character"),
         contains = "XRanges")

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Accessors
###

setMethod("length", "IntervalTree", function(x) .IntervalTreeCall(x, "length"))

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Constructor
###

setGeneric("IntervalTree",
           function(object, ...) standardGeneric("IntervalTree"))

setMethod("IntervalTree", "IRanges", function(object) {
  validObject(object)
  ptr <- .Call("IntegerIntervalTree_new", object, PACKAGE="IRanges")
  new("IntervalTree", ptr = ptr, mode = "integer")
})

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion
###

setAs("IntervalTree", "IRanges", function(from) {
  .IntervalTreeCall(from, "asIRanges")
})

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Low-level utilities
###

.IntervalTreeCall <- function(object, fun, ...) {
  validObject(object)
  fun <- paste("IntervalTree", fun, sep = "_")
  if (object@mode == "integer") {
    fun <- paste("Integer", fun, sep = "")
    .Call(fun, object@ptr, ..., PACKAGE="IRanges")
  } else stop("unknown interval tree mode: ", object@mode)
}
