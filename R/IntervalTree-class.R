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

IntervalTree <- function(ranges) {
  as(ranges, "IntervalTree")
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion
###

setAs("IntervalTree", "IRanges", function(from) {
  .IntervalTreeCall(from, "asIRanges")
})

setAs("IRanges", "IntervalTree", function(from) {
  validObject(from)
  ptr <- .Call("IntegerIntervalTree_new", from, PACKAGE="IRanges")
  new("IntervalTree", ptr = ptr, mode = "integer")
})

setAs("Ranges", "IntervalTree", function(from) {
  as(as(from, "IRanges"), "IntervalTree")
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
