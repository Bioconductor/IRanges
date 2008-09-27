### =========================================================================
### IntervalTree objects
### -------------------------------------------------------------------------

setClass("IntervalTree",
         representation(ptr = "externalptr", mode = "character"),
         contains = "XRanges")

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
