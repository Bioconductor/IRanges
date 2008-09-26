### =========================================================================
### IntervalTree objects
### -------------------------------------------------------------------------

setClass("IntervalTree", contains = "SpatialIndex")

setClass("IntegerIntervalTree",
         representation(ptr = "externalptr"),
         contains = "IntervalTree")

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Constructor
###

setGeneric("IntervalTree",
           function(object, ...) standardGeneric("IntervalTree"))

setMethod("IntervalTree", "IRanges", function(object) {
  validObject(object)
  ptr <- .Call("IntegerIntervalTree_new", object, PACKAGE="IRanges")
  new("IntegerIntervalTree", ptr = ptr)
})
