### =========================================================================
### RangesMatching objects
### -------------------------------------------------------------------------

setClass("RangesMatching", representation(matchMatrix = "Matrix"))


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Accessors
###

setGeneric("matchMatrix", function(object, ...) standardGeneric("matchMatrix"))
setMethod("matchMatrix", "RangesMatching", function(object) object@matchMatrix)

### TODO: many convenience methods
