### =========================================================================
### RangesMatching objects
### -------------------------------------------------------------------------

setClass("RangesMatching", representation(matchmatrix = "Matrix"))


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Accessors
###

setGeneric("matchmatrix", function(object, ...) standardGeneric("matchmatrix"))
setMethod("matchmatrix", "RangesMatching", function(object) object@matchmatrix)

### TODO: many convenience methods
