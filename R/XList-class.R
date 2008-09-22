### =========================================================================
### XList objects
### -------------------------------------------------------------------------

## A list that holds XSequence objects

setClass("XList", contains = "TypedList")

setMethod("elementClass", "XList", function(x) "XSequence")
