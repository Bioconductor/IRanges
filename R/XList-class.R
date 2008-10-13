### =========================================================================
### XList objects
### -------------------------------------------------------------------------

## A list that holds XSequence objects

setClass("XList", prototype = prototype(elementClass = "XSequence"),
         contains = "TypedList")
