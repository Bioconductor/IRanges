### =========================================================================
### AtomicList objects
### -------------------------------------------------------------------------

## A list that holds atomic objects

setClass("LogicalList", representation("VIRTUAL"),
         prototype = prototype(elementType = "logical"),
         contains = "Sequence")

setClass("IntegerList", representation("VIRTUAL"),
         prototype = prototype(elementType = "integer"),
         contains = "Sequence")

setClass("NumericList", representation("VIRTUAL"),
         prototype = prototype(elementType = "numeric"),
         contains = "Sequence")

setClass("ComplexList", representation("VIRTUAL"),
         prototype = prototype(elementType = "complex"),
         contains = "Sequence")

setClass("CharacterList", representation("VIRTUAL"),
         prototype = prototype(elementType = "character"),
         contains = "Sequence")

setClass("RawList", representation("VIRTUAL"),
         prototype = prototype(elementType = "raw"),
         contains = "Sequence")

setClass("RleList", representation("VIRTUAL"),
         prototype = prototype(elementType = "Rle"),
         contains = "Sequence")
