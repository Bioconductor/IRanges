### =========================================================================
### AtomicList objects
### -------------------------------------------------------------------------

## A list that holds atomic objects

setClass("AtomicList", representation("VIRTUAL"),
        prototype = prototype(elementType = "logical"),
        contains = "List")

setClass("LogicalList", representation("VIRTUAL"),
         prototype = prototype(elementType = "logical"),
         contains = "AtomicList")

setClass("IntegerList", representation("VIRTUAL"),
         prototype = prototype(elementType = "integer"),
         contains = "AtomicList")

setClass("NumericList", representation("VIRTUAL"),
         prototype = prototype(elementType = "numeric"),
         contains = "AtomicList")

setClass("ComplexList", representation("VIRTUAL"),
         prototype = prototype(elementType = "complex"),
         contains = "AtomicList")

setClass("CharacterList", representation("VIRTUAL"),
         prototype = prototype(elementType = "character"),
         contains = "AtomicList")

setClass("RawList", representation("VIRTUAL"),
         prototype = prototype(elementType = "raw"),
         contains = "AtomicList")

setClass("RleList", representation("VIRTUAL"),
         prototype = prototype(elementType = "Rle"),
         contains = "AtomicList")
