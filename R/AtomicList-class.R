### =========================================================================
### AtomicList objects
### -------------------------------------------------------------------------

## A list that holds atomic objects

setClass("LogicalList", representation("VIRTUAL"),
         prototype = prototype(elementType = "logical"),
         contains = "SequenceList")

setClass("IntegerList", representation("VIRTUAL"),
         prototype = prototype(elementType = "integer"),
         contains = "SequenceList")

setClass("NumericList", representation("VIRTUAL"),
         prototype = prototype(elementType = "numeric"),
         contains = "SequenceList")

setClass("ComplexList", representation("VIRTUAL"),
         prototype = prototype(elementType = "complex"),
         contains = "SequenceList")

setClass("CharacterList", representation("VIRTUAL"),
         prototype = prototype(elementType = "character"),
         contains = "SequenceList")

setClass("RawList", representation("VIRTUAL"),
         prototype = prototype(elementType = "raw"),
         contains = "SequenceList")

setClass("RleList", representation("VIRTUAL"),
         prototype = prototype(elementType = "Rle"),
         contains = "SequenceList")
