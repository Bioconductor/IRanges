### =========================================================================
### AtomicTypedList objects
### -------------------------------------------------------------------------

## A list that holds atomic objects

setClass("LogicalList", representation("VIRTUAL"),
         prototype = prototype(elementType = "logical"),
         contains="TypedListV2")
setClass("CompressedLogicalList",
         prototype = prototype(elementType = "logical",
                               unlistData = logical()),
         contains = c("CompressedTypedList", "LogicalList"))
setClass("SimpleLogicalList",
         prototype = prototype(elementType = "logical"),
         contains = c("SimpleTypedList", "LogicalList"))

setClass("IntegerList", representation("VIRTUAL"),
         prototype = prototype(elementType = "integer"),
         contains="TypedListV2")
setClass("CompressedIntegerList",
         prototype = prototype(elementType = "integer",
                               unlistData = integer()),
         contains = c("CompressedTypedList", "IntegerList"))
setClass("SimpleIntegerList",
         prototype = prototype(elementType = "integer"),
         contains = c("SimpleTypedList", "IntegerList"))

setClass("NumericList", representation("VIRTUAL"),
         prototype = prototype(elementType = "numeric"),
         contains="TypedListV2")
setClass("CompressedNumericList",
         prototype = prototype(elementType = "numeric",
                               unlistData = numeric()),
         contains = c("CompressedTypedList", "NumericList"))
setClass("SimpleNumericList",
         prototype = prototype(elementType = "numeric"),
         contains = c("SimpleTypedList", "NumericList"))

setClass("ComplexList", representation("VIRTUAL"),
         prototype = prototype(elementType = "complex"),
         contains="TypedListV2")
setClass("CompressedComplexList",
         prototype = prototype(elementType = "complex",
                               unlistData = complex()),
         contains = c("CompressedTypedList", "ComplexList"))
setClass("SimpleComplexList",
         prototype = prototype(elementType = "complex"),
         contains = c("SimpleTypedList", "ComplexList"))

setClass("CharacterList", representation("VIRTUAL"),
         prototype = prototype(elementType = "character"),
         contains="TypedListV2")
setClass("CompressedCharacterList",
         prototype = prototype(elementType = "character",
                               unlistData = character()),
         contains = c("CompressedTypedList", "CharacterList"))
setClass("SimpleCharacterList",
         prototype = prototype(elementType = "character"),
         contains = c("SimpleTypedList", "CharacterList"))

setClass("RawList", representation("VIRTUAL"),
         prototype = prototype(elementType = "raw"),
         contains="TypedListV2")
setClass("CompressedRawList",
         prototype = prototype(elementType = "raw",
                               unlistData = raw()),
         contains = c("CompressedTypedList", "RawList"))
setClass("SimpleRawList",
         prototype = prototype(elementType = "raw"),
         contains = c("SimpleTypedList", "RawList"))

setClass("RleList", representation("VIRTUAL"),
         prototype = prototype(elementType = "Rle"),
         contains="TypedListV2")
setClass("CompressedRleList",
         prototype = prototype(elementType = "Rle",
                               unlistData = new("Rle")),
         contains = c("CompressedTypedList", "RleList"))
setClass("SimpleRleList",
         prototype = prototype(elementType = "Rle"),
         contains = c("SimpleTypedList", "RleList"))

LogicalList <- function(..., compress = TRUE)
{
    if (compress)
        listClass <- "CompressedLogicalList"
    else
        listClass <- "SimpleLogicalList"
    TypedListV2(listClass, lapply(list(...), as.logical))
}

IntegerList <- function(..., compress = TRUE)
{
    if (compress)
        listClass <- "CompressedIntegerList"
    else
        listClass <- "SimpleIntegerList"
    TypedListV2(listClass, lapply(list(...), as.integer))
}

NumericList <- function(..., compress = TRUE)
{
    if (compress)
        listClass <- "CompressedNumericList"
    else
        listClass <- "SimpleNumericList"
    TypedListV2(listClass, lapply(list(...), as.numeric))
}

ComplexList <- function(..., compress = TRUE)
{
    if (compress)
        listClass <- "CompressedComplexList"
    else
        listClass <- "SimpleComplexList"
    TypedListV2(listClass, lapply(list(...), as.complex))
}

CharacterList <- function(..., compress = TRUE)
{
    if (compress)
        listClass <- "CompressedCharacterList"
    else
        listClass <- "SimpleCharacterList"
    TypedListV2(listClass, lapply(list(...), as.character))
}

RawList <- function(..., compress = TRUE)
{
    if (compress)
        listClass <- "CompressedRawList"
    else
        listClass <- "SimpleRawList"
    TypedListV2(listClass, lapply(list(...), as.raw))
}

RleList <- function(..., compress = TRUE)
{
    if (compress)
        listClass <- "CompressedRleList"
    else
        listClass <- "SimpleRleList"
    TypedListV2(listClass, lapply(list(...), as, "Rle"))
}
