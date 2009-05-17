### =========================================================================
### AtomicTypedList objects
### -------------------------------------------------------------------------

## A list that holds atomic objects

setClass("LogicalList", representation("VIRTUAL"),
         prototype = prototype(elementType = "logical"),
         contains = c("TypedListLike", "Sequence"))
setClass("CompressedLogicalList",
         prototype = prototype(elementType = "logical",
                               unlistData = logical()),
         contains = c("CompressedTypedListLike", "LogicalList"))
setClass("SimpleLogicalList",
         prototype = prototype(elementType = "logical"),
         contains = c("SimpleTypedListLike", "LogicalList"))

setClass("IntegerList", representation("VIRTUAL"),
         prototype = prototype(elementType = "integer"),
         contains = c("TypedListLike", "Sequence"))
setClass("CompressedIntegerList",
         prototype = prototype(elementType = "integer",
                               unlistData = integer()),
         contains = c("CompressedTypedListLike", "IntegerList"))
setClass("SimpleIntegerList",
         prototype = prototype(elementType = "integer"),
         contains = c("SimpleTypedListLike", "IntegerList"))

setClass("NumericList", representation("VIRTUAL"),
         prototype = prototype(elementType = "numeric"),
         contains = c("TypedListLike", "Sequence"))
setClass("CompressedNumericList",
         prototype = prototype(elementType = "numeric",
                               unlistData = numeric()),
         contains = c("CompressedTypedListLike", "NumericList"))
setClass("SimpleNumericList",
         prototype = prototype(elementType = "numeric"),
         contains = c("SimpleTypedListLike", "NumericList"))

setClass("ComplexList", representation("VIRTUAL"),
         prototype = prototype(elementType = "complex"),
         contains = c("TypedListLike", "Sequence"))
setClass("CompressedComplexList",
         prototype = prototype(elementType = "complex",
                               unlistData = complex()),
         contains = c("CompressedTypedListLike", "ComplexList"))
setClass("SimpleComplexList",
         prototype = prototype(elementType = "complex"),
         contains = c("SimpleTypedListLike", "ComplexList"))

setClass("CharacterList", representation("VIRTUAL"),
         prototype = prototype(elementType = "character"),
         contains = c("TypedListLike", "Sequence"))
setClass("CompressedCharacterList",
         prototype = prototype(elementType = "character",
                               unlistData = character()),
         contains = c("CompressedTypedListLike", "CharacterList"))
setClass("SimpleCharacterList",
         prototype = prototype(elementType = "character"),
         contains = c("SimpleTypedListLike", "CharacterList"))

setClass("RawList", representation("VIRTUAL"),
         prototype = prototype(elementType = "raw"),
         contains = c("TypedListLike", "Sequence"))
setClass("CompressedRawList",
         prototype = prototype(elementType = "raw",
                               unlistData = raw()),
         contains = c("CompressedTypedListLike", "RawList"))
setClass("SimpleRawList",
         prototype = prototype(elementType = "raw"),
         contains = c("SimpleTypedListLike", "RawList"))

setClass("RleList", representation("VIRTUAL"),
         prototype = prototype(elementType = "Rle"),
         contains = c("TypedListLike", "Sequence"))
setClass("CompressedRleList",
         prototype = prototype(elementType = "Rle",
                               unlistData = new("Rle")),
         contains = c("CompressedTypedListLike", "RleList"))
setClass("SimpleRleList",
         prototype = prototype(elementType = "Rle"),
         contains = c("SimpleTypedListLike", "RleList"))

LogicalList <- function(..., compress = TRUE)
{
    if (compress)
        listClass <- "CompressedLogicalList"
    else
        listClass <- "SimpleLogicalList"
    TypedListLike(listClass, lapply(list(...), as.logical))
}

IntegerList <- function(..., compress = TRUE)
{
    if (compress)
        listClass <- "CompressedIntegerList"
    else
        listClass <- "SimpleIntegerList"
    TypedListLike(listClass, lapply(list(...), as.integer))
}

NumericList <- function(..., compress = TRUE)
{
    if (compress)
        listClass <- "CompressedNumericList"
    else
        listClass <- "SimpleNumericList"
    TypedListLike(listClass, lapply(list(...), as.numeric))
}

ComplexList <- function(..., compress = TRUE)
{
    if (compress)
        listClass <- "CompressedComplexList"
    else
        listClass <- "SimpleComplexList"
    TypedListLike(listClass, lapply(list(...), as.complex))
}

CharacterList <- function(..., compress = TRUE)
{
    if (compress)
        listClass <- "CompressedCharacterList"
    else
        listClass <- "SimpleCharacterList"
    TypedListLike(listClass, lapply(list(...), as.character))
}

RawList <- function(..., compress = TRUE)
{
    if (compress)
        listClass <- "CompressedRawList"
    else
        listClass <- "SimpleRawList"
    TypedListLike(listClass, lapply(list(...), as.raw))
}

RleList <- function(..., compress = TRUE)
{
    if (compress)
        listClass <- "CompressedRleList"
    else
        listClass <- "SimpleRleList"
    TypedListLike(listClass, lapply(list(...), as, "Rle"))
}
