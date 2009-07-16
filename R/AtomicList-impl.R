### =========================================================================
### AtomicList object implementations
### -------------------------------------------------------------------------

## A list that holds atomic objects

setClass("CompressedLogicalList",
         prototype = prototype(elementType = "logical",
                               unlistData = logical()),
         contains = c("CompressedList", "LogicalList"))
setClass("SimpleLogicalList",
         prototype = prototype(elementType = "logical"),
         contains = c("SimpleList", "LogicalList"))

setClass("CompressedIntegerList",
         prototype = prototype(elementType = "integer",
                               unlistData = integer()),
         contains = c("CompressedList", "IntegerList"))
setClass("SimpleIntegerList",
         prototype = prototype(elementType = "integer"),
         contains = c("SimpleList", "IntegerList"))

setClass("CompressedNumericList",
         prototype = prototype(elementType = "numeric",
                               unlistData = numeric()),
         contains = c("CompressedList", "NumericList"))
setClass("SimpleNumericList",
         prototype = prototype(elementType = "numeric"),
         contains = c("SimpleList", "NumericList"))

setClass("CompressedComplexList",
         prototype = prototype(elementType = "complex",
                               unlistData = complex()),
         contains = c("CompressedList", "ComplexList"))
setClass("SimpleComplexList",
         prototype = prototype(elementType = "complex"),
         contains = c("SimpleList", "ComplexList"))

setClass("CompressedCharacterList",
         prototype = prototype(elementType = "character",
                               unlistData = character()),
         contains = c("CompressedList", "CharacterList"))
setClass("SimpleCharacterList",
         prototype = prototype(elementType = "character"),
         contains = c("SimpleList", "CharacterList"))

setClass("CompressedRawList",
         prototype = prototype(elementType = "raw",
                               unlistData = raw()),
         contains = c("CompressedList", "RawList"))
setClass("SimpleRawList",
         prototype = prototype(elementType = "raw"),
         contains = c("SimpleList", "RawList"))

setClass("CompressedRleList",
         prototype = prototype(elementType = "Rle",
                               unlistData = new("Rle")),
         contains = c("CompressedList", "RleList"))
setClass("SimpleRleList",
         prototype = prototype(elementType = "Rle"),
         contains = c("SimpleList", "RleList"))

LogicalList <- function(..., compress = TRUE)
{
    listData <- list(...)
    if (length(listData) == 1 && is.list(listData[[1]]))
        listData <- listData[[1]]
    if (compress)
        newCompressedList("CompressedLogicalList", lapply(listData, as.logical))
    else
        newSimpleList("SimpleLogicalList", lapply(listData, as.logical))
}

IntegerList <- function(..., compress = TRUE)
{
    listData <- list(...)
    if (length(listData) == 1 && is.list(listData[[1]]))
        listData <- listData[[1]]
    if (compress)
        newCompressedList("CompressedIntegerList", lapply(listData, as.integer))
    else
        newSimpleList("SimpleIntegerList", lapply(listData, as.integer))
}

NumericList <- function(..., compress = TRUE)
{
    listData <- list(...)
    if (length(listData) == 1 && is.list(listData[[1]]))
        listData <- listData[[1]]
    if (compress)
        newCompressedList("CompressedNumericList", lapply(listData, as.numeric))
    else
        newSimpleList("SimpleNumericList", lapply(listData, as.numeric))
}

ComplexList <- function(..., compress = TRUE)
{
    listData <- list(...)
    if (length(listData) == 1 && is.list(listData[[1]]))
        listData <- listData[[1]]
    if (compress)
        newCompressedList("CompressedComplexList", lapply(listData, as.complex))
    else
        newSimpleList("SimpleComplexList", lapply(listData, as.complex))
}

CharacterList <- function(..., compress = TRUE)
{
    listData <- list(...)
    if (length(listData) == 1 && is.list(listData[[1]]))
        listData <- listData[[1]]
    if (compress)
        newCompressedList("CompressedCharacterList", lapply(listData, as.character))
    else
        newSimpleList("SimpleCharacterList", lapply(listData, as.character))
}

RawList <- function(..., compress = TRUE)
{
    listData <- list(...)
    if (length(listData) == 1 && is.list(listData[[1]]))
        listData <- listData[[1]]
    if (compress)
        newCompressedList("CompressedRawList", lapply(listData, as.raw))
    else
        newSimpleList("SimpleRawList", lapply(listData, as.raw))
}

RleList <- function(..., compress = TRUE)
{
    listData <- list(...)
    if (length(listData) == 1 && is.list(listData[[1]]))
        listData <- listData[[1]]
    if (compress)
        newCompressedList("CompressedRleList", lapply(listData,  as, "Rle"))
    else
        newSimpleList("SimpleRleList", lapply(listData, as, "Rle"))
}
