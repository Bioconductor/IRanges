### =========================================================================
### AtomicTypedList object implementations
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
    if (compress)
        CompressedList("CompressedLogicalList", lapply(list(...), as.logical))
    else
        SimpleList("SimpleLogicalList", lapply(list(...), as.logical))
}

IntegerList <- function(..., compress = TRUE)
{
    if (compress)
        CompressedList("CompressedIntegerList", lapply(list(...), as.integer))
    else
        SimpleList("SimpleIntegerList", lapply(list(...), as.integer))
}

NumericList <- function(..., compress = TRUE)
{
    if (compress)
        CompressedList("CompressedNumericList", lapply(list(...), as.numeric))
    else
        SimpleList("SimpleNumericList", lapply(list(...), as.numeric))
}

ComplexList <- function(..., compress = TRUE)
{
    if (compress)
        CompressedList("CompressedComplexList", lapply(list(...), as.complex))
    else
        SimpleList("SimpleComplexList", lapply(list(...), as.complex))
}

CharacterList <- function(..., compress = TRUE)
{
    if (compress)
        CompressedList("CompressedCharacterList", lapply(list(...), as.character))
    else
        SimpleList("SimpleCharacterList", lapply(list(...), as.character))
}

RawList <- function(..., compress = TRUE)
{
    if (compress)
        CompressedList("CompressedRawList", lapply(list(...), as.raw))
    else
        SimpleList("SimpleRawList", lapply(list(...), as.raw))
}

RleList <- function(..., compress = TRUE)
{
    if (compress)
        CompressedList("CompressedRleList", lapply(list(...),  as, "Rle"))
    else
        SimpleList("SimpleRleList", lapply(list(...), as, "Rle"))
}
