### =========================================================================
### AtomicTypedList objects
### -------------------------------------------------------------------------

## A list that holds atomic objects

setClass("LogicalList",
         prototype = prototype(elementClass = "logical", compressible = TRUE),
         contains = "TypedList")
 
setClass("IntegerList",
         prototype = prototype(elementClass = "integer", compressible = TRUE),
         contains = "TypedList")

setClass("NumericList",
         prototype = prototype(elementClass = "numeric", compressible = TRUE),
         contains = "TypedList")

setClass("ComplexList",
         prototype = prototype(elementClass = "complex", compressible = TRUE),
         contains = "TypedList")

setClass("CharacterList",
         prototype = prototype(elementClass = "character", compressible = TRUE),
         contains = "TypedList")

setClass("RawList",
         prototype = prototype(elementClass = "raw", compressible = TRUE),
         contains = "TypedList")

LogicalList <- function(..., compress = TRUE)
{
  TypedList("LogicalList", elements = lapply(list(...), as.logical),
            compress = compress)
}

IntegerList <- function(..., compress = TRUE)
{
  TypedList("IntegerList", elements = lapply(list(...), as.integer),
            compress = compress)
}

NumericList <- function(..., compress = TRUE)
{
  TypedList("NumericList", elements = lapply(list(...), as.numeric),
            compress = compress)
}

ComplexList <- function(..., compress = TRUE)
{
  TypedList("ComplexList", elements = lapply(list(...), as.complex),
            compress = compress)
}

CharacterList <- function(..., compress = TRUE)
{
  TypedList("CharacterList", elements = lapply(list(...), as.character),
            compress = compress)
}

RawList <- function(..., compress = TRUE)
{
  TypedList("RawList", elements = lapply(list(...), as.raw),
            compress = compress)
}
