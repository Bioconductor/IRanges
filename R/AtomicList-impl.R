### =========================================================================
### AtomicList object implementations
### -------------------------------------------------------------------------

setClassUnion("atomic",
              c("raw", "logical", "integer", "numeric", "character", "complex"))

## A list that holds atomic objects

setClass("CompressedAtomicList",
         contains =  c("CompressedList", "AtomicList"),
         representation("VIRTUAL"))

setClass("SimpleAtomicList",
         contains =  c("SimpleList", "AtomicList"),
         representation("VIRTUAL"))
 
setClass("CompressedLogicalList",
         prototype = prototype(elementType = "logical",
                               unlistData = logical()),
         contains = c("CompressedAtomicList", "LogicalList"))
setClass("SimpleLogicalList",
         prototype = prototype(elementType = "logical"),
         contains = c("SimpleAtomicList", "LogicalList"))

setClass("CompressedIntegerList",
         prototype = prototype(elementType = "integer",
                               unlistData = integer()),
         contains = c("CompressedAtomicList", "IntegerList"))
setClass("SimpleIntegerList",
         prototype = prototype(elementType = "integer"),
         contains = c("SimpleAtomicList", "IntegerList"))

setClass("CompressedNumericList",
         prototype = prototype(elementType = "numeric",
                               unlistData = numeric()),
         contains = c("CompressedAtomicList", "NumericList"))
setClass("SimpleNumericList",
         prototype = prototype(elementType = "numeric"),
         contains = c("SimpleAtomicList", "NumericList"))

setClass("CompressedComplexList",
         prototype = prototype(elementType = "complex",
                               unlistData = complex()),
         contains = c("CompressedAtomicList", "ComplexList"))
setClass("SimpleComplexList",
         prototype = prototype(elementType = "complex"),
         contains = c("SimpleAtomicList", "ComplexList"))

setClass("CompressedCharacterList",
         prototype = prototype(elementType = "character",
                               unlistData = character()),
         contains = c("CompressedAtomicList", "CharacterList"))
setClass("SimpleCharacterList",
         prototype = prototype(elementType = "character"),
         contains = c("SimpleAtomicList", "CharacterList"))

setClass("CompressedRawList",
         prototype = prototype(elementType = "raw",
                               unlistData = raw()),
         contains = c("CompressedAtomicList", "RawList"))
setClass("SimpleRawList",
         prototype = prototype(elementType = "raw"),
         contains = c("SimpleAtomicList", "RawList"))

setClass("CompressedRleList",
         prototype = prototype(elementType = "Rle",
                               unlistData = new("Rle")),
         contains = c("CompressedAtomicList", "RleList"))
setClass("SimpleRleList",
         prototype = prototype(elementType = "Rle"),
         contains = c("SimpleAtomicList", "RleList"))

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Constructors
###

LogicalList <- function(..., compress = TRUE)
{
    if (!isTRUEorFALSE(compress))
        stop("'compress' must be TRUE or FALSE")
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
    if (!isTRUEorFALSE(compress))
        stop("'compress' must be TRUE or FALSE")
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
    if (!isTRUEorFALSE(compress))
        stop("'compress' must be TRUE or FALSE")
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
    if (!isTRUEorFALSE(compress))
        stop("'compress' must be TRUE or FALSE")
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
    if (!isTRUEorFALSE(compress))
        stop("'compress' must be TRUE or FALSE")
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
    if (!isTRUEorFALSE(compress))
        stop("'compress' must be TRUE or FALSE")
    listData <- list(...)
    if (length(listData) == 1 && is.list(listData[[1]]))
        listData <- listData[[1]]
    if (compress)
        newCompressedList("CompressedRawList", lapply(listData, as.raw))
    else
        newSimpleList("SimpleRawList", lapply(listData, as.raw))
}

RleList <- function(..., compress = FALSE)
{
    if (!isTRUEorFALSE(compress))
        stop("'compress' must be TRUE or FALSE")
    listData <- list(...)
    if (length(listData) == 1 && is.list(listData[[1]]))
        listData <- listData[[1]]
    if (compress)
        newCompressedList("CompressedRleList", lapply(listData,  as, "Rle"))
    else
        newSimpleList("SimpleRleList", lapply(listData, as, "Rle"))
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Group generic methods
###

atomicElementListClass <- function(x) {
    if (is(x, "Rle"))
        ans <- "RleList"
    else if (is.raw(x))
        ans <- "RawList"
    else if (is.logical(x))
        ans <- "LogicalList"
    else if (is.integer(x))
        ans <- "IntegerList"
    else if (is.numeric(x))
        ans <- "NumericList"
    else if (is.complex(x))
        ans <- "ComplexList"
    else if (is.character(x))
        ans <- "CharacterList"
    else
        ans <- NA_character_
    ans
}

SimpleAtomicList <- function(listData) {
    classOrder <-
      c("CharacterList", "ComplexList", "NumericList", "IntegerList",
        "LogicalList", "RawList", "RleList")
    uniqueClasses <- unique(unlist(lapply(listData, atomicElementListClass)))
    if (any(is.na(uniqueClasses)))
        stop("cannot create a SimpleAtomicList with non-atomic elements")
    baseClass <- classOrder[min(match(uniqueClasses, classOrder))]
    do.call(baseClass, c(listData, compress = FALSE))
}

CompressedAtomicList <- function(unlistData, partitioning) {
    classOrder <-
      c("CharacterList", "ComplexList", "NumericList", "IntegerList", 
        "LogicalList", "RawList", "RleList")
    baseClass <- atomicElementListClass(unlistData)
    if (is.na(baseClass))
        stop("cannot create a CompressedAtomicList with non-atomic elements")
    new2(paste("Compressed", baseClass, sep = ""), unlistData = unlistData,
         partitioning = partitioning, check = FALSE)
}

setMethod("Ops",
          signature(e1 = "SimpleAtomicList", e2 = "SimpleAtomicList"),
          function(e1, e2)
          {
              n <- length(e1)
              if (n != length(e2))
                  stop("cannot perform Ops on unequal length lists")
              if (n == 0)
                  return(e1)
              SimpleAtomicList(Map(.Generic, e1, e2))
          })

setMethod("Ops",
          signature(e1 = "CompressedAtomicList", e2 = "CompressedAtomicList"),
          function(e1, e2)
          {
              n <- length(e1)
              if (n != length(e2))
                  stop("cannot perform Ops on unequal length lists")
              if (n == 0)
                  return(e1)
              nms <- names(e1)
              if (is.null(nms))
                  nms <- names(e2)
              partitioning <- e1@partitioning
              names(partitioning) <- nms
              n1 <- elementLengths(e1)
              n2 <- elementLengths(e2)
              for (i in which(n2 == 0))
                  e1[[i]] <- e1[[i]][integer(0)]
              for (i in which(n1 == 0))
                  e2[[i]] <- e2[[i]][integer(0)]
              n1 <- elementLengths(e1)
              n2 <- elementLengths(e2)
              for (i in which(n1 < n2))
                  e1[[i]] <- rep(e1[[i]], length.out = n2[i])
              for (i in which(n2 < n1))
                  e2[[i]] <- rep(e2[[i]], length.out = n1[i])
              CompressedAtomicList(callGeneric(e1@unlistData, e2@unlistData),
                                   partitioning = partitioning)
          })

setMethod("Ops",
          signature(e1 = "SimpleAtomicList", e2 = "CompressedAtomicList"),
          function(e1, e2)
          {
              classMap <-
                c("character" = "CharacterList", "complex" = "ComplexList",
                  "numeric" = "NumericList", "integer" = "IntegerList",
                  "logical" = "LogicalList", "raw" = "RawList",
                  "Rle" = "RleList")
              if (sum(elementLengths(e1)) < .Machine$integer.max)
                  e1 <-
                    do.call(classMap[e1@elementType],
                            c(e1@listData, compress = TRUE))
              else
                  e2 <-
                    do.call(classMap[e2@elementType],
                            c(as.list(e2), compress = FALSE))
              callGeneric(e1, e2)
          })


setMethod("Ops",
          signature(e1 = "CompressedAtomicList", e2 = "SimpleAtomicList"),
          function(e1, e2)
          {
              classMap <-
                c("character" = "CharacterList", "complex" = "ComplexList",
                  "numeric" = "NumericList", "integer" = "IntegerList",
                  "logical" = "LogicalList", "raw" = "RawList",
                  "Rle" = "RleList")
              if (sum(elementLengths(e2)) < .Machine$integer.max)
                  e2 <-
                    do.call(classMap[e2@elementType],
                            c(e2@listData, compress = TRUE))
              else
                  e1 <-
                    do.call(classMap[e1@elementType],
                            c(as.list(e1), compress = FALSE))
              callGeneric(e1, e2)
          })

setMethod("Ops",
          signature(e1 = "AtomicList", e2 = "atomic"),
          function(e1, e2)
          {
              e2 <- SimpleAtomicList(rep(list(e2), length(e1)))
              callGeneric(e1, e2)
          })

setMethod("Ops",
          signature(e1 = "atomic", e2 = "AtomicList"),
          function(e1, e2)
          {
              e1 <- SimpleAtomicList(rep(list(e1), length(e2)))
              callGeneric(e1, e2)
          })

setMethod("!", "SimpleLogicalList",
          function(x) {
              x@listData <- lapply(x@listData, "!")
              x
          })

setMethod("!", "CompressedLogicalList",
          function(x) {
              x@unlistData <- !x@unlistData
              x
          })

setMethod("Math", "CompressedAtomicList",
          function(x)
              CompressedAtomicList(callGeneric(x@unlistData),
                                   partitioning = x@partitioning))

setMethod("Math", "SimpleAtomicList",
          function(x) SimpleAtomicList(lapply(x@listData, .Generic)))

setMethod("Math2", "CompressedAtomicList",
          function(x, digits)
          {
              if (missing(digits))
                  digits <- ifelse(.Generic == "round", 0, 6)
              CompressedAtomicList(callGeneric(x@unlistData, digits = digits),
                                   partitioning = x@partitioning)
          })

setMethod("Math2", "SimpleAtomicList",
          function(x, digits)
          {
              if (missing(digits))
                  digits <- ifelse(.Generic == "round", 0, 6)
              SimpleAtomicList(lapply(x@listData, .Generic, digits = digits))
          })

setMethod("Summary", "AtomicList",
          function(x, ..., na.rm = FALSE) sapply(x, .Generic, na.rm = na.rm))

setMethod("Complex", "CompressedAtomicList",
          function(z)
              CompressedAtomicList(callGeneric(z@unlistData),
                                   partitioning = z@partitioning))

setMethod("Complex", "SimpleAtomicList",
          function(z) SimpleAtomicList(lapply(z@listData, .Generic)))

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Running window statistic methods
###

setMethod("runsum", "RleList",
          function(x, k)
              endoapply(x, runsum, k = k))

setMethod("runwtsum", "RleList",
          function(x, k, wt)
              endoapply(x, runwtsum, k = k, wt = wt))

setMethod("runq", "RleList",
          function(x, k, i)
              endoapply(x, runq, k = k, i = i))
