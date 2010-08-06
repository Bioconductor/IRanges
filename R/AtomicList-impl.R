### =========================================================================
### AtomicList object implementations
### -------------------------------------------------------------------------

setClassUnion("atomic",
              c("raw", "logical", "integer", "numeric", "character", "complex"))

## A list that holds atomic objects

setClass("CompressedAtomicList",
         contains =  c("AtomicList", "CompressedList"),
         representation("VIRTUAL"))

setClass("SimpleAtomicList",
         contains =  c("AtomicList", "SimpleList"),
         representation("VIRTUAL"))
 
setClass("CompressedLogicalList",
         prototype = prototype(elementType = "logical",
                               unlistData = logical()),
         contains = c("LogicalList", "CompressedAtomicList"))
setClass("SimpleLogicalList",
         prototype = prototype(elementType = "logical"),
         contains = c("LogicalList", "SimpleAtomicList"))

setClass("CompressedIntegerList",
         prototype = prototype(elementType = "integer",
                               unlistData = integer()),
         contains = c("IntegerList", "CompressedAtomicList"))
setClass("SimpleIntegerList",
         prototype = prototype(elementType = "integer"),
         contains = c("IntegerList", "SimpleAtomicList"))

setClass("CompressedNumericList",
         prototype = prototype(elementType = "numeric",
                               unlistData = numeric()),
         contains = c("NumericList", "CompressedAtomicList"))
setClass("SimpleNumericList",
         prototype = prototype(elementType = "numeric"),
         contains = c("NumericList", "SimpleAtomicList"))

setClass("CompressedComplexList",
         prototype = prototype(elementType = "complex",
                               unlistData = complex()),
         contains = c("ComplexList", "CompressedAtomicList"))
setClass("SimpleComplexList",
         prototype = prototype(elementType = "complex"),
         contains = c("ComplexList", "SimpleAtomicList"))

setClass("CompressedCharacterList",
         prototype = prototype(elementType = "character",
                               unlistData = character()),
         contains = c("CharacterList", "CompressedAtomicList"))
setClass("SimpleCharacterList",
         prototype = prototype(elementType = "character"),
         contains = c("CharacterList", "SimpleAtomicList"))

setClass("CompressedRawList",
         prototype = prototype(elementType = "raw",
                               unlistData = raw()),
         contains = c("RawList", "CompressedAtomicList"))
setClass("SimpleRawList",
         prototype = prototype(elementType = "raw"),
         contains = c("RawList", "SimpleAtomicList"))

setClass("CompressedRleList",
         prototype = prototype(elementType = "Rle",
                               unlistData = new("Rle")),
         contains = c("RleList", "CompressedAtomicList"))
setClass("SimpleRleList",
         prototype = prototype(elementType = "Rle"),
         contains = c("RleList", "SimpleAtomicList"))

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Constructors
###

LogicalList <- function(..., compress = TRUE)
{
    if (!isTRUEorFALSE(compress))
        stop("'compress' must be TRUE or FALSE")
    listData <- list(...)
    if (length(listData) == 1 && is.list(listData[[1L]]))
        listData <- listData[[1L]]
    listData <-
      lapply(listData, function(x) structure(as.logical(x), names = names(x)))
    if (compress)
        newCompressedList("CompressedLogicalList", listData)
    else
        newSimpleList("SimpleLogicalList", listData)
}

.dotargsAsListOfIntegerVectors <- function(dotargs)
{
    if (length(dotargs) == 1) {
        arg1 <- dotargs[[1L]]
        if (is(arg1, "IntegerList"))
            return(as.list(arg1))
        if (is.character(arg1))
            return(strsplitAsListOfIntegerVectors(arg1))
        if (is.list(arg1))
            dotargs <- arg1
    }
    lapply(dotargs, function(x) structure(as.integer(x), names = names(x)))
}

IntegerList <- function(..., compress = TRUE)
{
    if (!isTRUEorFALSE(compress))
        stop("'compress' must be TRUE or FALSE")
    dotargs <- list(...)
    if (length(dotargs) == 1 && is(dotargs[[1L]], "IntegerList")) {
        if (is(dotargs[[1L]], "CompressedIntegerList") && compress
         || is(dotargs[[1L]], "SimpleIntegerList") && !compress)
            return(dotargs[[1L]])
    }
    listData <- .dotargsAsListOfIntegerVectors(dotargs)
    if (compress)
        newCompressedList("CompressedIntegerList", listData)
    else
        newSimpleList("SimpleIntegerList", listData)
}

NumericList <- function(..., compress = TRUE)
{
    if (!isTRUEorFALSE(compress))
        stop("'compress' must be TRUE or FALSE")
    listData <- list(...)
    if (length(listData) == 1 && is.list(listData[[1L]]))
        listData <- listData[[1L]]
    listData <-
      lapply(listData, function(x) structure(as.numeric(x), names = names(x)))
    if (compress)
        newCompressedList("CompressedNumericList", listData)
    else
        newSimpleList("SimpleNumericList", listData)
}

ComplexList <- function(..., compress = TRUE)
{
    if (!isTRUEorFALSE(compress))
        stop("'compress' must be TRUE or FALSE")
    listData <- list(...)
    if (length(listData) == 1 && is.list(listData[[1L]]))
        listData <- listData[[1L]]
    listData <-
      lapply(listData, function(x) structure(as.complex(x), names = names(x)))
    if (compress)
        newCompressedList("CompressedComplexList", listData)
    else
        newSimpleList("SimpleComplexList", listData)
}

CharacterList <- function(..., compress = TRUE)
{
    if (!isTRUEorFALSE(compress))
        stop("'compress' must be TRUE or FALSE")
    listData <- list(...)
    if (length(listData) == 1 && is.list(listData[[1L]]))
        listData <- listData[[1L]]
    listData <-
      lapply(listData, function(x) structure(as.character(x), names = names(x)))
    if (compress)
        newCompressedList("CompressedCharacterList", listData)
    else
        newSimpleList("SimpleCharacterList", listData)
}

RawList <- function(..., compress = TRUE)
{
    if (!isTRUEorFALSE(compress))
        stop("'compress' must be TRUE or FALSE")
    listData <- list(...)
    if (length(listData) == 1 && is.list(listData[[1L]]))
        listData <- listData[[1L]]
    listData <-
      lapply(listData, function(x) structure(as.raw(x), names = names(x)))
    if (compress)
        newCompressedList("CompressedRawList", listData)
    else
        newSimpleList("SimpleRawList", listData)
}

RleList <- function(..., compress = FALSE)
{
    if (!isTRUEorFALSE(compress))
        stop("'compress' must be TRUE or FALSE")
    listData <- list(...)
    if (length(listData) == 1 && is.list(listData[[1L]]))
        listData <- listData[[1L]]
    if (compress)
        newCompressedList("CompressedRleList", lapply(listData,  as, "Rle"))
    else
        newSimpleList("SimpleRleList", lapply(listData, as, "Rle"))
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion
###

setMethod("as.vector", c("AtomicList", "missing"), function(x, mode) unlist(x, use.names=FALSE))
setMethod("as.logical", "AtomicList", function(x) as.logical(unlist(x, use.names=FALSE)))
setMethod("as.integer", "AtomicList", function(x) as.integer(unlist(x, use.names=FALSE)))
setMethod("as.numeric", "AtomicList", function(x) as.numeric(unlist(x, use.names=FALSE)))
setMethod("as.complex", "AtomicList", function(x) as.complex(unlist(x, use.names=FALSE)))
setMethod("as.character", "AtomicList", function(x) as.character(unlist(x, use.names=FALSE)))
setMethod("as.raw", "AtomicList", function(x) as.raw(unlist(x, use.names=FALSE)))
setMethod("as.factor", "AtomicList", function(x) as.factor(unlist(x, use.names=FALSE)))
setMethod("as.data.frame", "AtomicList",
          function(x, row.names=NULL, optional=FALSE, ...)
          {
              if (is.null(row.names)) {
                  row.names <- unlist(lapply(x, names), use.names = FALSE)
              } else if (!is.character(row.names)) {
                  stop("'row.names'  must be NULL or a character vector")
              }
              spaceLevels <- seq_len(length(x))
              if (length(names(x)) > 0) {
                  spaceLabels <- names(x)
              } else {
                  spaceLabels <- as.character(spaceLevels)
              }
              data.frame(space =
                         factor(rep.int(seq_len(length(x)), elementLengths(x)),
                                levels = spaceLevels,
                                labels = spaceLabels),
                         value = as.vector(x),
                         row.names = row.names,
                         stringsAsFactors = FALSE)
          })
setMethod("as.list", "CompressedAtomicList",
          function(x, use.names = TRUE) {
              if (is(x, "CompressedRleList")) {
                  callNextMethod()
              } else {
                  codes <- seq_len(length(x))
                  ans <-
                    split(x@unlistData,
                          structure(rep.int(codes, elementLengths(x)),
                                    levels = as.character(codes),
                                    class = "factor"))
                  if (use.names) {
                      names(ans) <- names(x)
                  } else {
                      names(ans) <- NULL
                  }
                  ans
              }
          })

setAs("AtomicList", "vector", function(from) as.vector(from))
setAs("AtomicList", "logical", function(from) as.logical(from))
setAs("AtomicList", "integer", function(from) as.integer(from))
setAs("AtomicList", "numeric", function(from) as.numeric(from))
setAs("AtomicList", "complex", function(from) as.complex(from))
setAs("AtomicList", "character", function(from) as.character(from))
setAs("AtomicList", "raw", function(from) as.raw(from))
setAs("AtomicList", "factor", function(from) as.factor(from))
setAs("AtomicList", "data.frame", function(from) as.data.frame(from))
setAs("CompressedAtomicList", "list", function(from) as.list(from))

setAs("vector", "AtomicList", function(from) SimpleAtomicList(as.list(from)))

setMethod("lapply", "CompressedAtomicList",
          function(X, FUN, ...)
          {
              if (is(X, "CompressedRleList")) {
                  callNextMethod(X, FUN, ...)
              } else {
                  lapply(as.list(X), FUN, ...)
              }
          })

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
      c("RleList", "CharacterList", "ComplexList", "NumericList",
        "IntegerList", "LogicalList", "RawList")
    uniqueClasses <-
      unique(unlist(lapply(listData, atomicElementListClass), use.names=FALSE))
    if (anyMissing(uniqueClasses))
        stop("cannot create a SimpleAtomicList with non-atomic elements")
    baseClass <- classOrder[min(match(uniqueClasses, classOrder))]
    do.call(baseClass, c(listData, compress = FALSE))
}

CompressedAtomicList <- function(unlistData, partitioning) {
    classOrder <-
      c("RleList", "CharacterList", "ComplexList", "NumericList",
        "IntegerList", "LogicalList", "RawList")
    baseClass <- atomicElementListClass(unlistData)
    if (is.na(baseClass))
        stop("cannot create a CompressedAtomicList with non-atomic elements")
    new2(paste("Compressed", baseClass, sep = ""), unlistData = unlistData,
         partitioning = partitioning, check = FALSE)
}

CompressedAtomicListFromList <- function(listData) {
    classOrder <-
      c("RleList", "CharacterList", "ComplexList", "NumericList",
        "IntegerList", "LogicalList", "RawList")
    uniqueClasses <-
      unique(unlist(lapply(listData, atomicElementListClass), use.names=FALSE))
    if (anyMissing(uniqueClasses))
        stop("cannot create a SimpleAtomicList with non-atomic elements")
    baseClass <- classOrder[min(match(uniqueClasses, classOrder))]
    do.call(baseClass, c(listData, compress = TRUE))
}

setReplaceMethod("seqselect", "SimpleAtomicList",
                 function(x, start = NULL, end = NULL, width = NULL, value)
                 {
                     classOrder <-
                       c("RleList", "CharacterList", "ComplexList",
                         "NumericList", "IntegerList", "LogicalList", "RawList")
                     if (!is.null(value) && !is(value, "AtomicList")) {
                         if (is.list(value))
                             value <- SimpleAtomicList(value)
                         else
                             value <- SimpleAtomicList(list(value))
                     }
                     if (!is.null(value) && !is(value, class(x))) {
                         xClass <-
                           which(unlist(lapply(classOrder,
                                               function(y) is(x, y))))
                         vClass <-
                           which(unlist(lapply(classOrder,
                                               function(y) is(value, y))))
                         if (xClass < vClass) {
                             value <-
                               do.call(classOrder[xClass],
                                       c(value@listData, compress = FALSE))
                         } else {
                             x <-
                               do.call(classOrder[vClass],
                                             c(x@listData, compress = FALSE))
                         }
                     }
                     callNextMethod()
                 })

setReplaceMethod("seqselect", "CompressedAtomicList",
                 function(x, start = NULL, end = NULL, width = NULL, value)
                 {
                     classOrder <-
                       c("Rle" = "RleList",
                         "character" = "CharacterList",
                         "complex" = "ComplexList",
                         "numeric" = "NumericList",
                         "integer" = "IntegerList",
                         "logical", "LogicalList",
                         "raw" = "RawList")
                     if (!is.null(value) && !is(value, "AtomicList")) {
                         if (!is.list(value))
                             value <- list(value)
                         partitioning <-
                           PartitioningByEnd(cumsum(unlist(lapply(value, length),
                                                           use.names=FALSE)),
                                             names = names(value))
                         value <-
                           CompressedAtomicList(do.call(c, unname(value)),
                                                partitioning)
                     }
                     if (!is.null(value) && !is(value, class(x))) {
                         xClass <-
                           which(unlist(lapply(classOrder,
                                               function(y) is(x, y))))
                         vClass <-
                           which(unlist(lapply(classOrder,
                                               function(y) is(value, y))))
                         if (xClass < vClass) {
                             value <-
                               new2(class(x),
                                    unlistData =
                                    as(value@unlistData, names(classOrder)[xClass]),
                                    partitioning = value@partitioning, check = FALSE)
                         } else {
                             x <-
                               new2(class(value),
                                    unlistData =
                                    as(x@unlistData, names(classOrder)[vClass]),
                                       partitioning = x@partitioning, check = FALSE)
                         }
                     }
                     callNextMethod()
                 })

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
              n1 <- elementLengths(e1)
              n2 <- elementLengths(e2)
              if (any(n1 != n2)) {
                  u1 <- as.list(e1)
                  u2 <- as.list(e2)
                  zeroLength <- which((n1 == 0L) | (n2 == 0L))
                  empty1 <- e1[[1L]][integer(0)]
                  empty2 <- e2[[1L]][integer(0)]
                  for (i in zeroLength) {
                      u1[[i]] <- empty1
                      u2[[i]] <- empty2
                  }
                  n1[zeroLength] <- 0L
                  n2[zeroLength] <- 0L
                  for (i in which(n1 < n2))
                      u1[[i]] <- rep(u1[[i]], length.out = n2[i])
                  for (i in which(n2 < n1))
                      u2[[i]] <- rep(u2[[i]], length.out = n1[i])
                  partitioningEnd <- cumsum(pmax.int(n1, n2))
                  e1@unlistData <- unlist(u1)
                  e1@partitioning@end <- partitioningEnd
                  e2@unlistData <- unlist(u2)
                  e2@partitioning@end <- partitioningEnd
              }
              partitioning <- e1@partitioning
              names(partitioning) <- nms
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

setMethod("Ops",
          signature(e1 = "CompressedAtomicList", e2 = "atomic"),
          function(e1, e2)
          {
              if (length(e2) == 1)
                  CompressedAtomicList(callGeneric(e1@unlistData, e2),
                                       partitioning = e1@partitioning)
              else
                  callNextMethod()
          })

setMethod("Ops",
          signature(e1 = "atomic", e2 = "CompressedAtomicList"),
          function(e1, e2)
          {
              if (length(e1) == 1)
                  CompressedAtomicList(callGeneric(e1, e2@unlistData),
                                       partitioning = e2@partitioning)
              else
                  callNextMethod()
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
### More list-ized methods
###

setListMethod <- function(f,
                          inputClass,
                          outputBaseClass,
                          whichArg = 1L,
                          remainingSignature = character(),
                          mapply = FALSE,
                          endoapply = FALSE,
                          applyToUnlist = FALSE,
                          where = topenv(parent.frame()))
{
    fargs <- formals(args(get(f)))
    args <- sapply(names(fargs), as.name)
    names(args) <- sub("...", "", names(args), fixed = TRUE)
    if (applyToUnlist) {
        call2 <- as.call(c(as.name("@"), args[[whichArg]], "partitioning"))
        args[[whichArg]] <-
          as.call(c(as.name("@"), args[[whichArg]], "unlistData"))
        call1 <- as.call(c(as.name(f), args))
        call <-
          as.call(c(as.name("new2"), paste("Compressed", outputBaseClass, sep=""),
                    unlistData = call1, partitioning = call2, check = FALSE))
    } else {
        args <- c(args[[whichArg]], as.name(f), args[-whichArg])
        if (endoapply) {
            call <- as.call(c(as.name("endoapply"), args))
        } else if (missing(outputBaseClass)) {
            call <- as.call(c(as.name("sapply"), args, list(simplify = TRUE)))
        } else {
            if (mapply) {
                if (length(args) <= 3) {
                    call <-
                      as.call(c(as.name("mapply"), args[c(2:1,3L)],
                                SIMPLIFY = FALSE))
                } else {
                    call <-
                      as.call(c(as.name("mapply"),
                                args[c(2:1,3L)],
                                MoreArgs =
                                as.call(c(as.name("list"), tail(args, -3))),
                                SIMPLIFY = FALSE))
                }
            } else {
                call <- as.call(c(as.name("lapply"), args))
            }
            if (extends(inputClass, "SimpleList")) {
                call <-
                  as.call(c(as.name("new2"),
                            paste("Simple", outputBaseClass, sep=""),
                            listData = call, check = FALSE))
            } else {
                call <-
                  as.call(c(as.name(outputBaseClass), call, compress = TRUE))
            }
        }
    }
    def <- as.function(c(fargs, call))
    environment(def) <- parent.frame()
    setMethod(f, c(rep("ANY", whichArg - 1L), inputClass, remainingSignature),
              def, where)
}

setAtomicListMethod <- function(f,
                                inputBaseClass = "AtomicList",
                                outputBaseClass,
                                whichArg = 1L,
                                remainingSignature = character(),
                                mapply = FALSE,
                                endoapply = FALSE,
                                applyToUnlist = FALSE,
                                addRleList = TRUE,
                                rleListOutputBaseClass = "RleList",
                                where = topenv(parent.frame()))
{
    if (missing(outputBaseClass)) {
        for (i in inputBaseClass)
            setListMethod(f, i, whichArg = whichArg,
                          remainingSignature = remainingSignature,
                          endoapply = endoapply, where = where)
    } else if (endoapply) {
        setListMethod(f, "AtomicList", whichArg = whichArg,
                      remainingSignature = remainingSignature, endoapply = TRUE,
                      where = where)
    } else {
        setListMethod(f, paste("Simple", inputBaseClass, sep = ""),
                      outputBaseClass = outputBaseClass, whichArg = whichArg,
                      remainingSignature = remainingSignature, mapply = mapply,
                      where = where)
        setListMethod(f, paste("Compressed", inputBaseClass, sep = ""),
                      outputBaseClass = outputBaseClass, whichArg = whichArg,
                      remainingSignature, mapply = mapply,
                      applyToUnlist = applyToUnlist, where = where)
        if (addRleList) {
            setListMethod(f, "SimpleRleList",
                          outputBaseClass = rleListOutputBaseClass,
                          whichArg = whichArg,
                          remainingSignature = remainingSignature,
                          mapply = mapply, where = where)
            setListMethod(f, "CompressedRleList",
                          outputBaseClass = rleListOutputBaseClass,
                          whichArg = whichArg,
                          remainingSignature = remainingSignature,
                          mapply = mapply, applyToUnlist = applyToUnlist,
                          where = where)
        }
    }
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### General methods
###

setAtomicListMethod("%in%", outputBaseClass = "LogicalList",
                    remainingSignature = "atomic")
setAtomicListMethod("%in%", outputBaseClass = "LogicalList",
                    remainingSignature = "AtomicList", mapply = TRUE)
setAtomicListMethod("is.na", outputBaseClass = "LogicalList",
                    applyToUnlist = TRUE)
setAtomicListMethod("match", outputBaseClass = "IntegerList",
                    remainingSignature = "atomic")
setAtomicListMethod("match", outputBaseClass = "IntegerList",
                    remainingSignature = "AtomicList", mapply = TRUE)
setAtomicListMethod("sort", endoapply = TRUE)
setAtomicListMethod("order", outputBaseClass = "IntegerList")
setMethod("table", "SimpleAtomicList",
          function(...)
          {
              args <- list(...)
              if (length(args) > 1)
                  stop("Only one argument in '...' supported")
              x <- args[[1L]]
              values <-
                as.character(sort(unique(unlist(lapply(x, unique),
                                                use.names=FALSE))))
              zeros <- structure(rep.int(0L, length(values)), names = values)
              if (is.null(names(x)))
                  names(x) <- as.character(seq_len(length(x)))
              structure(do.call(rbind,
                                lapply(x, function(elt) {
                                           eltTable <- table(elt)
                                           out <- zeros
                                           out[names(eltTable)] <- eltTable
                                           out
                                       })), class = "table")
          })
setMethod("table", "CompressedAtomicList",
          function(...)
          {
              args <- list(...)
              if (length(args) > 1)
                  stop("Only one argument in '...' supported")
              x <- args[[1L]]
              nms <- names(x)
              if (is.null(nms)) {
                  nms <- as.character(seq_len(length(x)))
              }
              nms <- factor(rep.int(nms, elementLengths(x)), levels = nms)
              ans <- table(nms, as.vector(unlist(x, use.names = FALSE)))
              names(dimnames(ans)) <- NULL
              ans
          })
setAtomicListMethod("unique", endoapply = TRUE)
setMethod("unique", "CompressedRleList",
          function(x, incomparables = FALSE, ...)
          {
              if (is.factor(runValue(x@unlistData)))
                  runValue(x@unlistData) <- as.character(runValue(x@unlistData))
              CompressedAtomicListFromList(lapply(x, unique,
                                                  incomparables = incomparables,
                                                  ...))
          })
setMethod("unique", "SimpleRleList",
          function(x, incomparables = FALSE, ...)
              SimpleAtomicList(lapply(x,
                                      function(y) {
                                          if (is.factor(runValue(y)))
                                              runValue(y) <-
                                                as.character(runValue(y))
                                          unique(y,
                                                 incomparables = incomparables,
                                                 ...)
                                      })))

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Logical methods
###

setAtomicListMethod("!", inputBaseClass = "LogicalList", 
                    outputBaseClass = "LogicalList", applyToUnlist = TRUE)
setAtomicListMethod("which", inputBaseClass = "LogicalList",
                    outputBaseClass = "IntegerList",
                    rleListOutputBaseClass = "IntegerList")

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Numerical methods
###

for (i in c("IntegerList", "NumericList", "RleList")) {
    setAtomicListMethod("diff", inputBaseClass = i, endoapply = TRUE)
    setMethod("pmax", i, function(..., na.rm = FALSE)
                  mendoapply(pmax, ..., MoreArgs = list(na.rm = na.rm)))
    setMethod("pmin", i, function(..., na.rm = FALSE)
                  mendoapply(pmin, ..., MoreArgs = list(na.rm = na.rm)))
    setMethod("pmax.int", i, function(..., na.rm = FALSE)
                  mendoapply(pmax.int, ..., MoreArgs = list(na.rm = na.rm)))
    setMethod("pmin.int", i, function(..., na.rm = FALSE)
                  mendoapply(pmin.int, ..., MoreArgs = list(na.rm = na.rm)))
}
for (i in c("LogicalList", "IntegerList", "NumericList", "RleList")) {
    setAtomicListMethod("mean", inputBaseClass = i)
    setMethod("var", signature = c(x = i, y = "missing"),
              function(x, y = NULL, na.rm = FALSE, use)
              {
                  if (missing(use))
                      use <- ifelse(na.rm, "na.or.complete", "everything")
                  sapply(x, var, na.rm = na.rm, use = use, simplify = TRUE)
              })
    setMethod("var", signature = c(x = i, y = "AtomicList"),
              function(x, y = NULL, na.rm = FALSE, use)
              {
                  if (missing(use))
                      use <- ifelse(na.rm, "na.or.complete", "everything")
                  mapply(var, x, y, MoreArgs = list(na.rm = na.rm, use = use),
                         SIMPLIFY = TRUE)
              })
    setMethod("cov", signature = c(x = i, y = "AtomicList"),
              function(x, y = NULL, use = "everything",
                       method = c("pearson", "kendall", "spearman"))
                  mapply(cov, x, y,
                         MoreArgs = list(use = use, method = match.arg(method)),
                         SIMPLIFY = TRUE))
    setMethod("cor", signature = c(x = i, y = "AtomicList"),
              function(x, y = NULL, use = "everything",
                       method = c("pearson", "kendall", "spearman"))
                  mapply(cor, x, y,
                         MoreArgs = list(use = use, method = match.arg(method)),
                         SIMPLIFY = TRUE))
    setAtomicListMethod("sd", inputBaseClass = i)
    setAtomicListMethod("median", inputBaseClass = i)
    setAtomicListMethod("quantile", inputBaseClass = i)
    setMethod("mad", i,
              function(x, center = median(x), constant = 1.4826, na.rm = FALSE, 
                       low = FALSE, high = FALSE)
              {
                  if (!missing(center))
                      stop("'center' argument is not supported")
                  sapply(x, mad, constant = constant, na.rm = na.rm,
                         low = low, high = high, simplify = TRUE)
              })
    setAtomicListMethod("IQR", inputBaseClass = i)
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Running window statistic methods
###

setAtomicListMethod("smoothEnds", inputBaseClass = "IntegerList",
                    outputBaseClass = "NumericList",
                    addRleList = FALSE)
setAtomicListMethod("smoothEnds", inputBaseClass = "NumericList",
                    endoapply = TRUE)
setAtomicListMethod("smoothEnds", inputBaseClass = "RleList",
                    endoapply = TRUE)
setMethod("runmed", "CompressedIntegerList",
          function(x, k, endrule = c("median", "keep", "constant"),
                   algorithm = NULL, print.level = 0)
              NumericList(lapply(x, runmed, k = k, endrule = match.arg(endrule),
                                 algorithm = algorithm,
                                 print.level = print.level)))
setMethod("runmed", "SimpleIntegerList",
          function(x, k, endrule = c("median", "keep", "constant"),
                   algorithm = NULL, print.level = 0)
              NumericList(lapply(x, runmed, k = k, endrule = match.arg(endrule),
                                 algorithm = algorithm,
                                 print.level = print.level), compress = FALSE))
setMethod("runmed", "NumericList",
          function(x, k, endrule = c("median", "keep", "constant"),
                   algorithm = NULL, print.level = 0)
              endoapply(x, runmed, k = k, endrule = match.arg(endrule),
                        algorithm = algorithm, print.level = print.level))
setMethod("runmed", "RleList",
          function(x, k, endrule = c("median", "keep", "constant"),
                   algorithm = NULL, print.level = 0)
              endoapply(x, runmed, k = k, endrule = match.arg(endrule)))
setMethod("runmean", "RleList",
          function(x, k, endrule = c("drop", "constant"))
              endoapply(x, runmean, k = k, endrule = match.arg(endrule)))
setMethod("runsum", "RleList",
          function(x, k, endrule = c("drop", "constant"))
              endoapply(x, runsum, k = k, endrule = match.arg(endrule)))
setMethod("runwtsum", "RleList",
          function(x, k, wt, endrule = c("drop", "constant"))
              endoapply(x, runwtsum, k = k, wt = wt,
                        endrule = match.arg(endrule)))
setMethod("runq", "RleList",
          function(x, k, i, endrule = c("drop", "constant"))
              endoapply(x, runq, k = k, i = i, endrule = match.arg(endrule)))

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Character
###

setAtomicListMethod("nchar", inputBaseClass = "CharacterList",
                    outputBaseClass = "IntegerList", applyToUnlist = TRUE)
## need vectorized start, end
##setAtomicListMethod("substr")
##setAtomicListMethod("substring")
setAtomicListMethod("chartr", inputBaseClass = "CharacterList",
                    outputBaseClass = "CharacterList", whichArg = 3L,
                    applyToUnlist = TRUE)
setAtomicListMethod("tolower", inputBaseClass = "CharacterList",
                    outputBaseClass = "CharacterList",  applyToUnlist = TRUE)
setAtomicListMethod("toupper", inputBaseClass = "CharacterList",
                    outputBaseClass = "CharacterList", applyToUnlist = TRUE)
setAtomicListMethod("sub", inputBaseClass = "CharacterList",
                    outputBaseClass = "CharacterList", whichArg = 3L,
                    applyToUnlist = TRUE)
setAtomicListMethod("gsub", inputBaseClass = "CharacterList",
                    outputBaseClass = "CharacterList", whichArg = 3L,
                    applyToUnlist = TRUE)
### TODO: grep, grepl

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "show" method.
###

setMethod("show", "AtomicList",
          function(object) {
              lo <- length(object)
              k <- min(10, length(object))
              diffK <- lo - 10
              cat(class(object), " of length ", lo, "\n", sep = "")
              for (i in seq_len(k)) {
                  nm <- names(object)[i]
                  if (length(nm) > 0 && nchar(nm) > 0)
                      label <- paste("[[\"", nm, "\"]]", sep = "")
                  else
                      label <- paste("[[", i, "]]", sep = "")
                  if (length(object[[i]]) == 0) {
                      cat(label, " ", sep = "")
                      print(object[[i]])
                  } else {
                      cat(labeledLine(label, object[[i]], labelSep = "",
                                      count = FALSE))
                  }
              }
              if (diffK > 0)
                  cat("...\n<", diffK,
                      ifelse(diffK == 1,
                             " more element>\n", " more elements>\n"),
                      sep="")
          })

setMethod("show", "RleList",
          function(object) {
              lo <- length(object)
              k <- min(5, length(object))
              diffK <- lo - 5
              cat(class(object), " of length ", lo, "\n", sep = "")
              show(as.list(head(object, k)))
              if (diffK > 0)
                  cat("...\n<", diffK,
                      ifelse(diffK == 1,
                             " more element>\n", " more elements>\n"),
                      sep="")
          })

setMethod("showAsCell", "AtomicList",
          function(object) {
              unlist(lapply(object, function(x)
                                if (length(x) <= 3)
                                    paste(x, collapse = ",")
                                else
                                    paste(c(as.vector(head(x, 3)), "..."),
                                          collapse = ",")), use.names = FALSE)
          })
