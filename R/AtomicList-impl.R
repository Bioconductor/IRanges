### =========================================================================
### AtomicList object implementations
### -------------------------------------------------------------------------

## Possible optimizations for compressed lists:
## - order/sort: unlist, order by split factor first
## - sum/mean: unlist, rowsum() with split factor
##   - or probably faster would be a rangeSums function
## - cumsum: unlist, cumsum and subtract offsets

.ATOMIC_TYPES <- c("logical", "integer", "numeric", "complex",
                   "character", "raw")
setClassUnion("atomic", .ATOMIC_TYPES)

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

setClass("CompressedFactorList",
         prototype = prototype(elementType = "factor",
           unlistData = factor()),
         contains = c("FactorList", "CompressedAtomicList"))
setClass("SimpleFactorList",
         prototype = prototype(elementType = "factor"),
         contains = c("FactorList", "SimpleAtomicList"))

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Constructors
###

.dotargsAsList <- function(type, ...) {
  listData <- list(...)
  if (length(listData) == 1) {
      arg1 <- listData[[1]]
      if (is.list(arg1) || is(arg1, "List"))
        listData <- arg1
      else if (type == "integer" && class(arg1) == "character")
        listData <- strsplitAsListOfIntegerVectors(arg1) # weird special case
  }
  listData
}

AtomicListConstructor <- function(type, compress.default = TRUE) {
  constructor <- eval(substitute(function(..., compress = compress.default) {
    if (!isTRUEorFALSE(compress))
      stop("'compress' must be TRUE or FALSE")
    listData <- .dotargsAsList(type, ...)
    CompressedOrSimple <- if (compress) "Compressed" else "Simple"
    if (is(listData, S4Vectors:::listClassName(CompressedOrSimple, type)))
      listData
    else CoercerToList(type, compress)(listData)
  }, list(type = type)))
  formals(constructor)$compress <- compress.default
  constructor
}

LogicalList <- AtomicListConstructor("logical")
IntegerList <- AtomicListConstructor("integer")
NumericList <- AtomicListConstructor("numeric")
ComplexList <- AtomicListConstructor("complex")
CharacterList <- AtomicListConstructor("character")
RawList <- AtomicListConstructor("raw")
RleList <- AtomicListConstructor("Rle")
FactorList <- AtomicListConstructor("factor")

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Iteration
###

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
### Coercion
###

setMethod("as.list", "CompressedAtomicList",
          function(x, use.names = TRUE) {
              if (is(x, "CompressedRleList")) {
                  callNextMethod(x, use.names = use.names)
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

setAs("CompressedAtomicList", "list", function(from) as.list(from))

### Equivalent to 'as.vector(as.list(x), mode=mode)' but faster on
### CompressedAtomicList objects (10x, 75x, or more, depending on 'length(x)').
setMethod("as.vector", "AtomicList",
    function(x, mode="any")
    {
        valid_modes <- c("any", .ATOMIC_TYPES, "double", "list")
        mode <- match.arg(mode, valid_modes)
        if (mode %in% c("any", "list"))
            return(as.list(x))
        elt_lens <- elementLengths(x)
        if (any(elt_lens > 1L))
            stop("coercing an AtomicList object to an atomic vector ",
                 "is supported only for\n",
                 "  objects with top-level elements of length <= 1")
        ans <- base::rep.int(as.vector(NA, mode=mode), length(x))
        ans[elt_lens == 1L] <- as.vector(unlist(x, use.names=FALSE), mode=mode)
        ans
    }
)

setMethod("drop", "AtomicList", function(x) {
  lens <- elementLengths(x)
  if (any(lens > 1))
    stop("All element lengths must be <= 1")
  x_dropped <- rep.int(NA, sum(lens))
  x_dropped[lens > 0] <- unlist(x, use.names = FALSE)
  names(x_dropped) <- names(x)
  x_dropped
})

CoercerToList <- function(type, compress) {
  .coerceToList <- if (compress)
                     coerceToCompressedList
                   else
                     S4Vectors:::coerceToSimpleList
  function(from) {
    .coerceToList(from, type)
  }
}

setListCoercions <- function(type) {
  CompressedClass <- S4Vectors:::listClassName("Compressed", type)
  SimpleClass <- S4Vectors:::listClassName("Simple", type)
  Class <- S4Vectors:::listClassName("", type)
  hasCompressedList <- CompressedClass != "CompressedList"
  if (hasCompressedList) {
    setAs("ANY", CompressedClass, CoercerToList(type, compress = TRUE))
  }
  setAs("ANY", SimpleClass, CoercerToList(type, compress = FALSE))
  setAs("ANY", Class, CoercerToList(type, compress = hasCompressedList))
  setAs("SimpleList", Class, CoercerToList(type, compress = FALSE))
  setAs("list", Class, CoercerToList(type, compress = FALSE))
}

setListCoercions("logical")
setListCoercions("integer")
setListCoercions("numeric")
setListCoercions("complex")
setListCoercions("character")
setListCoercions("raw")
setListCoercions("Rle")
setListCoercions("factor")

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Group generic methods
###

emptyOpsReturnValue <- function(.Generic, e1, e2, compress) {
  dummy.vector <- do.call(.Generic,
                          list(vector(e1@elementType), vector(e2@elementType)))
  CoercerToList(NULL, compress)(dummy.vector)
}

recycleList <- function(x, length.out) {
  if (length.out %% length(x) > 0L)
    warning("shorter object is not a multiple of longer object length")
  rep(x, length.out = length.out)
}

setMethod("Ops",
          signature(e1 = "SimpleAtomicList", e2 = "SimpleAtomicList"),
          function(e1, e2)
          {
              if (length(e1) == 0L || length(e2) == 0L) {
                return(emptyOpsReturnValue(.Generic, e1, e2, compress = FALSE))
              }
              n <- max(length(e1), length(e2))
              e1 <- recycleList(e1, n)
              e2 <- recycleList(e2, n)
              as(Map(.Generic, e1, e2), "List")
          })

repLengthOneElements <- function(x, times) {
  x@unlistData <- rep(x@unlistData, times)
  x@partitioning@end <- cumsum(times)
  x
}

recycleListElements <- function(x, newlen) {
  len <- elementLengths(x)
  if (identical(len, newlen)) {
    return(x)
  }
  if (all(len == 1L)) {
    ans <- repLengthOneElements(x, newlen)
  } else {
    ans <- rep(x, newlen / len)
    if (length(unlist(ans, use.names=FALSE)) != sum(newlen)) {
      stop("Some element lengths are not multiples of their corresponding ",
           "element length in ", deparse(substitute(x)))
    }
  }
  ans
}

doBinaryCompressedListOp <- function(OP, e1, e2, skeleton) {
  if (!missing(skeleton)) {
    n <- length(skeleton)
  } else {
    n <- max(length(e1), length(e2))
  }
  e1 <- recycleList(e1, n)
  e2 <- recycleList(e2, n)
  if (missing(skeleton)) {
    n1 <- elementLengths(e1)
    n2 <- elementLengths(e2)
    if (any(n1 != n2)) {
      en <- ifelse(n1 == 0L | n2 == 0L, 0L, pmax.int(n1, n2))
    } else {
      en <- NULL
    }
    nms <- names(e1)
    if (is.null(nms))
      nms <- names(e2)
  } else {
    en <- elementLengths(skeleton)
    nms <- names(skeleton)
  }
  if (!is.null(en)) {
    e1 <- recycleListElements(e1, en)
    e2 <- recycleListElements(e2, en)
  }
  partitioning <- PartitioningByEnd(e1)  
  names(partitioning) <- nms
  relist(OP(unlist(e1, use.names=FALSE), unlist(e2, use.names=FALSE)),
         partitioning)
}

setMethod("Ops",
          signature(e1 = "CompressedAtomicList", e2 = "CompressedAtomicList"),
          function(e1, e2)
          {
              if (length(e1) == 0L || length(e2) == 0L) {
                return(emptyOpsReturnValue(.Generic, e1, e2, compress = TRUE))
              }
              doBinaryCompressedListOp(function(x, y) {
                .Generic <- .Generic
                callGeneric(x, y)
              }, e1, e2)
          })

setMethod("Ops",
          signature(e1 = "SimpleAtomicList", e2 = "CompressedAtomicList"),
          function(e1, e2)
          {
              if (sum(as.numeric(elementLengths(e1))) < .Machine$integer.max)
                  e1 <- as(e1, "CompressedList")
              else
                  e2 <- as(e2, "SimpleList")
              callGeneric(e1, e2)
          })


setMethod("Ops",
          signature(e1 = "CompressedAtomicList", e2 = "SimpleAtomicList"),
          function(e1, e2)
          {
              if (sum(as.numeric(elementLengths(e2))) < .Machine$integer.max)
                  e2 <- as(e2, "CompressedList")
              else
                  e1 <- as(e1, "SimpleList")
              callGeneric(e1, e2)
          })

setMethod("Ops",
          signature(e1 = "AtomicList", e2 = "atomic"),
          function(e1, e2)
          {
              e2 <- as(e2, "List")
              callGeneric(e1, e2)
          })

setMethod("Ops",
          signature(e1 = "atomic", e2 = "AtomicList"),
          function(e1, e2)
          {
              e1 <- as(e1, "List")
              callGeneric(e1, e2)
          })

setMethod("Ops",
          signature(e1 = "SimpleAtomicList", e2 = "atomic"),
          function(e1, e2)
          {
              e2 <- as(e2, "SimpleList")
              callGeneric(e1, e2)
          })

setMethod("Ops",
          signature(e1 = "atomic", e2 = "SimpleAtomicList"),
          function(e1, e2)
          {
              e1 <- as(e1, "SimpleList")
              callGeneric(e1, e2)
          })

setMethod("Ops",
          signature(e1 = "CompressedAtomicList", e2 = "atomic"),
          function(e1, e2)
          {
              if (length(e2) > 1) {
                  e2 <- S4Vectors:::recycleVector(e2, length(e1))
                  e2 <- rep(e2, elementLengths(e1))
              }
              relist(callGeneric(e1@unlistData, e2), e1)
          })

setMethod("Ops",
          signature(e1 = "atomic", e2 = "CompressedAtomicList"),
          function(e1, e2)
          {
              if (length(e1) > 1) {
                  e1 <- S4Vectors:::recycleVector(e1, length(e2))
                  e1 <- rep(e1, elementLengths(e2))
              }
              relist(callGeneric(e1, e2@unlistData), e2)
          })

setMethod("Math", "CompressedAtomicList",
          function(x) {
              relist(callGeneric(x@unlistData), x)
          })

setMethod("cumsum", "CompressedAtomicList",
          function(x) {
              xunlist <- unlist(x, use.names=FALSE)
              xcumsum <- cumsum(as.numeric(xunlist))
              partition <- PartitioningByEnd(x)
              ans <- xcumsum - rep(xcumsum[start(partition)] -
                  xunlist[start(partition)], width(partition))
              relist(ans, x)
          })

setMethod("cumprod", "CompressedAtomicList",
          function(x) {
              as(lapply(lst, .Generic), "CompressedList")
          })

setMethod("cummin", "CompressedAtomicList",
          function(x) {
              as(lapply(lst, .Generic), "CompressedList")
          })

setMethod("cummax", "CompressedAtomicList",
          function(x) {
              as(lapply(lst, .Generic), "CompressedList")
          })

setMethod("Math", "SimpleAtomicList",
          function(x) as(lapply(x@listData, .Generic), "List"))

setMethod("Math2", "CompressedAtomicList",
          function(x, digits)
          {
              if (missing(digits))
                  digits <- ifelse(.Generic == "round", 0, 6)
              relist(callGeneric(x@unlistData, digits = digits), x)
          })

setMethod("Math2", "SimpleAtomicList",
          function(x, digits)
          {
              if (missing(digits))
                  digits <- ifelse(.Generic == "round", 0, 6)
              as(lapply(x@listData, .Generic, digits = digits), "List")
          })

setMethod("Summary", "AtomicList",
          function(x, ..., na.rm = FALSE) {
            sapply(x, .Generic, na.rm = na.rm)
          })

rowsumCompressedList <- function(x, ..., na.rm = FALSE) {
  x_flat <- unlist(x, use.names = FALSE)
  ans <- vector(class(x_flat), length(x))
  non_empty <- elementLengths(x) > 0
  if (is.logical(x_flat))
    x_flat <- as.integer(x_flat)
  ans[non_empty] <- rowsum(x_flat, togroup(x), reorder = FALSE,
                           na.rm = na.rm)[,1]
  setNames(ans, names(x))
}

setMethod("sum", "CompressedNumericList", rowsumCompressedList)
setMethod("sum", "CompressedIntegerList", rowsumCompressedList)
setMethod("sum", "CompressedLogicalList", rowsumCompressedList)

setMethod("Summary", "CompressedRleList",
          function(x, ..., na.rm = FALSE) {
            toViewFun <- list(max = viewMaxs, min = viewMins, sum = viewSums)
            if (!is.null(viewFun <- toViewFun[[.Generic]])) {
              ans <- viewFun(as(x, "RleViews"), na.rm = na.rm)
              names(ans) <- names(x)
              ans
            }
            else sapply(x, .Generic, na.rm = na.rm)
          })

setMethod("all", "CompressedRleList", function(x, ..., na.rm = FALSE) {
  args <- list(...)
  if (length(args) > 0L)
    stop("Only a single argument in '...' is supported for now")
  if (!isTRUEorFALSE(na.rm))
    stop("'na.rm' must be TRUE or FALSE")
  rv <- runValue(x)
  if (na.rm)
    rv <- rv[!is.na(rv)]
  elen <- elementLengths(rv)
  ans <- elen == 0L
  singletons <- elen == 1L
  ans[singletons] <- unlist(rv, use.names = FALSE)[singletons[togroup(rv)]]
  ans
})

setMethod("Complex", "CompressedAtomicList",
          function(z)
              relist(callGeneric(z@unlistData), z))

setMethod("Complex", "SimpleAtomicList",
          function(z) as(lapply(z@listData, .Generic), "List"))

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

### S3/S4 combo for unique.RleList
unique.RleList <- function(x, incomparables=FALSE, ...)
    unique(runValue(x), incomparables=incomparables, ...)
setMethod("unique", "RleList", unique.RleList)

### S3/S4 combo for duplicated.CompressedIntegerList
duplicated.CompressedIntegerList <- function(x, incomparables=FALSE,
                                             fromLast=FALSE)
{
  if (!identical(incomparables, FALSE))
    stop("\"duplicated\" method for CompressedList objects ",
         "does not support the 'incomparables' argument")
  x_unlistData <- x@unlistData
  x_group <- rep.int(seq_along(x), elementLengths(x))
  ans_unlistData <-
    S4Vectors:::duplicatedIntegerPairs(x_group, x_unlistData,
                                       fromLast=fromLast)
  relist(ans_unlistData, x)
}
setMethod("duplicated", "CompressedIntegerList",
          duplicated.CompressedIntegerList)

### Could actually be made the "table" method for List objects. Will work on
### any List object 'x' for which 'as.factor(unlist(x))' works.
setMethod("table", "AtomicList",
    function(...)
    {
        args <- list(...)
        if (length(args) != 1L)
            stop("\"table\" method for AtomicList objects ",
                 "can only take one input object")
        x <- args[[1L]]
        y1 <- togroup(x)
        attributes(y1) <- list(levels=as.character(seq_along(x)),
                               class="factor")
        y2 <- as.factor(unlist(x, use.names=FALSE))
        ans <- table(y1, y2)
        names(dimnames(ans)) <- NULL
        x_names <- names(x)
        if (!is.null(x_names))
            rownames(ans) <- x_names
        ans
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Logical methods
###

setAtomicListMethod("which", inputBaseClass = "LogicalList",
                    outputBaseClass = "IntegerList",
                    rleListOutputBaseClass = "IntegerList")

setMethod("which", "CompressedLogicalList", function(x) {
  x.flat <- unlist(x, use.names = FALSE)
  part <- PartitioningByEnd(x)
  which.global <- which(x.flat)
  group <- findInterval(which.global, start(part))
  which.local <- which.global - start(part)[group] + 1L
  ans <- splitAsList(which.local, factor(group, seq_len(length(x))))
  names(ans) <- names(x)
  ans
})

ifelseReturnValue <- function(yes, no, len) {
  proto <- function(x)
    new(if (is.atomic(x)) class(x) else x@elementType)
  v <- logical()
  v[1L] <- proto(yes)[1L]
  v[1L] <- proto(no)[1L]
  v
  compress <- is(yes, "CompressedList") || is(no, "CompressedList")
  as(rep(v, length.out = len),
    if(compress) "CompressedList" else "SimpleList")
}

setMethods("ifelse", list(c("ANY", "ANY", "List"),
                          c("ANY", "List", "List"),
                          c("ANY", "List", "ANY")),      
           function(test, yes, no) {
             ans <- ifelseReturnValue(yes, no, length(test))
             ok <- !(nas <- is.na(test))
             if (any(test[ok])) 
               ans[test & ok] <- rep(yes, length.out = length(ans))[test & ok]
             if (any(!test[ok])) 
               ans[!test & ok] <- rep(no, length.out = length(ans))[!test & ok]
             ans[nas] <- NA
             names(ans) <- names(test)
             ans
           })

setMethods("ifelse", list(c("CompressedLogicalList", "ANY", "ANY"),
                          c("CompressedLogicalList", "ANY", "List"),
                          c("CompressedLogicalList", "List", "ANY"),
                          c("CompressedLogicalList", "List", "List")),
           function(test, yes, no) {
             doBinaryCompressedListOp(function(yes, no) {
               ifelse(unlist(test, use.names=FALSE), yes, no)
             }, as(yes, "List"), as(no, "List"), test)
           })

setMethods("ifelse", list(c("SimpleLogicalList", "ANY", "ANY"),
                          c("SimpleLogicalList", "ANY", "List"),
                          c("SimpleLogicalList", "List", "ANY"),
                          c("SimpleLogicalList", "List", "List")),
           function(test, yes, no) {
             as(mapply(ifelse, test, yes, no, SIMPLIFY=FALSE), "List")
           })

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Numerical methods
###


for (i in c("IntegerList", "NumericList", "RleList")) {
    setAtomicListMethod("diff", inputBaseClass = i, endoapply = TRUE)
    setAtomicListMethod("which.max", inputBaseClass = i)
    setAtomicListMethod("which.min", inputBaseClass = i)
    setMethod("pmax", i, function(..., na.rm = FALSE)
                  mendoapply(pmax, ..., MoreArgs = list(na.rm = na.rm)))
    setMethod("pmin", i, function(..., na.rm = FALSE)
                  mendoapply(pmin, ..., MoreArgs = list(na.rm = na.rm)))
    setMethod("pmax.int", i, function(..., na.rm = FALSE)
                  mendoapply(pmax.int, ..., MoreArgs = list(na.rm = na.rm)))
    setMethod("pmin.int", i, function(..., na.rm = FALSE)
                  mendoapply(pmin.int, ..., MoreArgs = list(na.rm = na.rm)))
}

setMethod("mean", "AtomicList",
    function(x, ...) sapply(x, mean, ...)
)

setMethod("var", c("AtomicList", "missing"),
    function(x, y=NULL, na.rm=FALSE, use)
    {
        if (missing(use))
            use <- ifelse(na.rm, "na.or.complete", "everything")
        sapply(x, var, na.rm=na.rm, use=use)
    }
)
setMethod("var", c("AtomicList", "AtomicList"),
    function(x, y=NULL, na.rm=FALSE, use)
    {
        if (missing(use))
            use <- ifelse(na.rm, "na.or.complete", "everything")
        mapply(var, x, y, MoreArgs=list(na.rm=na.rm, use=use))
    }
)

setMethod("cov", c("AtomicList", "AtomicList"),
    function(x, y=NULL,
             use="everything", method=c("pearson", "kendall", "spearman"))
        mapply(cov, x, y, MoreArgs=list(use=use, method=match.arg(method)))
)

setMethod("cor", c("AtomicList", "AtomicList"),
    function(x, y=NULL,
             use="everything", method=c("pearson", "kendall", "spearman"))
        mapply(cor, x, y, MoreArgs=list(use=use, method=match.arg(method)))
)

setMethod("sd", "AtomicList",
    function(x, na.rm=FALSE) sapply(x, sd, na.rm=na.rm)
)

setMethod("median", "AtomicList",
    function(x, na.rm=FALSE) sapply(x, median, na.rm=na.rm)
)

setMethod("quantile", "AtomicList",
    function(x, ...) sapply(x, quantile, ...)
)

setMethod("mad", "AtomicList",
    function(x, center=median(x), constant=1.4826, na.rm=FALSE,
                low=FALSE, high=FALSE)
    {
        if (!missing(center))
            stop("'center' argument is not supported")
        sapply(x, mad, constant=constant, na.rm=na.rm, low=low, high=high)
    }
)

setMethod("IQR", "AtomicList",
    function(x, na.rm=FALSE, type=7) sapply(x, IQR, na.rm=na.rm, type=type)
)

setMethod("which.max", "CompressedRleList",
          function(x) {
            viewWhichMaxs(as(x, "RleViews"), na.rm=TRUE) -
              c(0L, head(cumsum(elementLengths(x)), -1))
          })
setMethod("which.min", "CompressedRleList",
          function(x) {
            viewWhichMins(as(x, "RleViews"), na.rm=TRUE) -
              c(0L, head(cumsum(elementLengths(x)), -1))
          })

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
          function(x, k, endrule = c("drop", "constant"), na.rm = FALSE)
              endoapply(x, runmean, k = k, endrule = match.arg(endrule),
                        na.rm = na.rm))
setMethod("runsum", "RleList",
          function(x, k, endrule = c("drop", "constant"), na.rm = FALSE)
              endoapply(x, runsum, k = k, endrule = match.arg(endrule),
                        na.rm = na.rm))
setMethod("runwtsum", "RleList",
          function(x, k, wt, endrule = c("drop", "constant"), na.rm = FALSE)
              endoapply(x, runwtsum, k = k, wt = wt,
                        endrule = match.arg(endrule), na.rm = na.rm))
setMethod("runq", "RleList",
          function(x, k, i, endrule = c("drop", "constant"), na.rm = FALSE)
              endoapply(x, runq, k = k, i = i, endrule = match.arg(endrule),
                        na.rm = na.rm))

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Character
###

nchar_CompressedList <- function(x, type="chars", allowNA=FALSE)
{
        unlisted_x <- unlist(x, use.names=FALSE)
        unlisted_ans <- nchar(unlisted_x, type=type, allowNA=allowNA)
        relist(unlisted_ans, x)
}

setMethod("nchar", "CompressedCharacterList", nchar_CompressedList)
setMethod("nchar", "SimpleCharacterList", nchar_CompressedList)

setMethod("nchar", "CompressedRleList", nchar_CompressedList)
setMethod("nchar", "SimpleRleList", nchar_CompressedList)


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

setMethod("unstrsplit", "CharacterList",
    function(x, sep="") unstrsplit(as.list(x), sep=sep)
)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Rle methods
###

setMethod("runValue", "RleList", function(x) {
  as(lapply(x, runValue), "List")
})

setMethod("runValue", "CompressedRleList",
    function(x)
    {
        rle <- unlist(x, use.names=FALSE)
        rlePart <- PartitioningByWidth(runLength(rle))
        listPart <- PartitioningByEnd(x)
        ## 'rlePart' cannot contain empty ranges so using
        ## Using 'hit.empty.query.ranges=TRUE' won't affect the result
        ## (because 'rlePart' cannot contain empty ranges) but it makes
        ## findOverlaps_Ranges_Partitioning() just a little bit faster.
        hits <- findOverlaps_Ranges_Partitioning(rlePart, listPart,
                                                 hit.empty.query.ranges=TRUE)
        ans_partitioning <- PartitioningByEnd(subjectHits(hits), NG=length(x))
        ans_unlistData <- runValue(rle)[queryHits(hits)]
        ans <- relist(ans_unlistData, ans_partitioning)
        names(ans) <- names(x)
        ans
    }
)

setReplaceMethod("runValue", "CompressedRleList",
                 function(x, value) {
                   if (!identical(elementLengths(ranges(x)),
                                  elementLengths(value)))
                     stop("elementLengths() of 'x' and 'value' must match")
                   runValue(x@unlistData) <- unlist(value, use.names=FALSE)
                   x
                 })

setReplaceMethod("runValue", "SimpleRleList",
                 function(x, value) {
                   if (!identical(elementLengths(ranges(x)),
                                  elementLengths(value)))
                     stop("elementLengths() of 'x' and 'value' must match")
                   x@listData <- mapply(function(rle, v) {
                     runValue(rle) <- v
                     rle
                   }, x, value, SIMPLIFY=FALSE)
                   x
                 })

setMethod("runLength", "RleList", function(x) {
  as(lapply(x, runLength), "IntegerList")
})

setMethod("runLength", "CompressedRleList", function(x) {
  width(ranges(x))
})

setMethod("ranges", "RleList", function(x) {
  as(lapply(x, ranges), "List")
})

diceRangesByList <- function(x, list) {
  listPart <- PartitioningByEnd(list)
  ## 'x' cannot contain empty ranges so using
  ## 'hit.empty.query.ranges=TRUE' won't affect the result but
  ## it makes findOverlaps_Ranges_Partitioning() just a little
  ## bit faster.
  hits <- findOverlaps_Ranges_Partitioning(x, listPart,
                                           hit.empty.query.ranges=TRUE)
  ans_partitioning <- PartitioningByEnd(subjectHits(hits), NG=length(list))
  ans_unlistData <- shift(ranges(hits, x, listPart),
                          1L - start(listPart)[subjectHits(hits)])
  ans <- relist(ans_unlistData, ans_partitioning)
  names(ans) <- names(list)
  ans
}

setMethod("ranges", "CompressedRleList",
    function(x)
    {
      rle <- unlist(x, use.names=FALSE)
      rlePart <- PartitioningByWidth(runLength(rle))
      diceRangesByList(rlePart, x)
    }
)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Factor methods
###

setMethod("levels", "FactorList", function(x) {
  CharacterList(lapply(x, levels))
})

setMethod("levels", "CompressedFactorList", function(x) {
  setNames(rep(CharacterList(levels(x@unlistData)), length(x)), names(x))
})

setMethod("unlist", "SimpleFactorList",
          function(x, recursive = TRUE, use.names = TRUE) {
            levs <- levels(x)
            if (length(x) > 1L &&
                !all(vapply(levs[-1L], identical, logical(1L), levs[[1L]]))) {
              stop("inconsistent level sets")
            }
            structure(callNextMethod(),
                      levels=as.character(levs[[1L]]),
                      class="factor")
          })

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "show" method.
###

.showAtomicList <- function(object, minLines, ...)
{
    len <- length(object)
    k <- min(minLines, len)
    d <- len - minLines 
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
            cat(BiocGenerics:::labeledLine(label, object[[i]], labelSep = "",
                                           count = FALSE))
        }
    }
    if (d > 0)
        cat("...\n<", d,
            ifelse(d == 1,
                   " more element>\n", " more elements>\n"), sep="")
}

setMethod("show", "AtomicList",
          function(object) 
          {
              cat(classNameForDisplay(object), " of length ",
                  length(object), "\n", sep = "")
              .showAtomicList(object, 10) 
          }
)

setMethod("show", "RleList",
          function(object) {
              lo <- length(object)
              k <- min(5, length(object))
              diffK <- lo - 5
              cat(classNameForDisplay(object), " of length ", lo,
                  "\n", sep = "")
              show(as.list(head(object, k)))
              if (diffK > 0)
                  cat("...\n<", diffK,
                      ifelse(diffK == 1,
                             " more element>\n", " more elements>\n"),
                      sep="")
          })

setMethod("showAsCell", "AtomicList",
          function(object) {
              if (length(object) == 0L)
                return(character(0))
              unlist(lapply(object, function(x) {
                str <- paste(as.vector(head(x, 3)), collapse = ",") 
                if (length(x) > 3)
                  str <- paste(str, "...", sep = ",")
                str
              }), use.names = FALSE)
          })

