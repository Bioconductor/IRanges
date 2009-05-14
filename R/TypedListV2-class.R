### =========================================================================
### TypedListV2 objects
### -------------------------------------------------------------------------

## Wrapper around a list that ensures all elements extend from a certain type
setClassUnion("ANYTHING", methods:::.BasicClasses)

setClass("TypedListV2",
         contains="ListLike",
         representation(
                        "VIRTUAL",
                        elementType="character"
                        ),
         prototype(elementType="ANYTHING")
         )

setClass("CompressedTypedList",
         contains="TypedListV2",
         representation(
                        "VIRTUAL",
                        partitioning="PartitioningByEnd",
                        unlistData="ANYTHING"
                       )
         )

setClass("SimpleTypedList",
         contains=c("TypedListV2"),
         representation(
                        "VIRTUAL",
                        listData="list"
                        )
         )

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Accessor methods.
###

setGeneric("elementType", function(x, ...) standardGeneric("elementType"))
setMethod("elementType", "TypedListV2", function(x) x@elementType)

setGeneric("elementLengths", function(x) standardGeneric("elementLengths"))
setMethod("elementLengths", "CompressedTypedList",
          function(x) width(x@partitioning))
setMethod("elementLengths", "SimpleTypedList",
          function(x) {
              listData <- as.list(x)
              if (length(listData) == 0) {
                  integer(0)
              } else if (length(dim(listData[[1]])) < 2) {
                  unlist(lapply(listData, length))
              } else {
                  unlist(lapply(listData, nrow))
              }
          })

setMethod("length", "CompressedTypedList", function(x) length(x@partitioning))
setMethod("length", "SimpleTypedList", function(x) length(as.list(x)))

setMethod("names", "CompressedTypedList", function(x) names(x@partitioning))
setMethod("names", "SimpleTypedList", function(x) names(as.list(x)))

setReplaceMethod("names", "CompressedTypedList",
                 function(x, value)
                 {
                     partitions <- x@partitioning
                     names(partitions) <- value
                     slot(x, "partitioning") <- partitions
                     x
                 })
setReplaceMethod("names", "SimpleTypedList",
                 function(x, value) {
                     listData <- as.list(x)
                     names(listData) <- value
                     slot(x, "listData") <- listData
                     x
                 })

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Constructor.
###

.TypedListV2.compress.list <- function(x) {
    if (length(x) > 0) {
        if (length(dim(x[[1]])) < 2) {
            x <- do.call(c, unname(x))
        } else {
            x <- do.call(rbind, unname(x))
        }
    }
    x
}

CompressedTypedList <- function(listClass, unlistData, end=NULL, NAMES=NULL,
                                splitFactor=NULL, ...)
{
    if (!extends(listClass, "CompressedTypedList"))
        stop("cannot create a ", listClass, " as a 'CompressedTypedList'")
    elementTypeData <- elementType(new(listClass))
    if (is.list(unlistData)) {
        if (missing(NAMES))
            NAMES <- names(unlistData)
        if (length(unlistData) == 0) {
            end <- integer(0)
            unlistData <- new(elementTypeData)
        } else {
            if (length(dim(unlistData[[1]])) < 2) {
                end <-
                  cumsum(unlist(lapply(unlistData, length), use.names = FALSE))
            } else {
                end <-
                  cumsum(unlist(lapply(unlistData, nrow), use.names = FALSE))
            }
            unlistData <-
              .TypedListV2.compress.list(lapply(unlistData, as,
                                                elementTypeData))
        }
    } else if (!extends(class(unlistData), elementTypeData)) {
        stop("'unlistData' not of class 'elementType'")
    } else if (!is.null(splitFactor)) {
        if (is.unsorted(splitFactor)) {
            orderElts <- order(splitFactor)
            if (length(dim(unlistData)) < 2)
                unlistData <- unlistData[orderElts]
            else
                unlistData <- unlistData[orderElts, , drop = FALSE]
            splitFactor <- splitFactor[orderElts]
        }
        splitRle <- Rle(splitFactor)
        NAMES <- as.character(runValue(splitRle))
        end <- cumsum(runLength(splitRle))
    }

    new(listClass, unlistData = unlistData,
        partitioning = new("PartitioningByEnd", end = end, NAMES = NAMES),
        ...)
}

SimpleTypedList <- function(listClass, listData, ...) {
    if (!is.list(listData))
        stop("'listData' must be a list object")
    if (!extends(listClass, "SimpleTypedList"))
        stop("cannot create a ", listClass, " as a 'SimpleTypedList'")
    elementTypeData <- elementType(new(listClass))
    if (!all(unlist(lapply(listData, is, elementTypeData))))
        stop("all elements in 'listData' must be ", elementTypeData, " objects")
    new(listClass, listData = listData, ...)
}

TypedListV2 <- function(listClass, ...) {
    if (extends(listClass, "CompressedTypedList"))
        CompressedTypedList(listClass, ...)
    else if (extends(listClass, "SimpleTypedList"))
        SimpleTypedList(listClass, ...)
    else
        stop("unknown 'TypedListV2' class")
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Validity.
###

.valid.CompressedTypedList.partitioning <- function(x)
{
    if (length(dim(x@unlistData)) < 2) {
        objsize <- length(x@unlistData)
    } else {
        objsize <- nrow(x@unlistData)
    }
    if (nobj(x@partitioning) != objsize)
        "improper partitioning"
    else NULL
}
.valid.CompressedTypedList.unlistData <- function(x)
{
    elementTypeX <- elementType(x)
    if (!extends(class(x@unlistData), elementTypeX))
        paste("the 'unlistData' slot must be of class", elementTypeX)
    else NULL
}
.valid.CompressedTypedList <- function(x)
{
    c(.valid.CompressedTypedList.unlistData(x),
      .valid.CompressedTypedList.partitioning(x))
}
setValidity2("CompressedTypedList", .valid.CompressedTypedList)

.valid.SimpleTypedList.listData <- function(x)
{
    elementTypeX <- elementType(x)
    if (!all(sapply(as.list(x),
                    function(xi) extends(class(xi), elementTypeX))))
        return(paste("the 'listData' slot must be a list containing",
                     elementTypeX, "objects"))
    NULL
}
.valid.SimpleTypedList <- function(x)
{
    c(.valid.SimpleTypedList.listData(x))
}
setValidity2("SimpleTypedList", .valid.SimpleTypedList)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Subsetting.
###

.CompressedTypedList.list.subscript <-
function(X, INDEX, USE.NAMES = TRUE, COMPRESS = missing(FUN), FUN = identity,
         ...) {
    k <- length(INDEX)
    if (k == 0) {
        if (length(dim(X@unlistData)) < 2)
            elts <- X@unlistData[integer(0)]
        else
            elts <- X@unlistData[integer(0), , drop = FALSE]
    } else {
        if (COMPRESS) {
            runStarts <- which(c(TRUE, diff(INDEX) != 1L))
            whichToLoop <- seq_len(length(runStarts))
            startIndices <- INDEX[runStarts]
            endIndices <- startIndices + (diff(c(runStarts, k+1L)) - 1L)
        } else {
            runStarts <- seq_len(k)
            whichToLoop <- which(elementLengths(X)[INDEX] > 0)
            startIndices <- INDEX[runStarts[whichToLoop]]
            endIndices <- startIndices
        }
        if (length(dim(X@unlistData)) < 2)
            zeroLengthElt <- FUN(X@unlistData[integer(0)], ...)
        else
            zeroLengthElt <- FUN(X@unlistData[integer(0), , drop = FALSE], ...)
        elts <- rep(list(zeroLengthElt), length(runStarts))
        prelimLoopCount <- length(whichToLoop)
        if (prelimLoopCount > 0) {
            elementCumLengths <- cumsum(subseq(elementLengths(X), 1L, max(INDEX)))
            allData <- X@unlistData
            eltStarts <- rep.int(1L, prelimLoopCount)
            offsetStart <- startIndices > 1L
            eltStarts[offsetStart] <-
            elementCumLengths[startIndices[offsetStart] - 1L] + 1L
            eltEnds <- elementCumLengths[endIndices]
            okToLoop <- eltStarts <= eltEnds
            loopCount <- sum(okToLoop)
            if (loopCount > 0) {
                if (loopCount < prelimLoopCount) {
                    eltStarts <- eltStarts[okToLoop]
                    eltEnds <- eltEnds[okToLoop]
                    whichToLoop <- whichToLoop[okToLoop]
                }
                if (is.vector(allData)) {
                    eltWidths <- eltEnds - eltStarts + 1L
                    elts[whichToLoop] <-
                      lapply(seq_len(loopCount), function(j)
                             FUN(.Call("vector_subsetbyranges", allData, eltStarts[j], eltWidths[j],
                                       PACKAGE="IRanges"), ...))
                } else if (length(dim(allData)) < 2) {
                    elts[whichToLoop] <-
                      lapply(seq_len(loopCount), function(j)
                             FUN(allData[eltStarts[j]:eltEnds[j]], ...))
                } else {
                    elts[whichToLoop] <-
                      lapply(seq_len(loopCount), function(j)
                             FUN(allData[eltStarts[j]:eltEnds[j], , drop = FALSE], ...))
                }
            }
        }
        if (COMPRESS) {
            elts <- .TypedListV2.compress.list(elts)
        } else if (USE.NAMES) {
            names(elts) <- names(X)[INDEX]
        }
    }
    elts
}

### Extract the i-th element of a TypedList object.
### Supported 'i' types: numeric vector of length 1.
setGeneric("getElement", function(x, i) standardGeneric("getElement"))
setMethod("getElement", "CompressedTypedList", function(x, i)
          .CompressedTypedList.list.subscript(X = x, INDEX = i,
                                              USE.NAMES = FALSE))
setMethod("getElement", "SimpleTypedList", function(x, i) as.list(x)[[i]])

setMethod("[[", "TypedListV2",
          function(x, i, j, ...)
          {
              index <- try(callNextMethod(), silent = TRUE)
              if (inherits(index, "try-error")) {
                  if (length(i) == 1 && (is.na(i) || is.character(i)))
                      ans <- NULL
                  else
                      stop(index)
              } else {
                  ans <- getElement(x, index)
              }
              ans
          })

setMethod("$", "TypedListV2", function(x, name) x[[name]])

setReplaceMethod("[[", "CompressedTypedList",
                 function(x, i, j,..., value)
                 {
                     if (!missing(j) || length(list(...)) > 0)
                         warning("arguments beyond 'i' ignored")
                     if (missing(i))
                         stop("subscript is missing")
                     if (!is.character(i) && !is.numeric(i))
                         stop("invalid subscript type")
                     if (length(i) < 1L)
                         stop("attempt to select less than one element")
                     if (length(i) > 1L)
                         stop("attempt to select more than one element")
                     if (is.numeric(i) && (i < 1L || i > length(x)+1))
                         stop("subscript out of bounds")
                     if (is.character(i)) {
                         nameValue <- i
                         i <- match(i, names(x))
                         if (is.na(i))
                             i <- length(x) + 1L
                     } else {
                         nameValue <- ""
                     }
                     if (is.null(value)) {
                         if (i <= length(x)) # if name did not exist, could be +1
                             x <- x[-i]
                     } else {
                         value <- try(as(value, elementType(x)), silent = TRUE)
                         if (inherits(value, "try-error"))
                             stop("cannot coerce 'value' to a ", elementType(x), " instance")
                         listData <- as.list(x, use.names = FALSE)
                         listData[[i]] <- value
                         widths <- elementLengths(x)
                         widths[i] <-
                           ifelse(length(dim(value)) < 2, length(value), nrow(value))
                         if ((i == length(x) + 1L) &&
                             (!is.null(names(x)) || nchar(nameValue) > 0)) {
                             NAMES <- names(x)
                             if (is.null(NAMES))
                                 NAMES <- rep("", length(x))
                             NAMES[i] <- nameValue
                         } else {
                             NAMES <- names(x)
                         }
                         slot(x, "unlistData", check=FALSE) <-
                           .TypedListV2.compress.list(listData)
                         slot(x, "partitioning", check=FALSE) <-
                           new("PartitioningByEnd", end = cumsum(widths),
                               NAMES = NAMES)
                         x
                     }
                 })
         
setReplaceMethod("[[", "SimpleTypedList",
                 function(x, i, j,..., value)
                 {
                     listData <- as.list(x)
                     listData[[i]] <- value
                     slot(x, "listData") <- listData
                     x
                 })

setReplaceMethod("$", "TypedListV2",
                 function(x, name, value) {
                     x[[name]] <- value
                     x
                 })

### Supported 'i' types: numeric, character, logical, NULL and missing.
setMethod("[", "CompressedTypedList",
          function(x, i, j, ..., drop)
          {
              if (!missing(j) || length(list(...)) > 0)
                  stop("invalid subsetting")
              if (missing(i))
                  return(x)
              if (!is.atomic(i))
                  stop("invalid subscript type")
              lx <- length(x)
              nullOK <- extends("NULL", elementType(x))
              if (is.numeric(i)) {
                  if (any(is.na(i)) && !nullOK)
                      stop("cannot subset by NA (NULL elements not allowed)")
                  nna <- i[!is.na(i)]
                  if (any(nna < -lx) || any(nna > lx))
                      stop("subscript out of bounds")
                  if (any(nna < 0)) {
                      if (any(nna > 0))
                          stop("negative and positive indices cannot be mixed")
                      if (length(nna) < length(i))
                          stop("NAs cannot be mixed with negative subscripts")
                      i <- seq_len(lx)[nna]
                  }
              } else if (is.logical(i)) {
                  if (length(i) > lx && !nullOK)
                      stop("subscript out of bounds (and NULL elements not allowed)")
                  i <- which(rep(i, length.out = lx))
              } else if (is.character(i)) {
                  i <- match(i, names(x))
                  if (any(is.na(i)))
                      stop("mismatching names (and NULL elements not allowed)")
              } else if (is.null(i)) {
                  i <- integer(0)
              } else {
                  stop("invalid subscript type")
              }
              slot(x, "unlistData", check=FALSE) <-
                .CompressedTypedList.list.subscript(X = x, INDEX = i,
                                                    USE.NAMES = FALSE)
              slot(x, "partitioning", check=FALSE) <- 
                new("PartitioningByEnd",
                    end = cumsum(elementLengths(x)[i]),
                    NAMES = names(x)[i])
              x
          })

setMethod("[", "SimpleTypedList",
          function(x, i, j, ..., drop)
          {
              if (!missing(j) || length(list(...)) > 0)
                  stop("invalid subsetting")
              if (!missing(i)) {
                  slot(x, "listData", check=FALSE) <- as.list(x)[i]
              }
              x
          })

setReplaceMethod("[", "TypedListV2",
                 function(x, i, j,..., value)
                 stop("attempt to modify the value of a ", class(x),
                      " instance")
                 )

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Combining and splitting.
###

setMethod("append", c("CompressedTypedList", "CompressedTypedList"),
          function(x, values, after=length(x)) {
              if (!isSingleNumber(after))
                  stop("'after' must be a single number")
              if (!extends(elementType(x), elementType(values)))
                  stop("the element class of 'values' must extend that of 'x'")
              CompressedTypedList(class(x),
                                  append(as.list(x, use.names = TRUE),
                                  as.list(values, use.names = TRUE),
                                  after = after))
          })

setMethod("append", c("SimpleTypedList", "SimpleTypedList"),
          function(x, values, after=length(x)) {
              slot(x, "listData") <-
                append(as.list(x), as.list(values), after=after)
              x
          })

## NOTE: while the 'c' function does not have an 'x', the generic does
## c() is a primitive, so 'x' can be missing; dispatch is by position,
## although sometimes this does not work so well, so it's best to keep
## names off the parameters whenever feasible.

setMethod("c", "CompressedTypedList",
          function(x, ..., recursive = FALSE) {
              if (recursive)
                  stop("'recursive' mode is not supported")
              if (!missing(x))
                  tls <- list(x, ...)
              else
                  tls <- list(...)
              if (!all(sapply(tls, is, "CompressedTypedList")))
                  stop("all arguments in '...' must be CompressedTypedList objects")
              ecs <- sapply(tls, elementType)
              if (!all(sapply(ecs, extends, ecs[[1]])))
                  stop("all arguments in '...' must have an element class that extends ",
                       "that of the first argument")
              elts <-
                unlist(lapply(tls,
                       function(x) {
                           elts <- as.list(x)
                           names(elts) <- names(x)
                           elts
                       }), recursive = FALSE)
              CompressedTypedList(class(tls[[1]]), elts)
          })

setMethod("c", "SimpleTypedList",
          function(x, ..., recursive = FALSE) {
              slot(x, "listData") <- do.call("c", lapply(list(x, ...), as.list))
              x
          })

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Looping.
###

setMethod("lapply", "CompressedTypedList",
          function(X, FUN, ...)
          {
              .CompressedTypedList.list.subscript(X = X,
                                                  INDEX = seq_len(length(X)),
                                                  USE.NAMES = TRUE,
                                                  FUN = match.fun(FUN), ...)
          })

setMethod("lapply", "SimpleTypedList",
          function(X, FUN, ...)
          {
              lapply(as.list(X), FUN = FUN, ...)
          })

.sapplyDefault <- base::sapply
environment(.sapplyDefault) <- globalenv()
setMethod("sapply", "TypedListV2",
          function(X, FUN, ..., simplify=TRUE, USE.NAMES=TRUE)
          {
              .sapplyDefault(X = X, FUN = FUN, ..., simplify = simplify,
                             USE.NAMES = USE.NAMES)
          })

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion.
###

### From an TypedListV2 object to a normal R list.
setAs("TypedListV2", "list", function(from) as.list(from))

### Subclasses should override this for customized list coercion
setMethod("as.list", "CompressedTypedList",
          function(x, use.names = TRUE) {
              .CompressedTypedList.list.subscript(X = x,
                                                  INDEX = seq_len(length(x)),
                                                  USE.NAMES = use.names,
                                                  COMPRESS = FALSE)
          })

setMethod("as.list", "SimpleTypedList",
          function(x, use.names = TRUE) {
              ans <- x@listData
              if (!use.names)
                  names(ans) <- NULL
              ans
          })

setGeneric("unlistData", function(x) standardGeneric("unlistData"))
setMethod("unlistData", "CompressedTypedList",
          function(x) {
              if (length(x) == 0)
                  NULL
              else
                  x@unlistData
          })
setMethod("unlistData", "SimpleTypedList",
          function(x) {
              if (length(x) == 0)
                  NULL
              else
                  .TypedListV2.compress.list(as.list(x))
          })

setMethod("unlist", "TypedListV2",
          function(x, recursive = TRUE, use.names = TRUE) {
              if (!missing(recursive))
                  warning("'recursive' argument currently ignored")
              ans <- unlistData(x)
              if (!is.null(ans)) {
                  if (length(dim(ans)) < 2 && use.names) {
                      nms <- rep(names(x), elementLengths(x))
                      if (!is.null(nms) && !is.null(names(ans)))
                          nms <- paste(nms, names(ans), sep = ".")
                      else if (is.null(nms))
                          nms <- names(ans)
                      names(ans) <- nms
                  } else {
                      if (!use.names)
                          rownames(ans) <- NULL
                  }
              }
              ans
          })

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "show" method.
###

setMethod("show", "TypedListV2",
          function(object)
          {
            lo <- length(object)
            cat("  A ", class(object), " instance of length ", lo, "\n", sep="")
### TODO: show (some of the) elements here
          })
