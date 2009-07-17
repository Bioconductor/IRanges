### =========================================================================
### CompressedList objects
### -------------------------------------------------------------------------

setClass("CompressedList",
         contains="Sequence",
         representation(
                        "VIRTUAL",
                        partitioning="PartitioningByEnd",
                        unlistData="ANYTHING"
                       )
         )

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Accessor methods.
###

setMethod("elementLengths", "CompressedList",
    function(x)
    {
        ans <- width(x@partitioning)
        names(ans) <- names(x)
        ans
    }
)

### A CompressedList object is considered empty iff all its elements are empty.
setMethod("isEmpty", "CompressedList", function(x) all(elementLengths(x) == 0L))

setMethod("length", "CompressedList", function(x) length(x@partitioning))

setMethod("names", "CompressedList", function(x) names(x@partitioning))

setReplaceMethod("names", "CompressedList",
                 function(x, value)
                 {
                     names(x@partitioning) <- value
                     x
                 })

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Constructor.
###

.compress.list <- function(x) {
    if (length(x) > 0) {
        if (length(dim(x[[1]])) < 2) {
            x <- do.call(c, unname(x))
        } else {
            x <- do.call(rbind, unname(x))
        }
    }
    x
}

newCompressedList <- function(listClass, unlistData, end=NULL, NAMES=NULL,
                              splitFactor=NULL, drop=FALSE, ...)
{
    if (!extends(listClass, "CompressedList"))
        stop("cannot create a ", listClass, " as a 'CompressedList'")
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
              .compress.list(lapply(unlistData, as, elementTypeData))
        }
    } else if (!extends(class(unlistData), elementTypeData)) {
        stop("'unlistData' not of class ", elementTypeData)
    } else if (!is.null(splitFactor)) {
        if (is.unsorted(splitFactor)) {
            orderElts <- order(splitFactor)
            if (length(dim(unlistData)) < 2)
                unlistData <- unlistData[orderElts]
            else
                unlistData <- unlistData[orderElts, , drop = FALSE]
            splitFactor <- splitFactor[orderElts]
        }
        if (is.factor(splitFactor) && drop)
            splitFactor <- splitFactor[drop = TRUE]
        splitRle <- Rle(splitFactor)
        if (is.factor(splitFactor) && !drop) {
            NAMES <- levels(splitFactor)
            width <- structure(rep(0L, length(NAMES)), names = NAMES)
            width[as.character(runValue(splitRle))] <- runLength(splitRle)
            end <- cumsum(unname(width))
        } else {
            NAMES <- as.character(runValue(splitRle))
            end <- cumsum(runLength(splitRle))
        }
    }

    new2(listClass, unlistData = unlistData,
         partitioning =
         new2("PartitioningByEnd", end = end, NAMES = NAMES, check=FALSE),
         ..., check=FALSE)
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Validity.
###

.valid.CompressedList.partitioning <- function(x)
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
.valid.CompressedList.unlistData <- function(x)
{
    elementTypeX <- elementType(x)
    if (!extends(class(x@unlistData), elementTypeX))
        paste("the 'unlistData' slot must be of class", elementTypeX)
    else NULL
}
.valid.CompressedList <- function(x)
{
    c(.valid.CompressedList.unlistData(x),
      .valid.CompressedList.partitioning(x))
}
setValidity2("CompressedList", .valid.CompressedList)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Subsetting.
###

.CompressedList.list.subscript <-
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
            elementCumLengths <- cumsum(window(elementLengths(X), 1L, max(INDEX)))
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
            elts <- .compress.list(elts)
        } else if (USE.NAMES) {
            names(elts) <- names(X)[INDEX]
        }
    }
    elts
}

setMethod("[[", "CompressedList",
          function(x, i, j, ...) {
              dotArgs <- list(...)
              if (length(dotArgs) > 0)
                  dotArgs <- dotArgs[names(dotArgs) != "exact"]
              if (!missing(j) || length(dotArgs) > 0)
                  stop("incorrect number of subscripts")
              index <-
                try(checkAndTranslateDbleBracketSubscript(x, i), silent = TRUE)
              if (inherits(index, "try-error")) {
                  if (length(i) == 1 && (is.na(i) || is.character(i)))
                      ans <- NULL
                  else
                      stop(index)
              } else {
                  ans <-
                    .CompressedList.list.subscript(X = x, INDEX = index,
                                                   USE.NAMES = FALSE)
              }
              ans
          })

setReplaceMethod("[[", "CompressedList",
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
                         names(widths) <- NULL
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
                           .compress.list(listData)
                         slot(x, "partitioning", check=FALSE) <-
                           new2("PartitioningByEnd", end = cumsum(widths),
                                NAMES = NAMES, check=FALSE)
                         x
                     }
                 })

setReplaceMethod("$", "CompressedList",
                 function(x, name, value) {
                     x[[name]] <- value
                     x
                 })

### Supported 'i' types: numeric, character, logical, NULL and missing.
setMethod("[", "CompressedList",
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
              } else if (is.character(i) || is.factor(i)) {
                  nms <- names(x)
                  if (is.null(nms))
                      stop("cannot subset by character when names are NULL")
                  i <- match(i, nms)
                  if (any(is.na(i)))
                      stop("mismatching names (and NULL elements not allowed)")
              } else if (is.null(i)) {
                  i <- integer(0)
              } else {
                  stop("invalid subscript type")
              }
              ends <- cumsum(elementLengths(x)[i])
              names(ends) <- NULL
              slot(x, "unlistData", check=FALSE) <-
                .CompressedList.list.subscript(X = x, INDEX = i,
                                               USE.NAMES = FALSE)
              slot(x, "partitioning", check=FALSE) <- 
                new2("PartitioningByEnd", end = ends, NAMES = names(x)[i],
                     check=FALSE)
              .bracket.Sequence(x, i)
          })

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Combining and splitting.
###

## NOTE: while the 'c' function does not have an 'x', the generic does
## c() is a primitive, so 'x' can be missing; dispatch is by position,
## although sometimes this does not work so well, so it's best to keep
## names off the parameters whenever feasible.

setMethod("c", "CompressedList",
          function(x, ..., recursive = FALSE) {
              if (recursive)
                  stop("'recursive' mode is not supported")
              if (!missing(x))
                  tls <- list(x, ...)
              else
                  tls <- list(...)
              if (!all(sapply(tls, is, "CompressedList")))
                  stop("all arguments in '...' must be CompressedList objects")
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
              eltmetaX <- elementMetadata(x)
              x <- newCompressedList(class(tls[[1]]), elts)
              slot(x, "elementMetadata", check=FALSE) <- eltmetaX
              .c.Sequence(x, ...)
          })

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Looping.
###

setMethod("lapply", "CompressedList",
          function(X, FUN, ...)
          {
              .CompressedList.list.subscript(X = X,
                                             INDEX = seq_len(length(X)),
                                             USE.NAMES = TRUE,
                                             COMPRESS = FALSE,
                                             FUN = match.fun(FUN), ...)
          })

.updateCompressedList <- function(X, listData) {
    elementTypeX <- elementType(X)
    if (!all(sapply(listData,
                    function(Xi) extends(class(Xi), elementTypeX))))
        stop("'FUN' must return elements of class ", elementTypeX)
    if (length(listData) == 0) {
        end <- integer(0)
    } else {
        if (length(dim(listData[[1]])) < 2) {
            end <- cumsum(unlist(lapply(listData, length), use.names = FALSE))
        } else {
            end <- cumsum(unlist(lapply(listData, nrow), use.names = FALSE))
        }
    }
    initialize(X,
               unlistData = .compress.list(listData),
               partitioning = 
               new2("PartitioningByEnd", end = end, NAMES = names(X),
                    check=FALSE))
}

setMethod("endoapply", "CompressedList",
          function(X, FUN, ...) {
              .updateCompressedList(X,
                                    .CompressedList.list.subscript(X = X,
                                              INDEX = seq_len(length(X)),
                                              USE.NAMES = FALSE,
                                              COMPRESS = FALSE,
                                              FUN = match.fun(FUN), ...))
          })

setMethod("mendoapply", "CompressedList",
          function(FUN, ..., MoreArgs = NULL) {
              .updateCompressedList(list(...)[[1]],
                                    mapply(FUN = FUN, ..., MoreArgs = MoreArgs))
          })

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion.
###

setMethod("as.list", "CompressedList",
          function(x, use.names = TRUE) {
              .CompressedList.list.subscript(X = x,
                                             INDEX = seq_len(length(x)),
                                             USE.NAMES = use.names,
                                             COMPRESS = FALSE)
          })

setMethod("unlist", "CompressedList",
          function(x, recursive = TRUE, use.names = TRUE) {
              if (!missing(recursive))
                  warning("'recursive' argument currently ignored")
              ans <- x@unlistData
              if (length(x) > 0) {
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

setMethod("show", "CompressedList",
          function(object)
          {
              lo <- length(object)
              cat(class(object), ": ", lo,
                  ifelse(lo == 1, " element\n", " elements\n"), sep = "")
              if (!is.null(names(object)))
                  cat(labeledLine("names", names(object)))
          })
