### =========================================================================
### CompressedList objects
### -------------------------------------------------------------------------

setClass("CompressedList",
         contains="Sequence",
         representation(
                        "VIRTUAL",
                        partitioning="PartitioningByEnd",
                        unlistData="ANY"
                       )
         )

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Accessor methods.
###

setMethod("elementLengths", "CompressedList",
    function(x)
    {
        ans <- elementLengths(x@partitioning)
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
        if (length(dim(x[[1L]])) < 2) {
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
            if (length(dim(unlistData[[1L]])) < 2) {
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
        if (!is(splitFactor, "Rle"))
            splitFactor <- Rle(splitFactor)
        if (is.unsorted(splitFactor)) {
            orderElts <- order(as.vector(splitFactor))
            if (length(dim(unlistData)) < 2)
                unlistData <- unlistData[orderElts]
            else
                unlistData <- unlistData[orderElts, , drop = FALSE]
            splitFactor <- sort(splitFactor)
            if (is.factor(runValue(splitFactor)) && drop)
                runValue(splitFactor) <- runValue(splitFactor)[drop = TRUE]
        }
        if (is.factor(runValue(splitFactor)) && !drop) {
            NAMES <- levels(runValue(splitFactor))
            width <- structure(rep.int(0L, length(NAMES)), names = NAMES)
            width[as.character(runValue(splitFactor))] <- runLength(splitFactor)
            end <- cumsum(unname(width))
        } else {
            NAMES <- as.character(runValue(splitFactor))
            end <- cumsum(runLength(splitFactor))
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
    ## FIXME: workaround to support CompressedNormalIRangesList
    ## elementTypeX <- elementType(x)
    elementTypeX <- elementType(new(class(x)))
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
    nonZeroLength <- elementLengths(X)[INDEX] > 0L
    whichNonZeroLength <- which(nonZeroLength)
    kOK <- length(whichNonZeroLength)
    if ((k > 0) && all(nonZeroLength)) {
        zeroLengthElt <- NULL
    } else if (length(dim(X@unlistData)) < 2) {
        zeroLengthElt <- FUN(X@unlistData[integer(0)], ...)
    } else {
        zeroLengthElt <- FUN(X@unlistData[integer(0), , drop = FALSE], ...)
    }
    useFastSubset <- (is.vector(X@unlistData) || is(X@unlistData, "Sequence"))
    if (!COMPRESS && (k == 0)) {
        elts <- list()
    } else if (COMPRESS && (kOK == 0)) {
        elts <- zeroLengthElt
    } else if(COMPRESS && missing(FUN) && useFastSubset) {
        nzINDEX <- INDEX[whichNonZeroLength]
        elts <-
          seqselect(X@unlistData,
                    start = start(X@partitioning)[nzINDEX],
                    width = width(X@partitioning)[nzINDEX])
    } else {
        elts <- rep(list(zeroLengthElt), k)
        if (kOK > 0) {
            nzINDEX <- INDEX[whichNonZeroLength]
            eltStarts <- start(X@partitioning)[nzINDEX]
            eltEnds <- end(X@partitioning)[nzINDEX]
            if (useFastSubset) {
                elts[whichNonZeroLength] <-
                  lapply(seq_len(kOK), function(j)
                         FUN(window(X@unlistData, start = eltStarts[j],
                                    end = eltEnds[j]), ...))
            } else if (length(dim(X@unlistData)) < 2) {
                elts[whichNonZeroLength] <-
                  lapply(seq_len(kOK), function(j)
                         FUN(X@unlistData[eltStarts[j]:eltEnds[j]], ...))
            } else {
                elts[whichNonZeroLength] <-
                  lapply(seq_len(kOK), function(j)
                         FUN(X@unlistData[eltStarts[j]:eltEnds[j], ,
                                          drop = FALSE], ...))
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
                             stop("cannot coerce 'value' to a ", elementType(x),
                                  " instance")
                         listData <- as.list(x, use.names = FALSE)
                         listData[[i]] <- value
                         widths <- elementLengths(x)
                         names(widths) <- NULL
                         widths[i] <-
                           ifelse(length(dim(value)) < 2, length(value),
                                  nrow(value))
                         if ((i == length(x) + 1L) &&
                             (!is.null(names(x)) || nchar(nameValue) > 0)) {
                             NAMES <- names(x)
                             if (is.null(NAMES))
                                 NAMES <- rep.int("", length(x))
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
              if (!missing(i)) {
                  if (is(i, "RangesList") || is(i, "RleList") ||
                      is(i, "LogicalList") || is(i, "IntegerList")) {
                      x <- seqselect(x, i)
                  } else if (!is.atomic(i)) {
                      stop("invalid subscript type")
                  } else {
                      lx <- length(x)
                      nullOK <- extends("NULL", elementType(x))
                      if (is.numeric(i)) {
                          if (!is.integer(i))
                              i <- as.integer(i)
                          ina <- is.na(i)
                          if (any(ina)) {
                              if (!nullOK)
                                  stop("cannot subset by NA (NULL elements not allowed)")
                              i <- i[!ina]
                          }
                          if (any(abs(i) > lx))
                              stop("subscript out of bounds")
                          if (any(i < 0)) {
                              if (any(i > 0))
                                  stop("negative and positive indices cannot be mixed")
                              if (sum(ina) > 0)
                                  stop("NAs cannot be mixed with negative subscripts")
                              i <- seq_len(lx)[i]
                          }
                      } else if (is.logical(i)) {
                          if (any(is.na(i)))
                              stop("subscript contains NAs")
                          li <- length(i)
                          if (li > lx && !nullOK)
                              stop("subscript out of bounds (and NULL elements not allowed)")
                          if (li < lx)
                              i <- rep(i, length.out = lx)
                          i <- which(i)
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
                      partitionEnd <- cumsum(elementLengths(x)[i])
                      names(partitionEnd) <- NULL
                      slot(x, "unlistData", check=FALSE) <-
                        .CompressedList.list.subscript(X = x, INDEX = i,
                                                       USE.NAMES = FALSE)
                      slot(x, "partitioning", check=FALSE) <- 
                        new2("PartitioningByEnd", end = partitionEnd,
                             NAMES = names(x)[i], check=FALSE)
                      x <- .bracket.Sequence(x, i)
                  }
              }
              x
          })

setMethod("seqselect", "CompressedList",
          function(x, start=NULL, end=NULL, width=NULL)
          {
              if (!is.null(start) && is.null(end) && is.null(width) &&
                  (length(x) > 0)) {
                  if (length(x) != length(start))
                      stop("'length(start)' must equal 'length(x)' when ",
                           "'end' and 'width' are NULL")
                  if (is.list(start)) {
                      if (is.logical(start[[1L]]))
                          start <- LogicalList(start)
                      else if (is.numeric(start[[1L]]))
                          start <- IntegerList(start)
                  } else if (is(start, "RleList")) {
                      start <- IRangesList(start)
                  }
                  if (is(start, "RangesList")) {
                      unlistData <-
                        seqselect(x@unlistData,
                                  shift(unlist(start),
                                        rep.int(start(x@partitioning) - 1L,
                                                elementLengths(start))))
                      partitionEnd <-
                        cumsum(unlist(lapply(start, function(x) sum(width(x))),
                                      use.names = FALSE))
                  } else if (is(start, "LogicalList")) {
                      xeltlen <- elementLengths(x)
                      whichRep <- which(xeltlen != elementLengths(start))
                      for (i in whichRep)
                          start[[i]] <- rep(start[[i]], length.out = xeltlen[i])
                      if (length(dim(x@unlistData)) < 2) {
                          unlistData <- x@unlistData[unlist(start)]
                      } else {
                          unlistData <-
                            x@unlistData[unlist(start), , drop = FALSE]
                      }
                      partitionEnd <-
                        cumsum(unlist(lapply(start, sum), use.names = FALSE))
                  } else if (is(start, "IntegerList")) {
                      i <-
                        unlist(start +
                               newCompressedList("CompressedIntegerList",
                                                 start(x@partitioning) - 1L,
                                                 end = seq_len(length(x))))
                      if (length(dim(x@unlistData)) < 2)
                          unlistData <- x@unlistData[i]
                      else
                          unlistData <- x@unlistData[i, , drop = FALSE]
                      partitionEnd <- cumsum(unname(elementLengths(start)))
                  } else {
                      stop("unrecognized 'start' type")
                  }
                  x <-
                    initialize(x,
                               unlistData = unlistData,
                               partitioning = 
                                 new2("PartitioningByEnd",
                                      end = partitionEnd, NAMES = names(x),
                                      check=FALSE))
              } else {
                  x <- callNextMethod()
              }
              x
          })

setReplaceMethod("seqselect", "CompressedList",
                 function(x, start = NULL, end = NULL, width = NULL, value)
                 {
                     if (!is.null(start) && is.null(end) && is.null(width) &&
                         (length(x) > 0)) {
                         if (length(x) != length(start))
                             stop("'length(start)' must equal 'length(x)' ",
                                  "when 'end' and 'width' are NULL")
                         if (is.list(start)) {
                             if (is.logical(start[[1L]]))
                                 start <- LogicalList(start)
                             else if (is.numeric(start[[1L]]))
                                 start <- IntegerList(start)
                         } else if (is(start, "RleList")) {
                             start <- IRangesList(start)
                         }
                         if (is(start, "RangesList")) {
                             start <-
                               shift(unlist(start),
                                     rep.int(start(x@partitioning) - 1L,
                                             elementLengths(start)))
                         } else if (is(start, "LogicalList")) {
                             xeltlen <- elementLengths(x)
                             whichRep <- which(xeltlen != elementLengths(start))
                             for (i in whichRep) {
                                 start[[i]] <-
                                   rep(start[[i]], length.out = xeltlen[i])
                             }
                             start <- unlist(start)
                         } else if (is(start, "IntegerList")) {
                             i <-
                               unlist(start +
                                      newCompressedList("CompressedIntegerList",
                                                        start(x@partitioning) - 1L,
                                                        end = seq_len(length(x))))
                             start <- rep.int(FALSE, sum(elementLengths(x)))
                             start[i] <- TRUE
                         } else {
                             stop("unrecognized 'start' type")
                         }
                         seqselect(x@unlistData, start) <-
                           unlist(value, use.names=FALSE)
                     } else {
                         x <- callNextMethod()
                     }
                     x
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
              if (missing(x))
                  tls <- unname(list(...))
              else
                  tls <- unname(list(x, ...))
              if (!all(sapply(tls, is, "CompressedList")))
                  stop("all arguments in '...' must be CompressedList objects")
              ecs <- sapply(tls, elementType)
              if (!all(sapply(ecs, extends, ecs[[1L]])))
                  stop("all arguments in '...' must have an element class ",
                       "that extends that of the first argument")
              if (length(dim(tls[[1L]]@unlistData)) < 2)
                  unlistData <- do.call(c, lapply(tls, slot, "unlistData"))
              else
                  unlistData <- do.call(rbind, lapply(tls, slot, "unlistData"))
              partitionEnd <-
                cumsum(do.call(c,
                               lapply(tls, function(y) {
                                          z <- elementLengths(y)
                                          names(z) <- NULL
                                          z
                                      })))
              NAMES <-
                do.call(c,
                        lapply(tls, function(y) {
                                   nms <- names(y)
                                   if (is.null(nms))
                                       nms <- rep.int("", length(y))
                                   nms
                               }))
              if (all(nchar(NAMES) == 0L))
                  NAMES <- NULL
              eltmetaX <- elementMetadata(x)
              x <- newCompressedList(class(tls[[1L]]), unlistData,
                                     end = partitionEnd, NAMES = NAMES)
              slot(x, "elementMetadata", check=FALSE) <- eltmetaX
              .c.Sequence(x, ...)
          })

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Looping.
###

setMethod("lapply", "CompressedList",
          function(X, FUN, ...)
          {
              if (length(X) == 0)
                  list()
              else
                  .CompressedList.list.subscript(X = X,
                                                 INDEX = seq_len(length(X)),
                                                 USE.NAMES = TRUE,
                                                 COMPRESS = FALSE,
                                                 FUN = match.fun(FUN), ...)
          })

setMethod("aggregate", "CompressedList",
          function(x, by, FUN, start = NULL, end = NULL, width = NULL,
                   frequency = NULL, delta = NULL, ..., simplify = TRUE)
          {
              if (!missing(by) && is(by, "RangesList")) {
                  if (length(x) != length(by))
                      stop("for Ranges 'by', 'length(x) != length(by)'")
                  y <- as.list(x)
                  result <-
                    lapply(structure(seq_len(length(x)), names = names(x)),
                           function(i)
                               aggregate(y[[i]], by = by[[i]], FUN = FUN,
                                         frequency = frequency, delta = delta,
                                         ..., simplify = simplify))
                  ans <- try(SimpleAtomicList(result), silent = TRUE)
                  if (inherits(ans, "try-error"))
                      ans <- newSimpleList("SimpleList", result)
              } else {
                  ans <- callNextMethod()
              }
              ans
          })

.updateCompressedList <- function(X, listData) {
    elementTypeX <- elementType(X)
    if (!all(sapply(listData,
                    function(Xi) extends(class(Xi), elementTypeX))))
        stop("'FUN' must return elements of class ", elementTypeX)
    if (length(listData) == 0) {
        end <- integer(0)
    } else {
        if (length(dim(listData[[1L]])) < 2) {
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
              .updateCompressedList(list(...)[[1L]],
                                    mapply(FUN = FUN, ..., MoreArgs = MoreArgs,
                                           SIMPLIFY = FALSE))
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
                      if (!is.null(names(x))) {
                          nms <- rep.int(names(x), elementLengths(x))
                          if (!is.null(names(ans)))
                              nms <- paste(nms, names(ans), sep = ".")
                          names(ans) <- nms
                      }
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
              cat(class(object), " of length ", lo, "\n", sep = "")
              if (!is.null(names(object)))
                  cat(labeledLine("names", names(object)))
          })
