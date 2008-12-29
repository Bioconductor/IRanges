### =========================================================================
### TypedList objects
### -------------------------------------------------------------------------

## Wrapper around a list that ensures all elements extend from a certain type
setClassUnion("ANYTHING", methods:::.BasicClasses)

setClass("TypedList",
         representation(
                        elements="list",   # a list of R objects
                        NAMES="characterORNULL",  # R doesn't like @names !!
                        elementClass="character",
                        elementCumLengths="integer", # cumsum(c(1L, sapply(as.list(x), length)))
                        compressedIndices="integer",  # length(x) + 1 vector
                        compressible="logical"
                        ),
         prototype(
                   elements=list(),
                   NAMES=NULL,
                   elementClass="ANYTHING",
                   elementCumLengths=integer(0),
                   compressedIndices=1L,
                   compressible=FALSE
                   ),
         contains = "VIRTUAL"
         )

setMethod("initialize", "TypedList",
          function(.Object, elements = list(), NAMES = names(elements),
                   elementClass = .Object@elementClass,
                   elementCumLengths = NULL, compressedIndices = NULL,
                   compress = FALSE, check = TRUE, ...) {
            if (!is.list(elements))
              stop("'elements' must be a list object")
            if (!all(unlist(lapply(elements, is, elementClass))))
              stop("all elements must be instances of '", elementClass, "'")
            if (is.null(elementCumLengths) != is.null(compressedIndices))
              stop("must supply either both 'elementCumLengths' and 'compressedIndices' or neither")
            if (.Object@compressible)
              elements <- lapply(elements, as, elementClass)
            if (is.null(elementCumLengths)) {
              if (length(elements) == 0) {
                elementCumLengths <- 1L
              } else if (length(dim(elements[[1]])) < 2) {
                elementCumLengths <- cumsum(c(1L, unlist(lapply(elements, length), use.names = FALSE)))
              } else {
                elementCumLengths <- cumsum(c(1L, unlist(lapply(elements, nrow), use.names = FALSE)))
              }
            }
            slot(.Object, "NAMES", check = check) <- NAMES
            if (is.null(compressedIndices)) {
              if(compress && (length(elements) > 0)) {
                compressedIndices <- c(1L, length(elements) + 1L)
                elements <- .TypedList.compress.list(elements)
              } else {
                compressedIndices <- seq_len(length(elements) + 1L)
              }
            } else if(compress) {
              warning("cannot compress object since 'compressedIndices' is supplied")
            }
            slot(.Object, "elements", check = check) <- unname(elements)
            slot(.Object, "elementClass", check = check) <- elementClass
            slot(.Object, "elementCumLengths", check = check) <- elementCumLengths
            slot(.Object, "compressedIndices", check = check) <- compressedIndices
            callNextMethod(.Object, ...)
          })

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Accessor methods.
###

setGeneric("elementClass", function(x, ...) standardGeneric("elementClass"))
setMethod("elementClass", "TypedList", function(x) x@elementClass)

setGeneric("elementLengths", function(x) standardGeneric("elementLengths"))
setMethod("elementLengths", "TypedList", function(x) diff(x@elementCumLengths))

setMethod("length", "TypedList", function(x) length(x@elementCumLengths) - 1L)

setMethod("names", "TypedList", function(x) x@NAMES)

### The only replacement method for TypedList objects!
setReplaceMethod("names", "TypedList",
                 function(x, value)
                 {
                   if (is.null(value)) {
                     x@NAMES <- NULL
                     return(x)
                   }
                   value <- as.character(value)
                   if (length(value) > length(x))
                     stop("number of names ",
                          length(value), " greater than number of elements",
                          length(x))
                   if (length(value) < length(x))
                     value <- c(value, rep(NA_character_, length(x) - length(value)))
                   x@NAMES <- value
                   x
                 })

setMethod("isEmpty", "TypedList",
          function(x)
          {
            if (length(x) == 0)
              logical(0)
            else
              (elementLengths(x) == 0)
          })

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Constructor.
###

.TypedList.compress.list <- function(x, as.list = TRUE) {
  if (length(x) == 1) {
    if (as.list) {
      x <- unname(x)
    } else {
      x <- x[[1]]
    }
  } else if (length(x) > 1) {
    if (length(dim(x[[1]])) < 2) {
      x <- do.call(c, unname(x))
    } else {
      x <- do.call(rbind, unname(x))
    }
    if (as.list)
      x <- list(x)
  }
  x
}

TypedList <- function(listClass, elements = list(), splitFactor = NULL,
                      compress = FALSE) {
  if (!extends(listClass, "TypedList"))
    stop("cannot create a ", listClass, " as a 'TypedList'")
  if (length(splitFactor) == 0) {
    elementCumLengths <- NULL
    compressedIndices <- NULL
    NAMES <- names(elements)
  } else {
    if (is.factor(splitFactor))
      splitFactor <- as.character(splitFactor)
    orderElts <- order(splitFactor)
    if (length(dim(elements)) < 2)
      elements <- list(elements[orderElts])
    else
      elements <- list(elements[orderElts, , drop = FALSE])
    splitFactor <- splitFactor[orderElts]
    splitRle <- rle(splitFactor)
    NAMES <- as.character(splitRle[["values"]])
    elementCumLengths <- cumsum(c(1L, splitRle[["lengths"]]))
    compressedIndices <- c(1L, length(splitRle[["lengths"]]) + 1L)
  }
  new(listClass, elements = elements, NAMES = NAMES,
      elementCumLengths = elementCumLengths,
      compressedIndices = compressedIndices, compress = compress)
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Validity.
###

## allow subclasses to provide the required class of elements in the list
.valid.TypedList.elements <- function(x)
{
  ### NOTE: we do not use is() here, because it uses S3 inheritance
  ### for S3 objects, while we want S4 inheritance (i.e. "ANY" should
  ### mean any type of object)
  elementClassX <- elementClass(x)
  if (!is.list(x@elements) ||
      !all(sapply(x@elements,
                  function(xi) extends(class(xi), elementClassX))))
    return(paste("the 'elements' slot must contain a list of",
                 elementClassX, "objects"))
  NULL
}

.valid.TypedList.names <- function(x)
{
  if (!is(names(x), "characterORNULL"))
    return("the names must be NULL or contain a character vector")
  if (is.null(names(x)))
    return(NULL)
  if (length(names(x)) != length(x))
    return("number of names and number of elements differ")
  NULL
}

.valid.TypedList.compression <- function(x)
{
  if (length(x) > 0) {
    if (length(dim(x@elements[[1]])) < 2) {
      nRagged <- unlist(lapply(x@elements, length))
    } else {
      nRagged <- unlist(lapply(x@elements, nrow))
    }
    if (!all(cumsum(c(1L, nRagged)) == x@elementCumLengths[x@compressedIndices]))
      return("ill-formed element lengths")
  }
  if ((length(x@compressible) != 1) || is.na(x@compressible))
    return("compressible status is unknown")
  if ((length(x@compressedIndices) > length(x@elementCumLengths)) ||
      (x@compressedIndices[1] != 1) ||
      (tail(x@compressedIndices, 1) != (length(x) + 1)) ||
      is.unsorted(x@compressedIndices))
    return("ill-formed 'compressedIndices'")
  NULL
}

.valid.TypedList.slot.names <- function(x)
{
  for (i in c("elements","NAMES", "elementClass", "elementCumLengths",
              "compressedIndices", "compressible")) {
    if (length(names(slot(x, i))) != 0)
      stop("slot '", i, "' should not have names")
  }
  NULL
}

.valid.TypedList <- function(x)
{
  c(.valid.TypedList.elements(x),
    .valid.TypedList.names(x),
    .valid.TypedList.compression(x),
    .valid.TypedList.slot.names(x))
}

setValidity2("TypedList", .valid.TypedList)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Subsetting.
###

.TypedList.list.subscript <-
function(x, i, use.names = TRUE, compress = x@compressible) {
  k <- length(i)
  if (k == 0) {
    elts <- list()
  } else if (!x@compressible) {
    elts <- x@elements[i]
    if (use.names) {
      names(elts) <- names(x)[i]
    }
  } else {
    listIndices <- findInterval(i, x@compressedIndices)
    if (compress) {
      runStarts <-
        which(c(TRUE, diff(listIndices) != 0L | diff(i) != 1L))
      runOffsets <- diff(c(runStarts, k+1L)) - 1L
      whichToLoop <- seq_len(length(runStarts))
    } else {
      runStarts <- seq_len(k)
      runOffsets <- rep.int(0L, k)
      whichToLoop <- which(elementLengths(x)[i] > 0)
    }
    if (length(dim(elt)) < 2)
        zeroLengthElt <- x@elements[[1]][integer(0)]
    else
        zeroLengthElt <- x@elements[[1]][integer(0), , drop = FALSE]
    elts <- rep(list(zeroLengthElt), length(runStarts))
    if (length(whichToLoop) > 0) {
      elts[whichToLoop] <-
        lapply(whichToLoop,
               function(j) {
                 runStart <- runStarts[j]
                 runOffset <- runOffsets[j]
                 runEnd <- runStart + runOffset
                 listIndex <- listIndices[runStart]
                 elt <- x@elements[[listIndex]]
                 startIndex <- i[runStart]
                 endIndex <- startIndex + runOffset + 1L
                 firstInListElt <- x@compressedIndices[listIndex]
                 eltLength <-
                   x@compressedIndices[listIndex+1L] - firstInListElt
                 if (eltLength > abs(endIndex - startIndex)) {
                   if (abs(x@elementCumLengths[startIndex] -
                           x@elementCumLengths[endIndex]) == 0) {
                     elt <- new(elementClass(x))
                   } else {
                     eltStart <-
                       x@elementCumLengths[startIndex] -
                         x@elementCumLengths[firstInListElt] + 1L
                     eltEnd <-
                       x@elementCumLengths[endIndex] -
                         x@elementCumLengths[firstInListElt]
                     if (is.vector(elt))
                       elt <- subseq(elt, start = eltStart, end = eltEnd)
                     else if (length(dim(elt)) < 2)
                       elt <- elt[eltStart:eltEnd]
                     else
                       elt <- elt[eltStart:eltEnd, , drop = FALSE]
                   }
                 }
                 elt
               })
    }
    if (compress) {
      elts <- .TypedList.compress.list(elts)
    } else if (use.names) {
      names(elts) <- names(x)[i]
    }
  }
  elts
}

### Extract the i-th element of a TypedList object.
### Supported 'i' types: numeric vector of length 1.
setMethod("[[", "TypedList",
          function(x, i, j, ...)
          {
            if (!missing(j) || length(list(...)) > 0)
              stop("invalid subsetting")
            if (missing(i))
              stop("subscript is missing")
            if (!is.character(i) && !is.numeric(i))
              stop("invalid subscript type")
            if (length(i) < 1L)
              stop("attempt to select less than one element")
            if (length(i) > 1L)
              stop("attempt to select more than one element")
            if (!is.character(i) && !is.na(i) && (i<1L || i>length(x)))
              stop("subscript out of bounds")
            if (is.character(i)) {
              i <- match(i, names(x))
            }
            if (is.na(i)) {
              ans <- NULL
            } else {
              ans <- .TypedList.list.subscript(x, i, use.names = FALSE)[[1]]
            }
            ans
          })

setMethod("$", "TypedList", function(x, name) x[[name]])

setReplaceMethod("[[", "TypedList",
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
                     value <- try(as(value, elementClass(x)), silent = TRUE)
                     if (inherits(value, "try-error"))
                       stop("cannot coerce 'value' to 'elementClass' instance")
                     elts <- as.list(x, use.names = FALSE)
                     elts[[i]] <- value
                     elementLengths <- elementLengths(x)
                     elementLengths[i] <-
                       ifelse(length(dim(value)) < 2, length(value), nrow(value))
                     if ((i == length(x) + 1L) &&
                         (!is.null(names(x)) || nchar(nameValue) > 0)) {
                       NAMES <- names(x)
                       if (is.null(NAMES))
                         NAMES <- rep("", length(x))
                       NAMES[i] <- nameValue
                       slot(x, "NAMES", check=FALSE) <- NAMES
                     }
                     slot(x, "elements", check=FALSE) <- elts
                     slot(x, "elementCumLengths", check=FALSE) <- cumsum(c(1L, elementLengths))
                     slot(x, "compressedIndices", check=FALSE) <- seq_len(length(elementLengths) + 1L)
                   }
                   x
                 })

setReplaceMethod("$", "TypedList",
                 function(x, name, value) {
                   x[[name]] <- value
                   x
                 })

### Supported 'i' types: numeric, character, logical, NULL and missing.
setMethod("[", "TypedList",
          function(x, i, j, ..., drop)
          {
            if (!missing(j) || length(list(...)) > 0)
              stop("invalid subsetting")
            if (missing(i))
              return(x)
            if (!is.atomic(i))
              stop("invalid subscript type")
            lx <- length(x)
            nullOK <- extends("NULL", elementClass(x))
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
            elts <-
              .TypedList.list.subscript(x, i, use.names = FALSE, compress = x@compressible)
            elementLengths <- elementLengths(x)[i]
            if (!is.null(names(x)))
              slot(x, "NAMES", check=FALSE) <- names(x)[i]
            slot(x, "elements", check=FALSE) <- elts
            slot(x, "elementCumLengths", check=FALSE) <- cumsum(c(1L, elementLengths))
            if (x@compressible) {
              if (length(i) == 0)
                slot(x, "compressedIndices", check=FALSE) <- 1L
              else
                slot(x, "compressedIndices", check=FALSE) <- c(1L, length(i) + 1L)
            } else {
              slot(x, "compressedIndices", check=FALSE) <- seq_len(length(i) + 1L)
            }
            x
          })

setReplaceMethod("[", "TypedList",
                 function(x, i, j,..., value)
                 stop("attempt to modify the value of a ", class(x),
                      " instance")
                 )

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Combining and splitting.
###

setMethod("append", c("TypedList", "TypedList"),
          function(x, values, after=length(x)) {
            if (!isSingleNumber(after))
              stop("'after' must be a single number")
            if (!extends(elementClass(x), elementClass(values)))
              stop("the element class of 'values' must extend that of 'x'")
            TypedList(class(x),
                      append(as.list(x, use.names = TRUE),
                             as.list(values, use.names = TRUE),
                             after = after),
                      compress = x@compressible)
          })

## NOTE: while the 'c' function does not have an 'x', the generic does
## c() is a primitive, so 'x' can be missing; dispatch is by position,
## although sometimes this does not work so well, so it's best to keep
## names off the parameters whenever feasible.

setMethod("c", "TypedList",
          function(x, ..., recursive = FALSE) {
            if (recursive)
              stop("'recursive' mode is not supported")
            if (!missing(x))
              tls <- list(x, ...)
            else
              tls <- list(...)
            if (!all(sapply(tls, is, "TypedList")))
              stop("all arguments in '...' must be instances of 'TypedList'")
            ecs <- sapply(tls, elementClass)
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
            TypedList(class(tls[[1]]), elts, compress = tls[[1]]@compressible)
          })

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "lapply" method.
###

setMethod("lapply", c("TypedList", "function"),
          function(X, FUN, ...) {
            lapply(as.list(X), FUN, ...)
          })

setMethod("lapply", c("TypedList", "character"),
          function(X, FUN, ...) {
            lapply(as.list(X), FUN, ...)
          })

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion.
###

### From an TypedList object to a normal R list.
setAs("TypedList", "list",
      function(from) {
        as.list(from)
      })

### Subclasses should override this for customized list coercion
setMethod("as.list", "TypedList",
          function(x, use.names = TRUE) {
            if (length(x@compressedIndices) < length(x) + 1) {
              ans <-
                .TypedList.list.subscript(x, seq_len(length(x)), use.names = use.names,
                                          compress = FALSE)
            } else {
              ans <- x@elements
              if (use.names)
                names(ans) <- names(x)
            }
            ans
          })

## setMethod("as.data.frame", "TypedList",
##           function(x, row.names=NULL, optional=FALSE, ...)
##           {
##             if (!(is.null(row.names) || is.character(row.names)))
##               stop("'row.names'  must be NULL or a character vector")
##             as.data.frame(as(x, "list"), row.names = row.names,
##                           optional = optional, ...)
##           })

setMethod("unlist", "TypedList",
          function(x, recursive = TRUE, use.names = TRUE) {
            if (!missing(recursive))
              warning("'recursive' argument currently ignored")
            ans <- .TypedList.compress.list(x@elements, as.list = FALSE)
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
            ans
          })

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Methods for compression
###

setGeneric("isCompressed", function(x) standardGeneric("isCompressed"))
setMethod("isCompressed", "TypedList",
          function(x) length(x@compressedIndices) == ifelse(length(x) == 0, 1L, 2L))

setGeneric("compress", function(x, ...) standardGeneric("compress"))
setMethod("compress", "TypedList",
          function(x) {
            if (!x@compressible)
              stop("cannot compress an instance of a ", class(x), " object")
            if (!isCompressed(x)) {
              if (length(x) == 0)
                slot(x, "compressedIndices", check=FALSE) <- 1L
              else
                slot(x, "compressedIndices", check=FALSE) <- c(1L, length(x) + 1L)
              slot(x, "elements", check=FALSE) <- unlist(x)
            }
            x
          })

setGeneric("uncompress", function(x, ...) standardGeneric("uncompress"))
setMethod("uncompress", "TypedList",
          function(x) {
            slot(x, "compressedIndices", check=FALSE) <- seq_len(length(x) + 1L)
            slot(x, "elements", check=FALSE) <- as.list(x, use.names = FALSE)
            x
          })

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "show" method.
###

setMethod("show", "TypedList",
          function(object)
          {
            lo <- length(object)
            cat("  A ", class(object), " instance of length ", lo, "\n", sep="")
### TODO: show (some of the) elements here
          })
