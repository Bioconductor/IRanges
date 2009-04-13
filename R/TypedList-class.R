### =========================================================================
### TypedList objects
### -------------------------------------------------------------------------

## Wrapper around a list that ensures all elements extend from a certain type
setClassUnion("ANYTHING", methods:::.BasicClasses)

setClass("TypedList",
         contains="ListLike",
         representation(
                       "VIRTUAL",
                        elements="list",   # a list of R objects
                        NAMES="characterORNULL",  # R doesn't like @names !!
                        elementClass="character",
                        elementLengths="integer",
                        compress="logical"
                        ),
         prototype(
                   elements=list(),
                   NAMES=NULL,
                   elementClass="ANYTHING",
                   elementLengths=integer(0),
                   compress=FALSE
                   )
         )

setMethod("initialize", "TypedList",
          function(.Object,
                   elements = .Object@elements,
                   NAMES = names(elements),
                   elementClass = .Object@elementClass,
                   elementLengths = integer(0),
                   compress = .Object@compress,
                   check = TRUE, ...) {
            if (!is.list(elements))
              stop("'elements' must be a list object")
            if (!all(unlist(lapply(elements, is, elementClass))))
              stop("all elements must be ", elementClass, " objects")
            if (length(elementLengths) == 0) {
              if (length(elements) == 0) {
                elementLengths <- integer(0)
              } else if (length(dim(elements[[1]])) < 2) {
                elementLengths <- unlist(lapply(elements, length), use.names = FALSE)
              } else {
                elementLengths <- unlist(lapply(elements, nrow), use.names = FALSE)
              }
            }
            slot(.Object, "NAMES", check = check) <- NAMES
            if (compress) {
              elements <-
                .TypedList.compress.list(lapply(elements, as, elementClass))
            } else {
              elements <- unname(elements)
            }
            slot(.Object, "elements", check = check) <- elements
            slot(.Object, "elementClass", check = check) <- elementClass
            slot(.Object, "elementLengths", check = check) <- elementLengths
            slot(.Object, "compress", check = check) <- compress
            callNextMethod(.Object, ...)
          })

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Updating old TypedList objects.
###

isOldTypedList <- function(object) {
    if (!is(object, "TypedList"))
        stop("'object' must inherit from 'TypedList'")

    if (!is(object, "ListLike")) {
        TRUE
    } else {
        objectSlots <- attributes(object)
        objectSlots$class <- NULL
        classSlots <- slotNames(class(object))
        !all(classSlots %in% names(objectSlots))
    }
}

updateTypedList <- function(object) {
    if (!is(object, "TypedList"))
        stop("'object' must inherit from 'TypedList'")

    objectSlots <- attributes(object)
    objectSlots$class <- NULL
    classSlots <- slotNames(class(object))
    if (all(classSlots %in% names(objectSlots))) {
        ans <- object
    } else {
        objectSlots$elements <-
          lapply(objectSlots$elements, function(x)
                 if (is(x, "TypedList")) updateTypedList(x) else x)
        nulls <-
          sapply(names(objectSlots), function(slt) is.null(slot(object, slt)))
        objectSlots[nulls] <- NULL
        if ("elementCumLengths" %in% names(objectSlots)) {
            objectSlots$elementLengths <-
              diff(slot(object, "elementCumLengths"))
        }
        keep <- intersect(names(objectSlots), classSlots)
        ans <- do.call(new, c(list(class(object)), objectSlots[keep]))
    }
    ans
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Accessor methods.
###

setGeneric("elementClass", function(x, ...) standardGeneric("elementClass"))
setMethod("elementClass", "TypedList", function(x) x@elementClass)

setGeneric("elementLengths", function(x) standardGeneric("elementLengths"))
setMethod("elementLengths", "TypedList",
          function(x) {
            if (isOldTypedList(x))
              x <- updateTypedList(x)
            x@elementLengths
          })

setMethod("length", "TypedList",
          function(x) {
            if (isOldTypedList(x))
              x <- updateTypedList(x)
            length(x@elementLengths)
          })

setMethod("names", "TypedList", function(x) x@NAMES)

### The only replacement method for TypedList objects!
setReplaceMethod("names", "TypedList",
                 function(x, value)
                 {
                   if (isOldTypedList(x))
                     x <- updateTypedList(x)
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
                      compress = !is.null(splitFactor)) {
  if (!extends(listClass, "TypedList"))
    stop("cannot create a ", listClass, " as a 'TypedList'")
  if (length(splitFactor) == 0) {
    elementLengths <- NULL
    NAMES <- names(elements)
  } else {
    if (is.unsorted(splitFactor)) {
      orderElts <- order(splitFactor)
      if (length(dim(elements)) < 2)
        elements <- list(elements[orderElts])
      else
        elements <- list(elements[orderElts, , drop = FALSE])
      splitFactor <- splitFactor[orderElts]
    } else {
      elements <- list(elements)
    }
    if (is.factor(splitFactor)) {
      splitRle <- rle(as.character(splitFactor))
      fullValues <- levels(splitFactor)
      fullLengths <- rep.int(0L, length(fullValues))
      fullLengths[match(splitRle[["values"]], fullValues)] <-
        splitRle[["lengths"]]
      splitRle[["values"]] <- fullValues
      splitRle[["lengths"]] <- fullLengths
    } else {
      splitRle <- rle(splitFactor)
    }
    NAMES <- as.character(splitRle[["values"]])
    elementLengths <- splitRle[["lengths"]]
  }
  new(listClass, elements = elements, NAMES = NAMES,
      elementLengths = elementLengths, compress = compress)
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Validity.
###

.valid.TypedList.classdef <- function(x)
{
  if (isOldTypedList(x))
    return("object uses old class definition; use 'updateTypedList'")
    
}

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
  if ((length(x@compress) != 1) || is.na(x@compress))
    return("compress status is unknown")
  if ((x@compress && length(x@elements) > 1) ||
      (!x@compress && (length(x) != length(x@elements)))) {
    return("improper compression")
  }
  NULL
}

.valid.TypedList.slot.names <- function(x)
{
  for (i in slotNames("TypedList")) {
    if (length(names(slot(x, i))) != 0)
      return(paste("slot '", i, "' should not have names", sep=""))
  }
  NULL
}

.valid.TypedList <- function(x)
{
  c(.valid.TypedList.classdef(x),
    .valid.TypedList.elements(x),
    .valid.TypedList.names(x),
    .valid.TypedList.compression(x),
    .valid.TypedList.slot.names(x))
}

setValidity2("TypedList", .valid.TypedList)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Subsetting.
###

.TypedList.list.subscript <-
function(x, i, use.names = TRUE, compress = x@compress) {
  k <- length(i)
  if (k == 0) {
    elts <- list()
  } else if (length(x) == length(x@elements)) {
    elts <- x@elements[i]
    if (use.names) {
      names(elts) <- names(x)[i]
    }
  } else {
    if (compress) {
      runStarts <- which(c(TRUE, diff(i) != 1L))
      whichToLoop <- seq_len(length(runStarts))
      startIndices <- i[runStarts]
      endIndices <- startIndices + (diff(c(runStarts, k+1L)) - 1L)
    } else {
      runStarts <- seq_len(k)
      whichToLoop <- which(elementLengths(x)[i] > 0)
      startIndices <- i[runStarts[whichToLoop]]
      endIndices <- startIndices
    }
    if (length(dim(x@elements[[1]])) < 2)
        zeroLengthElt <- x@elements[[1]][integer(0)]
    else
        zeroLengthElt <- x@elements[[1]][integer(0), , drop = FALSE]
    elts <- rep(list(zeroLengthElt), length(runStarts))
    loopCount <- length(whichToLoop)
    if (loopCount > 0) {
      elementCumLengths <- cumsum(subseq(elementLengths(x), 1, max(i)))
      allData <- x@elements[[1]]
      elts[whichToLoop] <-
        lapply(seq_len(loopCount),
               function(j) {
                 startIndex <- startIndices[j]
                 endIndex <- endIndices[j]
                 eltLength <-
                   sum(subseq(elementLengths(x), startIndex, endIndex))
                 if (eltLength == 0L) {
                   elt <- zeroLengthElt
                 } else {
                   if (startIndex == 1L)
                     eltStart <- 1L
                   else
                     eltStart <- elementCumLengths[startIndex - 1L] + 1L
                   eltEnd <- elementCumLengths[endIndex]
                   if (is.vector(allData) && (eltStart <= eltEnd))
                     elt <- subseq(allData, start = eltStart, end = eltEnd)
                   else if (length(dim(allData)) < 2)
                     elt <- allData[eltStart:eltEnd]
                   else
                     elt <- allData[eltStart:eltEnd, , drop = FALSE]
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
            if (isOldTypedList(x))
              x <- updateTypedList(x)
            index <- try(callNextMethod(), silent = TRUE)
            if (inherits(index, "try-error")) {
              if (length(i) == 1 && (is.na(i) || is.character(i)))
                ans <- NULL
              else
                stop(index)
            } else {
              ans <- .TypedList.list.subscript(x, index, use.names = FALSE)[[1]]
            }
            ans
          })

setReplaceMethod("[[", "TypedList",
                 function(x, i, j,..., value)
                 {
                   if (isOldTypedList(x))
                     x <- updateTypedList(x)
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
                       stop("cannot coerce 'value' to a ", elementClass(x), " instance")
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
                     slot(x, "elementLengths", check=FALSE) <- elementLengths
                   }
                   x
                 })

setReplaceMethod("$", "TypedList",
                 function(x, name, value) {
                   if (isOldTypedList(x))
                     x <- updateTypedList(x)
                   x[[name]] <- value
                   x
                 })

### Supported 'i' types: numeric, character, logical, NULL and missing.
setMethod("[", "TypedList",
          function(x, i, j, ..., drop)
          {
            if (isOldTypedList(x))
              x <- updateTypedList(x)
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
              .TypedList.list.subscript(x, i, use.names = FALSE, compress = x@compress)
            elementLengths <- elementLengths(x)[i]
            if (!is.null(names(x)))
              slot(x, "NAMES", check=FALSE) <- names(x)[i]
            slot(x, "elements", check=FALSE) <- elts
            slot(x, "elementLengths", check=FALSE) <- elementLengths
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
            if (isOldTypedList(x))
              x <- updateTypedList(x)
            if (isOldTypedList(values))
              values <- updateTypedList(values)
            if (!isSingleNumber(after))
              stop("'after' must be a single number")
            if (!extends(elementClass(x), elementClass(values)))
              stop("the element class of 'values' must extend that of 'x'")
            TypedList(class(x),
                      append(as.list(x, use.names = TRUE),
                             as.list(values, use.names = TRUE),
                             after = after),
                      compress = x@compress)
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
              stop("all arguments in '...' must be TypedList objects")
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
            TypedList(class(tls[[1]]), elts, compress = tls[[1]]@compress)
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
              if (isOldTypedList(x))
                x <- updateTypedList(x)
              if (length(x@elements) < length(x)) {
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

setMethod("unlist", "TypedList",
          function(x, recursive = TRUE, use.names = TRUE) {
            if (isOldTypedList(x))
              x <- updateTypedList(x)
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
### The "show" method.
###

setMethod("show", "TypedList",
          function(object)
          {
            lo <- length(object)
            cat("  A ", class(object), " instance of length ", lo, "\n", sep="")
### TODO: show (some of the) elements here
          })
