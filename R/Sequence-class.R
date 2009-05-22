### =========================================================================
### Sequence objects
### -------------------------------------------------------------------------
###
### The Sequence virtual class is a general container for storing a sequence
### i.e. an ordered set of elements.
###

setClassUnion("ANYTHING", methods:::.BasicClasses)

setClass("Sequence",
         contains="Annotated",
         representation(
                        "VIRTUAL",
                        elementMetadata = "DataFrameORNULL",
                        elementType = "character"
                        ),
         prototype(elementType="ANYTHING")
)


### [[ operation
checkAndTranslateDbleBracketSubscript <- function(x, i, j, ...)
{
    subscripts <- list(...)
    if ("exact" %in% names(subscripts)) {
        exact <- subscripts[["exact"]]
        subscripts[["exact"]] <- NULL
    } else {
        exact <- TRUE  # default
    }
    if (!missing(i))
        subscripts$i <- i
    if (!missing(j))
        subscripts$j <- j
    if (length(subscripts) != 1L)
        stop("incorrect number of subscripts")
    subscript <- subscripts[[1]]
    if (!is.character(subscript) && !is.numeric(subscript))
        stop("invalid subscript type '", class(subscript), "'")
    if (length(subscript) < 1L)
        stop("attempt to extract less than one element")
    if (length(subscript) > 1L)
        stop("attempt to extract more than one element")
    if (is.na(subscript))
        stop("invalid subscript NA")
    if (is.numeric(subscript)) {
        if (!is.integer(subscript))
            subscript <- as.integer(subscript)
        if (subscript < 1L || length(x) < subscript)
            stop("subscript out of bounds")
        return(subscript)
    }
    ## 'subscript' is a character string
    names_x <- names(x)
    if (is.null(names_x))
        stop("attempt to extract by name when elements have no names")
    #if (subscript == "")
    #    stop("invalid subscript \"\"")
    ans <- charmatch(subscript, names_x)
    if (is.na(ans))
        stop("subscript \"", subscript, "\" matches no name")
    if (ans == 0L) {
        if (isTRUE(exact))
            stop("subscript \"", subscript, "\" matches no name or more than one name")
        stop("subscript \"", subscript, "\" matches more than one name")
    }
    if (isTRUE(exact) && nchar(subscript) != nchar(names_x[ans]))
        stop("subscript \"", subscript, "\" matches no name")
    ans
}
setMethod("[[", "Sequence", function(x, i, j, ...)
          stop("missing '[[' method for Sequence class ", class(x)))
setMethod("$", "Sequence", function(x, name) x[[name, exact=FALSE]])

setMethod("lapply", "Sequence",
          function(X, FUN, ...)
          {
              FUN <- match.fun(FUN)
              ii <- seq_len(length(X))
              names(ii) <- names(X)
              lapply(ii, function(i) FUN(X[[i]], ...))
          })

### The implicit generic would dispatch on the X, FUN, simplify and USE.NAMES
### args but we don't want that.
setGeneric("sapply", signature="X",
           function(X, FUN, ..., simplify=TRUE, USE.NAMES=TRUE)
           standardGeneric("sapply"))

setMethod("sapply", "Sequence",
          function(X, FUN, ..., simplify=TRUE, USE.NAMES=TRUE)
          {
              FUN <- match.fun(FUN)
              ii <- seq_len(length(X))
              names(ii) <- names(X)
              sapply(ii, function(i) FUN(X[[i]], ...), simplify=simplify,
                     USE.NAMES=USE.NAMES)
          })

setAs("Sequence", "list", function(from) as.list(from))

setMethod("as.list", "Sequence", function(x, ...) lapply(x, identity))

setGeneric("isEmpty", function(x) standardGeneric("isEmpty"))

setMethod("isEmpty", "ANY",
          function(x)
          {
              if (is.atomic(x))
                  return(length(x) == 0L)
              if (!is.list(x) && !is(x, "Sequence"))
                  stop("isEmpty() is not defined for objects of class ", class(x))
              ## Recursive definition
              if (length(x) == 0)
                  return(logical(0))
              sapply(x, function(xx) all(isEmpty(xx)))
          })


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Accessor methods.
###

setGeneric("elementLengths", function(x) standardGeneric("elementLengths"))

setGeneric("elementType", function(x, ...) standardGeneric("elementType"))
setMethod("elementType", "Sequence", function(x) x@elementType)

setGeneric("elementMetadata",
           function(x, ...) standardGeneric("elementMetadata"))
setMethod("elementMetadata", "Sequence",
          function(x) {
              if ("elementMetadata" %in% names(attributes(x))) {
                  emd <- x@elementMetadata
                  if (!is.null(emd) && !is.null(names(x)))
                      rownames(emd) <- names(x)
              } else {
                  emd <- NULL
              }
              emd
          })

setGeneric("elementMetadata<-",
           function(x, ..., value) standardGeneric("elementMetadata<-"))
setReplaceMethod("elementMetadata", c("Sequence", "DataFrameORNULL"),
                 function(x, value) {
                     if ("elementMetadata" %in% names(attributes(x))) {
                         if (!is.null(value) && length(x) != nrow(value))
                             stop("the number of rows in elementMetadata 'value' ",
                                     "(if non-NULL) must match the length of 'x'")
                         if (!is.null(value))
                             rownames(value) <- NULL
                         x@elementMetadata <- value
                     }
                     x
                 })

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Validity.
###

.valid.Sequence.elementMetadata <- function(x) {
    emd <- elementMetadata(x)
    if (!is.null(emd) && nrow(emd) != length(x))
        "number of rows in non-NULL 'elementMetadata(x)' must match length of 'x'"
    else if (!is.null(emd) && !identical(rownames(emd), names(x)))
        "the rownames of non-NULL 'elementMetadata(x)' must match the names of 'x'"
    else NULL
}
.valid.Sequence <- function(x)
{
    c(.valid.Sequence.elementMetadata(x))
}
setValidity2("Sequence", .valid.Sequence)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Subsetting.
###

.bracket.Sequence <-
function(x, i, j, ..., drop)
{
    if (!is.null(elementMetadata(x)))
        elementMetadata(x) <- elementMetadata(x)[i,,drop=FALSE]
    x
}
setMethod("[", "Sequence", function(x, i, j, ..., drop)
          stop("missing '[' method for Sequence class ", class(x)))

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Combining and splitting.
###

setMethod("append", c("Sequence", "Sequence"),
          function(x, values, after=length(x)) {
              if (!is.null(elementMetadata(x)))
                  elementMetadata(x) <-
                    rbind(elementMetadata(x), elementMetadata(values))
              x
          })

.c.Sequence <-
function(x, ..., recursive = FALSE) {
    if (!is.null(elementMetadata(x)))
        elementMetadata(x) <-
          do.call(rbind, lapply(c(list(x), ...), elementMetadata))
    x
}
setMethod("c", "Sequence",
          function(x, ..., recursive = FALSE)
          stop("missing 'c' method for Sequence class ", class(x)))

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Basic methods.
###

setReplaceMethod("[", "Sequence", function(x, i, j,..., value)
                 stop("attempt to modify the value of a ", class(x), " instance"))

setMethod("head", "Sequence",
          function(x, n = 6L, ...)
          {
              stopifnot(length(n) == 1L)
              if (n < 0L)
                  n <- max(length(x) + n, 0L)
              else
                  n <- min(n, length(x))
              if (n == 0L)
                  x[integer(0)]
              else
                  window(x, 1L, n)
          })

setMethod("rep", "Sequence", function(x, times)
          x[rep.int(seq_len(length(x)), times)])

setMethod("rev", "Sequence",
          function(x) {
              if (length(x) == 0)
                  x
              else
                  x[length(x):1]  
          })

setGeneric("seqextract", signature="x",
           function(x, start=NULL, end=NULL, width=NULL) standardGeneric("seqextract"))
  
setMethod("seqextract", "Sequence",
          function(x, start=NULL, end=NULL, width=NULL)
          {
              ir <- IRanges(start=start, end=end, width=width, names=NULL)
              if (any(start(ir) < 1L) || any(end(ir) > length(x)))
                  stop("some ranges are out of bounds")
              do.call(c,
                      lapply(seq_len(length(ir)),
                             function(i)
                                 window(x,
                                        start = start(ir)[i],
                                        width = width(ir)[i])))
          })

setMethod("seqextract", "vector",
          function(x, start=NULL, end=NULL, width=NULL)
          {
              ir <- IRanges(start=start, end=end, width=width, names=NULL)
              .Call("vector_subsetbyranges", x, start(ir), width(ir), PACKAGE="IRanges")
          })

setMethod("subset", "Sequence",
          function (x, subset, ...) 
          {
              if (!is.logical(subset)) 
                  stop("'subset' must be logical")
              x[subset & !is.na(subset)]
          })

setMethod("tail", "Sequence",
          function(x, n = 6L, ...)
          {
              stopifnot(length(n) == 1L)
              xlen <- length(x)
              if (n < 0L) 
                  n <- max(xlen + n, 0L)
              else
                  n <- min(n, xlen)
              if (n == 0L)
                  x[integer(0)]
              else
                  window(x, xlen - n + 1L, xlen)
          })

### Returns an IRanges instance of length 1.
### Not exported.
solveWindowSEW <- function(seq_length, start, end, width)
{
    solved_SEW <-
      try(solveUserSEW(seq_length, start=start, end=end, width=width),
          silent = TRUE)
    if (is(solved_SEW, "try-error"))
        stop("Invalid sequence coordinates.\n",
             "  Please make sure the supplied 'start', 'end' and 'width' arguments\n",
             "  are defining a region that is within the limits of the sequence.")
    solved_SEW
}

setMethod("window", "Sequence",
          function(x, start = NULL, end = NULL, width = NULL,
                   frequency = NULL, delta = NULL, ...)
          {
              if (is.null(frequency) && is.null(delta)) {
                  solved_SEW <-
                    solveWindowSEW(length(x),
                                   start = ifelse(is.null(start), NA, start),
                                   end = ifelse(is.null(end), NA, end),
                                   width = ifelse(is.null(width), NA, width))
                  x[as.integer(solved_SEW)]
              } else {
                  if (!is.null(width)) {
                      if (is.null(start))
                          start <- end - width + 1L
                      else if (is.null(end))
                          end <- start + width - 1L
                  }
                  idx <-
                    stats:::window.default(seq_len(length(x)), start = start, end = end,
                                           frequency = frequency, deltat = delta, ...)
                  attributes(idx) <- NULL
                  x[idx]
              }
          })

setMethod("window", "vector",
          function(x, start = NULL, end = NULL, width = NULL,
                  frequency = NULL, delta = NULL, ...)
          {
              if (is.null(frequency) && is.null(delta)) {
                  solved_SEW <-
                    solveWindowSEW(length(x),
                                   start = ifelse(is.null(start), NA, start),
                                   end = ifelse(is.null(end), NA, end),
                                   width = ifelse(is.null(width), NA, width))
                  .Call("vector_subsetbyranges",
                        x, start(solved_SEW), width(solved_SEW),
                        PACKAGE="IRanges")
               } else {
                  if (!is.null(width)) {
                      if (is.null(start))
                          start <- end - width + 1L
                      else if (is.null(end))
                          end <- start + width - 1L
                  }
                  idx <-
                    stats:::window.default(seq_len(length(x)), start = start, end = end,
                                           frequency = frequency, deltat = delta, ...)
                  attributes(idx) <- NULL
                  x[idx]
              }
          })

### Maybe this is how `!=` should have been defined in the base package so
### nobody would ever need to bother implementing such an obvious thing.
setMethod("!=", signature(e1="Sequence", e2="Sequence"),
    function(e1, e2) !(e1 == e2)
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Looping methods.
###

.aggregateInternal <-
function(x, by, FUN, start = NULL, end = NULL, width = NULL,
         frequency = NULL, delta = NULL, ..., simplify = TRUE)
{
    FUN <- match.fun(FUN)
    if (!missing(by)) {
        start <- start(by)
        end <- end(by)
    } else {
        if (!is.null(width)) {
            if (is.null(start))
                start <- end - width + 1L
            else if (is.null(end))
                end <- start + width - 1L
        }
        start <- as.integer(start)
        end <- as.integer(end)
    }
    if (length(start) != length(end))
        stop("'start', 'end', and 'width' arguments have unequal length")
    n <- length(start)
    if (is.null(frequency) && is.null(delta)) {
        sapply(seq_len(n), function(i)
               FUN(window(x, start = start[i], end = end[i]), ...),
               simplify = simplify)
    } else {
        frequency <- rep(frequency, length.out = n)
        delta <- rep(delta, length.out = n)
        sapply(seq_len(n), function(i)
               FUN(window(x, start = start[i], end = end[i],
                   frequency = frequency[i], delta = delta[i]),
                   ...),
               simplify = simplify)
    }
}

setMethod("aggregate", "Sequence", .aggregateInternal)

setMethod("aggregate", "vector", .aggregateInternal)

.shiftApplyInternal <-
function(SHIFT, X, Y, FUN, ..., OFFSET = 0L, simplify = TRUE, verbose = FALSE)
{
    FUN <- match.fun(FUN)
    N <- length(X)
    if (N != length(Y))
        stop("'X' and 'Y' must be of equal length")
    
    if (length(SHIFT) == 0 || !is.numeric(SHIFT) ||
            any(is.na(SHIFT)) || any(SHIFT < 0))
        stop("all 'SHIFT' values must be non-negative")
    SHIFT <- as.integer(SHIFT)
    
    if (length(OFFSET) == 0 || !is.numeric(OFFSET) ||
            any(is.na(OFFSET)) || any(OFFSET < 0))
        stop("'OFFSET' must be non-negative")
    OFFSET <- as.integer(OFFSET)
    
    ## Perform X setup
    shiftedStartX <- rep.int(1L + OFFSET, length(SHIFT))
    shiftedEndX <- N - SHIFT
    
    ## Perform Y setup
    shiftedStartY <- 1L + SHIFT
    shiftedEndY <- rep.int(N - OFFSET, length(SHIFT))
    
    if (verbose) {
        maxI <- length(SHIFT)
        ans <-
          sapply(seq_len(length(SHIFT)),
                 function(i) {
                     cat("\r", i, "/", maxI)
                     FUN(window(X, start = shiftedStartX[i], end = shiftedEndX[i]),
                         window(Y, start = shiftedStartY[i], end = shiftedEndY[i]),
                         ...)
                 }, simplify = simplify)
        cat("\n")
    } else {
        ans <-
          sapply(seq_len(length(SHIFT)),
                 function(i)
                     FUN(window(X, start = shiftedStartX[i], end = shiftedEndX[i]),
                         window(Y, start = shiftedStartY[i], end = shiftedEndY[i]),
                         ...),
                 simplify = simplify)
    }
    ans
}

setGeneric("shiftApply", signature = c("X", "Y"),
           function(SHIFT, X, Y, FUN, ..., OFFSET = 0L, simplify = TRUE,
                    verbose = FALSE)
           standardGeneric("shiftApply"))

setMethod("shiftApply", signature(X = "Sequence", Y = "Sequence"),
          .shiftApplyInternal)

setMethod("shiftApply", signature(X = "vector", Y = "vector"),
          .shiftApplyInternal)
