### =========================================================================
### RangesCollection objects
### -------------------------------------------------------------------------

## Accepts any type of Ranges instance as an element

setClass("RangesCollection",
         representation(
                        range_list="list",   # a list of Ranges objects
                        NAMES="characterORNULL"  # R doesn't like @names !!
                        ),
         prototype(
                   range_list=list(),
                   NAMES=NULL
                   )
         )

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Accessor methods.
###


setGeneric("range_list", function(x) standardGeneric("range_list"))
setMethod("range_list", "RangesCollection", function(x) x@range_list)

setMethod("length", "RangesCollection", function(x) length(range_list(x)))

setMethod("names", "RangesCollection", function(x) x@NAMES)

### The only replacement method for RangesCollection objects!
setReplaceMethod("names", "RangesCollection",
                 function(x, value)
                 {
                   if (is.null(value)) {
                     x@NAMES <- NULL
                     return(x)
                   }
                   if (!is.character(value))
                     stop("'value' must be NULL or a character vector")
                   if (length(value) > length(x))
                     stop("number of names ",
                          length(value), " greater than number of ranges",
                          length(x))
                   if (length(value) < length(x))
                     value <- c(value, rep(NA, length(x) - length(value)))
                   x@NAMES <- value
                   x
                 }
                 )


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Validity.
###

## allow subclasses to provide the required subclass of Ranges in the list

setGeneric("rangeClass", function(x, ...) standardGeneric("rangeClass"))

setMethod("rangeClass", "RangesCollection", function(x) "Ranges")


.valid.RangesCollection.range_list <- function(x)
{
  if (!is.list(range_list(x))
      || !all(sapply(range_list(x), is, rangeClass(x))))
    return(paste("the 'range_list' slot must contain a list of",
                 rangeClass(x), "objects"))
  NULL
}

.valid.RangesCollection.names <- function(x)
{
  if (!is(x@NAMES, "characterORNULL"))
    return("the 'NAMES' slot must be NULL or contain a character vector")
  if (is.null(names(x)))
    return(NULL)
  if (any(is.na(names(x))))
    return("the names must be non-NA strings")
  if (length(names(x)) != length(x))
    return("number of names and number of elements differ")
  NULL
}

.valid.RangesCollection <- function(x)
{
  c(.valid.RangesCollection.range_list(x),
    .valid.RangesCollection.names(x))
}

setValidity2("RangesCollection", .valid.RangesCollection)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Constructor.
###

RangesCollection <- function(...)
{
  ranges <- list(...)
  new("RangesCollection", range_list=ranges, NAMES=names(ranges))
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "isEmpty" methods.
###

setMethod("isEmpty", "RangesCollection",
          function(x)
          {
            if (length(x) == 0)
              return(logical(0))
            sapply(range_list(x), isEmpty)
          }
          )


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "max" and "min" methods.
###

setMethod("max", "RangesCollection",
          function(x, ..., na.rm)
          {
            if (length(x) == 0)
              return(integer(0))
            sapply(range_list(x), max)
          }
          )

setMethod("min", "RangesCollection",
          function(x, ..., na.rm)
          {
            if (length(x) == 0)
              return(integer(0))
            sapply(range_list(x), min)
          }
          )

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Subsetting.
###

### Extract the i-th element of a RangesCollection object as a NormalIRanges
### object.
### Supported 'i' types: numeric vector of length 1.
setMethod("[[", "RangesCollection",
          function(x, i, j, ...)
          {
            if (!missing(j) || length(list(...)) > 0)
              stop("invalid subsetting")
            if (missing(i))
              stop("subscript is missing")
            if (is.character(i))
              stop("cannot subset a ", class(x), " object by names")
            if (!is.numeric(i))
              stop("invalid subscript type")
            if (length(i) < 1L)
              stop("attempt to select less than one element")
            if (length(i) > 1L)
              stop("attempt to select more than one element")
            if (is.na(i))
              stop("subscript cannot be NA")
            if (i < 1L || i > length(x))
              stop("subscript out of bounds")
            range_list(x)[[i]]
          }
          )

setReplaceMethod("[[", "RangesCollection",
                 function(x, i, j,..., value)
                 {
                   stop("attempt to modify the value of a ", class(x), " instance")
                 }
                 )

### Supported 'i' types: numeric vector, logical vector, NULL and missing.
setMethod("[", "RangesCollection",
          function(x, i, j, ..., drop)
          {
            if (!missing(j) || length(list(...)) > 0)
              stop("invalid subsetting")
            if (missing(i))
              return(x)
            if (!is.atomic(i))
              stop("invalid subscript type")
            if (is.character(i))
              stop("cannot subset a ", class(x), " object by names")
            lx <- length(x)
            if (is.numeric(i)) {
              if (any(is.na(i)))
                stop("subscript contains NAs")
              if (any(i < -lx) || any(i > lx))
                stop("subscript out of bounds")
            } else if (is.logical(i)) {
              if (any(is.na(i)))
                stop("subscript contains NAs")
              if (length(i) > lx)
                stop("subscript out of bounds")
            } else if (!is.null(i)) {
              stop("invalid subscript type")
            }
            slot(x, "range_list", check=FALSE) <- range_list(x)[i]
            if (!is.null(names(x)))
              slot(x, "NAMES", check=FALSE) <- names(x)[i]
            x
          }
          )

setReplaceMethod("[", "RangesCollection",
                 function(x, i, j,..., value)
                 stop("attempt to modify the value of a ", class(x), " instance")
                 )


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "append" method.
###

setMethod("append", "RangesCollection",
          function(x, values, after=length(x))
          {
            if (!is(values, "RangesCollection"))
              stop("'values' must be a RangesCollection object")
            if (!isSingleNumber(after))
              stop("'after' must be a single number")
            ans_range_list <- append(range_list(x), range_list(values),
                                     after=after)
            nm1 <- names(x)
            nm2 <- names(values)
            if (is.null(nm1) && is.null(nm2)) {
              ans_NAMES <- NULL
            } else {
              if (is.null(nm1))
                nm1 <- rep.int("", length(x))
              if (is.null(nm2))
                nm2 <- rep.int("", length(values))
              ans_NAMES <- append(nm1, nm2, after=after)
            }
            ## This transformation must be atomic.
            x@range_list <- ans_range_list
            x@NAMES <- ans_NAMES
            x
          }
          )


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Some useful endomorphisms: "reduce" and "gaps".
###

### 'with.inframe.attrib' is ignored.
setMethod("reduce", "RangesCollection",
          function(x, with.inframe.attrib=FALSE)
          {
            range_list <- range_list(x)
            if (length(range_list) == 0) {
              nir1 <- new("NormalIRanges")
            } else {
              start1 <- unlist(lapply(range_list, start))
              width1 <- unlist(lapply(range_list, width))
              ranges <- new2("IRanges", start=start1, width=width1, check=FALSE)
              nir1 <- toNormalIRanges(ranges)
            }
            ## This transformation must be atomic.
            x@range_list <- list(nir1)
            x@NAMES <- NULL
            x
          }
          )

### 'start' and 'end' are ignored.
setMethod("gaps", "RangesCollection",
          function(x, start=NA, end=NA)
          {
            x@range_list <- lapply(range_list(x), gaps)
            x@NAMES <- NULL
            x
          }
          )


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion.
###

### From a RangesCollection object to a NormalIRanges object.
setAs("RangesCollection", "NormalIRanges",
      function(from) reduce(from)[[1]]
      )


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "show" method.
###

setMethod("show", "RangesCollection",
          function(object)
          {
            lo <- length(object)
            cat("  A ", class(object), " instance of length ", lo, "\n", sep="")
            ### TODO: show (some of the) ranges here
          }
          )

