### =========================================================================
### ValuedIRanges objects
### -------------------------------------------------------------------------

## For associating data with a Ranges instance
## Extends IRanges and delegates to an XDataFrame which stores the data

setClass("ValuedIRanges",
         representation(values = "XDataFrame"),
         contains = "IRanges")

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Accessor methods.
###

setGeneric("values", function(object, ...) standardGeneric("values"))
setMethod("values", "ValuedIRanges", function(object) object@values)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Validity.
###

.valid.ValuedIRanges <- function(x)
{
  ## lengths of objects in 'data' should equal length of Ranges
  if (nrow(values(x)) != length(x))
    "All data elements must have lengths matching that of the Ranges instance"
}

setValidity2("ValuedIRanges", .valid.ValuedIRanges)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Constructor.
###

ValuedIRanges <- function(ranges = IRanges(), ...) {
  if (!is(ranges, "IRanges"))
    stop("'ranges' must be an IRanges instance")
  data_list <- list(...)
  if (!all(lapply(data_list, length) == length(ranges)))
    stop("All arguments in '...' must have lengths matching that of 'ranges'")
  ## TODO: move to XDataFrame()
  ##mc <- as.list(match.call())[-1]
  ##mc <- mc[names(mc) != "ranges"]
  ##argdp <- lapply(mc, deparse)
  ##emptynames <- nchar(names(data_list)) == 0
  ##names(data_list)[emptynames] <- make.names(argdp[emptynames], TRUE)
  values <- do.call("XDataFrame", data_list)
  new("ValuedIRanges", ranges, values = values)
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Subsetting.
###

setMethod("[[", "ValuedIRanges",
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
            if (i < 1L || i > length(x))
              stop("subscript out of bounds")
            values(x)[[i]]
          }
          )

setReplaceMethod("[[", "ValuedIRanges",
                 function(x, i, j,..., value)
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
                   if (is.numeric(i) && (i < 1L || i > ncol(values(x))+1))
                     stop("subscript out of bounds")
                   if (length(range(x)) != length(value)) {
                     if (length(value) < 1)
                       stop("data length is positive, 'value' length is 0")
                     if (length(range(x)) %% length(value) > 0)
                       stop("data length is zero not a multiple of replacement",
                            " length")
                     value <- rep(value, length = length(range(x)))
                   }
                   values(x)[[i]] <- value
                   value
                 })

### Supported 'i' types: numeric vector, logical vector, NULL and missing.
setMethod("[", "ValuedIRanges",
          function(x, i, j, ..., drop)
          {
            if (missing(i) && missing(j))
              return(x)
            if (missing(i))
              i <- seq(length(x))
            if (missing(j))
              j <- seq(ncol(values(x)))
            if (length(list(...)) > 0)
              stop("parameters in '...' not supported")
            checkIndex <- function(i) {
              if (!is.atomic(i))
                stop("invalid subscript type")
              if (is.character(i))
                stop("cannot subset a ", class(x), " object by names")
              lx <- length(x)
              if (any(is.na(i)))
                stop("subscript contains NAs")
              if (is.numeric(i)) {
                if (any(i < -lx) || any(i > lx))
                  stop("subscript out of bounds")
                if (any(i < 0) && any(i > 0))
                  stop("negative and positive indices cannot be mixed")
              } else if (is.logical(i)) {
                if (length(i) > lx)
                  stop("subscript out of bounds")
              } else if (!is.null(i)) {
                stop("invalid subscript type")
              }
            }
            checkIndex(i)
            checkIndex(j)
            slot(x, "ranges", check=FALSE) <- ranges(x)[i]
            x@values <- values(x)[i,j]
            x
          }
          )

setReplaceMethod("[", "ValuedIRanges",
                 function(x, i, j,..., value)
                 stop("attempt to modify the value of a ", class(x),
                      " instance")
                 )

### Lists of ValuedIRanges instances

setClass("ValuedIRangesList", contains = "IRangesList")
setMethod("elementClass", "ValuedIRangesList", function(x) "ValuedIRanges")
