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
    "the number of ranges must equal the number of rows in the data frame"
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
  ## row.names are just to ensure the XDataFrame has the correct row count
  values <- XDataFrame(..., row.names = seq_len(length(ranges)))
  rownames(values) <- NULL
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
            if (is.numeric(i) && !is.na(i) && (i < 1L || i > length(x)))
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
                   if (length(x) != length(value)) {
                     stop("length of 'value' must match the number of ranges")
                     ##if (length(value) < 1)
                     ##  stop("data length is positive, 'value' length is 0")
                     ##if (length(range(x)) %% length(value) > 0)
                     ##  stop("data not a multiple of replacement length")
                     ##value <- rep(value, length = length(range(x)))
                   }
                   x@values[[i]] <- value
                   x
                 })

### Supported 'i' types: numeric vector, logical vector, NULL and missing.
setMethod("[", "ValuedIRanges",
          function(x, i, j, ..., drop)
          {
            if (!missing(drop) || length(list(...)) > 0)
              warning("'drop' and parameters in '...' not supported")
            if (missing(i) && missing(j))
              return(x)
            if (missing(i))
              i <- seq(length(x))
            if (missing(j))
              j <- seq(ncol(values(x)))
            checkIndex <- function(i, row = FALSE) {
              if (!is.atomic(i))
                stop("invalid subscript type")
              lx <- length(x)
              if (!is.null(i) && any(is.na(i)))
                stop("subscript contains NAs")
              if (is.numeric(i)) {
                if (any(i < -lx) || any(i > lx))
                  stop("subscript out of bounds")
                if (any(i < 0) && any(i > 0))
                  stop("negative and positive indices cannot be mixed")
              } else if (is.logical(i)) {
                if (length(i) > lx)
                  stop("subscript out of bounds")
              } else if (is.character(i)) {
                if (row) 
                  stop("cannot subset a ", class(x), " object by names")
              } else if (!is.null(i)) {
                stop("invalid subscript type")
              }
            }
            checkIndex(i, TRUE)
            checkIndex(j)
            x <- callNextMethod(x, i) # subset the ranges
            x@values <- values(x)[i,j,drop=FALSE] # subset the data values
            x
          })

setReplaceMethod("[", "ValuedIRanges",
                 function(x, i, j,..., value)
                 stop("attempt to modify the value of a ", class(x),
                      " instance")
                 )

### Lists of ValuedIRanges instances

setClass("ValuedIRangesList", contains = "IRangesList")
setMethod("elementClass", "ValuedIRangesList", function(x) "ValuedIRanges")

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion.
###

setMethod("as.data.frame", "ValuedIRanges",
          function(x, row.names=NULL, optional=FALSE, ...)
          {
            if (!(is.null(row.names) || is.character(row.names)))
              stop("'row.names' must be NULL or a character vector")

            cbind(callNextMethod(), as.data.frame(values(x)))
          })
