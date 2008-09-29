### =========================================================================
### RangedData objects
### -------------------------------------------------------------------------

## For keeping data with your ranges

setClass("RangedData",
         representation(ranges = "RangesORXRanges", values = "ANY"))


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Accessor methods.
###

setGeneric("values", function(object, ...) standardGeneric("values"))
setMethod("values", "RangedData", function(object) object@values)

## coerced to internal ranges
setGeneric("ranges", function(object, ...) standardGeneric("ranges"))
setMethod("ranges", "RangedData",
          function(object, asRanges = TRUE) {
            if (!isTRUEorFALSE(asRanges))
              stop("'asRanges' should be TRUE or FALSE")
            if (asRanges)
              as(object@ranges, "Ranges")
            else object@ranges
          })

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Validity.
###

.valid.RangedData <- function(x)
{
  if (length(ranges(x, FALSE)) != NROW(values(x)))
    "the number of ranges must equal the number of records in the data"
  else NULL
}

setValidity2("RangedData", .valid.RangedData)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Constructor.
###

RangedData <- function(ranges = IRanges(), values = NULL) {
  if (!is(ranges, "RangesORXRanges"))
    stop("'ranges' must be either a Ranges or XRanges instance")
  if (length(ranges) != NROW(values))
    stop("lengths of 'ranges' and 'values' must be equal")
  new("RangedData", ranges = ranges, values = values)
}

## This is a convenience that sticks the arguments into an XDataFrame

RangedDataFrame <- function(ranges = IRanges(), ...) {
  RangedData(ranges, XDataFrame(...))
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Subsetting.
###

setMethod("[[", "RangedData",
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
            if (is.numeric(i) && !is.na(i) && (i < 1L || i > length(values(x))))
              stop("subscript out of bounds")
            values(x)[[i]]
          }
          )

setReplaceMethod("[[", "RangedData",
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
                   if (is.numeric(i) && (i < 1L || i > length(values(x))+1))
                     stop("subscript out of bounds")
                   ##if (!is.null(value) &&
                   ##    length(values(x)[[1]]) != length(value)) {
                   ##  stop("invalid replacement length")
                     ##if (length(value) < 1)
                     ##  stop("data length is positive, 'value' length is 0")
                     ##if (length(range(x)) %% length(value) > 0)
                     ##  stop("data not a multiple of replacement length")
                     ##value <- rep(value, length = length(range(x)))
                   ##}
                   x@values[[i]] <- value
                   x
                 })

### Supported 'i' types: numeric vector, logical vector, NULL and missing.
setMethod("[", "RangedData",
          function(x, i, j, ..., drop)
          {
            if (!missing(drop) || length(list(...)) > 0)
              stop("'drop' and parameters in '...' not supported")
            if (missing(i) && missing(j))
              return(x)
            checkIndex <- function(i, row = FALSE) {
              if (!is.atomic(i))
                stop("invalid subscript type")
              if (row)
                lx <- NROW(values(x))
              else lx <- NCOL(values(x))
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
            values <- values(x)
            array <- is.array(values) || is.data.frame(values)
            mstyle <- nargs() == 3
            if (mstyle && !array)
              stop("matrix-style subsetting not allowed for non-array values")
            if (!missing(j))
              checkIndex(j)
            if (!missing(i)) {
              checkIndex(i, TRUE)
              x@ranges <- ranges(x)[i] ### NOTE: this brings XRanges into R
              if (!mstyle) {
                if (array) # subset array data by rows
                  x@values <- values[i,,drop=FALSE]
                else x@values <- values[i] # subset non-array data
              }
            }
            if (mstyle) { # subset array data, matrix-style
              if (missing(i))
                x@values <- values[,j,drop=FALSE]
              else if (missing(j))
                x@values <- values[i,]
              else x@values <- values[i,j,drop=FALSE]
            }
            x
          })

setReplaceMethod("[", "RangedData",
                 function(x, i, j,..., value)
                 stop("attempt to modify the value of a ", class(x),
                      " instance")
                 )

### Lists of RangedData instances

setClass("RangedDataList", contains = "TypedList")
setMethod("elementClass", "RangedDataList", function(x) "RangedData")

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion
###

setMethod("as.data.frame", "RangedData",
          function(x, row.names=NULL, optional=FALSE, ...)
          {
            if (!(is.null(row.names) || is.character(row.names)))
              stop("'row.names'  must be NULL or a character vector")
            if (!missing(optional) || length(list(...)))
              warning("'optional' and arguments in '...' ignored")
            r <- ranges(x)
            data.frame(as.data.frame(r), as.data.frame(values(x)),
                       row.names = row.names)
          })

setAs("RangedData", "rle", function(from) {
  rlencode(from)
})
