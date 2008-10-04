### =========================================================================
### RangedData objects
### -------------------------------------------------------------------------

## For keeping data with your ranges

setClass("RangedData",
         representation(ranges = "RangesList", values = "XDataFrame",
                        design = "XDataFrame", annotation = "characterORNULL"))

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Accessor methods.
###

setGeneric("values", function(object, ...) standardGeneric("values"))
setMethod("values", "RangedData", function(object) object@values)

setGeneric("design", function(object, ...) standardGeneric("design"))
setMethod("design", "RangedData", function(object) object@design)

setMethod("ranges", "RangedData", function(object) object@ranges)

setGeneric("annotation", function(object) standardGeneric("annotation"))
setMethod("annotation", "RangedData", function(object) object@annotation)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Validity.
###

.valid.values <- function(x)
{
  if (sum(width(ranges(x))) != nrow(values(x)))
    "the total number of ranges must equal the number of rows in 'values'"
  else NULL
}

.valid.design <- function(x) {
  if (nrow(design(x)) != length(ranges(x)))
    "the number of Ranges instances must equal the number of rows in 'design'"
  else if (!all(rownames(design(x)) == rownames(ranges(x))))
    "the rownames of 'design' must match the names of 'ranges'"
  else NULL
}

.valid.RangedData <- function(x) {
  c(.valid.values(x), .valid.design(x))
}

setValidity2("RangedData", .valid.RangedData)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Constructor.
###

RangedData <- function(ranges = RangesList(), ..., design = XDataFrame()) {
  if (is(ranges, "RangesORXRanges"))
    ranges <- RangesList(ranges)
  if (!is(ranges, "RangesList"))
    stop("'ranges' must be a RangesList instance")
  if (!canCoerce(design, "XDataFrame"))
    stop("cannot coerce 'design' to an XDataFrame")
  design <- XDataFrame(design, row.names = names(ranges))
  if (nrow(design) != length(ranges))
    stop("number of rows in 'design' must match the length of 'ranges'")
  values <- XDataFrame(...)
  if (length(ranges) != nrow(values))
    stop("lengths of 'ranges' and elements in '...' must be equal")
  new("RangedData", ranges = ranges, values = values, design = design)
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
                   if (!is.null(value) && nrow(values(x)) != length(value))
                     stop("invalid replacement length")
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
                lx <- nrow(values(x))
              else lx <- ncol(values(x))
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
              } else if (!is.null(i)) {
                stop("invalid subscript type")
              }
            }
            values <- values(x)
            ranges <- ranges(x)
            mstyle <- nargs() == 3
            if (!missing(j))
              checkIndex(j)
            if (!missing(i)) {
              checkIndex(i, TRUE)
              rind <- c(1, cumsum(width(ranges)) + 1)
              names(rind) <- names(ranges)
              rind <- rind[i]
              x@ranges <- ranges[i]
              rw <- width(ranges(x))
              dind <- rep(rind, rw) + unlist(lapply(rw, seq_len))
              if (!mstyle)
                x@values <- values[dind,,drop=FALSE]
            }
            if (mstyle) { # subset array data, matrix-style
              if (missing(i))
                x@values <- values[,j,drop=FALSE]
              else if (missing(j))
                x@values <- values[dind,]
              else x@values <- values[dind,j,drop=FALSE]
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
            as.data.frame(XDataFrame(x, row.names = row.names))
          })

setAs("RangedData", "XDataFrame",
      function(from)
      {
        rl <- ranges(from)
        XDataFrame(unlist(rl), design(from)[rep(seq_len(rl), width(rl))],
                   values(from))
      })

setAs("RangedData", "rle", function(from) {
  rlencode(from)
})
