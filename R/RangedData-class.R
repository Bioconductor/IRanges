### =========================================================================
### RangedData objects
### -------------------------------------------------------------------------

## For keeping data with your ranges

## There are two design aims:
## 1) Efficiency when data is large (i.e. apply by chromosome)
## 2) Convenience when data is not so large (i.e. unrolling the data)
setClass("RangedData",
         representation(ranges = "RangesList", values = "SplitXDataFrame",
                        annotation = "characterORNULL"))

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Accessor methods.
###

setGeneric("values", function(object, ...) standardGeneric("values"))
setMethod("values", "RangedData", function(object) object@values)

setMethod("ranges", "RangedData", function(object) object@ranges)

setGeneric("annotation", function(object) standardGeneric("annotation"))
setMethod("annotation", "RangedData", function(object) object@annotation)

## range delegates
setMethod("start", "RangedData", function(x) start(ranges(x)))
setMethod("end", "RangedData", function(x) end(ranges(x)))
setMethod("width", "RangedData", function(x) width(ranges(x)))
setMethod("length", "RangedData", function(x) length(ranges(x)))
setMethod("names", "RangedData", function(x) names(ranges(x)))

## values delegates
setMethod("dim", "RangedData",
          function(x) {
            dim(values(x))
          })
setMethod("dimnames", "RangedData",
          function(x) {
            dimnames(values(x))
          })

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Validity.
###

.valid.RangedData.ranges <- function(x)
{
  if (!identical(lapply(ranges(x), length), lapply(values(x), nrow)))
    "the number of ranges must equal the number of rows"
  else if (!identical(unlist(lapply(ranges(x), names)), rownames(x)))
    "the names of the ranges must equal the rownames"
  else NULL
}

.valid.RangedData <- function(x) {
  c(.valid.RangedData.ranges(x))
}

setValidity2("RangedData", .valid.RangedData)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Constructor.
###

## creates a single-element RangedData (unless splitter is specified)

### FIXME: when splitter specified, we make one copy too many
RangedData <- function(ranges = IRanges(), ..., splitter = NULL,
                       annotation = NULL)
{
  if (!is(ranges, "RangesORXRanges"))
    stop("'ranges' must be a Ranges or XRanges instance")
  if (((nargs() - !missing(splitter)) - !missing(annotation)) > 1) 
    values <- XDataFrame(...) ## at least one column specified
  else values <- new("XDataFrame", nrows = length(ranges))
  if (length(ranges) != nrow(values))
    stop("lengths of 'ranges' and elements in '...' must be equal")
  rownames(values) <- names(ranges) ## ensure these are identical
  if (!is.null(splitter)) {
    if (length(splitter) != length(ranges))
      stop("length of 'splitter' (if non-NULL) must match length of 'ranges'")
    ranges <- split(ranges, splitter)
    values <- split(values, splitter)
  } else {
    ranges <- RangesList(ranges)
    values <- SplitXDataFrame(values)
  }
  if (!is.null(annotation) && !isSingleString(annotation))
    stop("'annotation' must be a single string")
  new("RangedData", ranges = ranges, values = values, annotation = annotation)
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Subsetting.
###

## The extraction operator delegates to the values (extracts columns)

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
            if (is.numeric(i) && !is.na(i) && (i < 1L || i > ncol(x)))
              stop("subscript out of bounds")
            unlist(lapply(values(x), `[[`, i), use.names=FALSE)
          })

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
                   if (is.numeric(i) && (i < 1L || i > ncol(x)+1))
                     stop("subscript out of bounds")
                   if (!is.null(value) && nrow(x) != length(value))
                     stop("invalid replacement length")
                   values <- values(x)
                   nrows <- sapply(elements(values(x)), nrow)
                   spaces <- rep(seq_len(length(x)), nrows)
                   svalues <- NULL
                   if (!is.null(value))
                     svalues <- split(value, spaces)
                   for (j in seq_len(length(x))) {
                     values[[j]][[i]] <- svalues[[j]]
                   }
                   x@values <- values
                   x
                 })

### Supported index types: numeric, logical, character, NULL and missing.
## Two index modes:
## - list style ([i]):  subsets by range space (e.g. chromosome)
## - matrix style ([i,j]): subsets the data frame
setMethod("[", "RangedData",
          function(x, i, j, ..., drop)
          {
            if (!missing(drop) || length(list(...)) > 0)
              stop("'drop' and parameters in '...' not supported")
            if (missing(i) && missing(j))
              return(x)
            checkIndex <- function(i, lx) {
              if (!is.atomic(i))
                stop("invalid subscript type")
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
            if (mstyle) {
              if (!missing(j))
                checkIndex(j, ncol(x))
              else j <- seq_len(ncol(x))
              if (!missing(i)) {
                checkIndex(i, nrow(x))
                dummy <- seq_len(nrow(x))
                names(dummy) <- rownames(x)
                i <- dummy[i]
                if (any(is.na(i))) ## cannot subset by NAs yet
                  stop("invalid rownames specified")
              } else i <- seq_len(nrow(x))
              w <- cumsum(c(1, sapply(elements(values(x)), nrow)))
              si <- split(i, factor(findInterval(i, w), seq_along(w)))
              for (k in seq_len(length(x))) {
                values[[k]] <- values[[k]][si[[k]] - w[k] + 1, j, drop=FALSE]
                ranges[[k]] <- ranges[[k]][si[[k]] - w[k] + 1]
              }
            } else if (!missing(i)) {
              checkIndex(i, length(x))
              ranges <- ranges[i]
              values <- values[i]
            }
            x@ranges <- ranges
            x@values <- values
            x
          })

setReplaceMethod("[", "RangedData",
                 function(x, i, j,..., value)
                 stop("operation not supported")
                 )

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Combining and splitting.
###

setMethod("c", "RangedData", function(x, ..., recursive = FALSE) {
  if (recursive)
    stop("'recursive' mode not supported")
  rds <- list(...)
  if (!missing(x))
    rds <- c(list(x), rds)
  rd <- rds[[1]]
  if (!all(sapply(rds, is, "RangedData")))
    stop("all arguments in '...' must be instances of RangedData")
  rd@ranges <- do.call("c", lapply(rds, ranges))
  rdvalues <- do.call("c", lapply(rds, values))
  rd
})

setMethod("split", "RangedData", function(x, f, drop = FALSE) {
  splitInd <- split(seq_len(length(x)), f)
  do.call("RangedDataList", lapply(splitInd, function(ind) x[ind,]))
})

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
            data.frame(as.data.frame(ranges(x)),
                       as.data.frame(values(x)), row.names = row.names)
          })

setAs("RangedData", "XDataFrame",
      function(from)
      {
        XDataFrame(as.data.frame(ranges(from)), values(from))
      })

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Show
###

setMethod("show", "RangedData", function(object) {
  cat("A RangedData object with", ncol(object), "cols on", nrow(object),
      "ranges in", length(object), "spaces\n")
})

### =========================================================================
### RangedDataList objects
### -------------------------------------------------------------------------

### Lists of RangedData instances

setClass("RangedDataList", prototype = prototype(elementClass = "RangedData"),
         contains = "TypedList")

RangedDataList <- function(...)
{
  rds <- list(...)
  NAMES <- names(rds)
  names(rds) <- NULL
  new("RangedData", elements=rds, NAMES=NAMES)
}

