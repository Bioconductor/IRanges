### =========================================================================
### RangedData objects
### -------------------------------------------------------------------------

## For keeping data with your ranges

## There are two design aims:
## 1) Efficiency when data is large (i.e. apply by chromosome)
## 2) Convenience when data is not so large (i.e. unrolling the data)
setClass("RangedData",
         representation(ranges = "RangesList", values = "SplitXDataFrame"))

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Accessor methods.
###

setGeneric("values", function(x, ...) standardGeneric("values"))
setMethod("values", "RangedData", function(x) x@values)

setGeneric("ranges", function(x, ...) standardGeneric("ranges"))
setMethod("ranges", "RangedData", function(x) x@ranges)

## range delegates
setMethod("start", "RangedData", function(x) start(ranges(x)))
setMethod("end", "RangedData", function(x) end(ranges(x)))
setMethod("width", "RangedData", function(x) width(ranges(x)))
setMethod("length", "RangedData", function(x) length(ranges(x)))
setMethod("names", "RangedData", function(x) names(ranges(x)))
setReplaceMethod("names", "RangedData",
                 function(x, value) {
                   if (!is.null(value) && !is.character(value))
                     stop("'value' must be NULL or a character vector")
                   names(x@ranges) <- value
                   names(x@values) <- value
                   x
                 })

setMethod("space", "RangedData", function(x) space(ranges(x)))
setMethod("universe", "RangedData", function(x) universe(ranges(x)))
setReplaceMethod("universe", "RangedData",
                 function(x, value) {
                   universe(x@ranges) <- value
                   x
                 })

setMethod("range", "RangedData", function(x, ..., na.rm) {
  args <- list(x, ...)
  rangeLists <- lapply(args, ranges)
  do.call("range", rangeLists)
})

## values delegates
setMethod("dim", "RangedData",
          function(x) {
            dim(values(x))
          })
setMethod("dimnames", "RangedData",
          function(x) {
            dimnames(values(x))
          })
setReplaceMethod("dimnames", "RangedData",
          function(x, value) {
            browser()
            rn <- as.character(value[[1]])
            cn <- as.character(value[[2]])
            inds <- rep(seq_len(length(x@ranges)),
                        unlist(lapply(x@ranges, length)))
            if (is.null(value[[1]]))
              rns <- vector("list", length(inds))
            else rns <- split(rn, inds)
            values <- elements(values(x))
            ranges <- elements(ranges(x))
            for(i in seq_len(length(x@ranges))) {
              dimnames(values[[i]]) <- list(rns[[i]], cn)
              names(ranges[[i]]) <- rns[[i]]
            }
            x@values <- initialize(x@values, elements = values)
            x@ranges <- initialize(x@ranges, elements = ranges)
            x
          })


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Validity.
###

.valid.RangedData.ranges <- function(x)
{
  if (!identical(lapply(ranges(x), length), lapply(values(x), nrow)))
    "'ranges' and 'values' must be of the same length and have the same names"
  else if (!identical(unlist(lapply(ranges(x), names), use.names=FALSE),
                      rownames(x)))
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

## creates a single-element RangedData (unless splitter (space) is specified)

RangedData <- function(ranges = IRanges(), ..., space = NULL,
                       universe = NULL)
{
  if (!is(ranges, "Ranges"))
    stop("'ranges' must be a Ranges or XRanges instance")
  if (((nargs() - !missing(space)) - !missing(universe)) > 1) 
    values <- XDataFrame(...) ## at least one column specified
  else values <- new("XDataFrame", nrows = length(ranges))
  if (length(ranges) != nrow(values))
    stop("lengths of 'ranges' and elements in '...' must be equal")
  rownames(values) <- names(ranges) ## ensure these are identical
  if (length(space) > 1) {
    if (length(space) != length(ranges)) {
      if (length(space) > length(ranges))
        stop("length of 'space' greater than length of 'ranges'")
      if (length(ranges) %% length(space) != 0)
        stop("length of 'ranges' not a multiple of 'space' length")
      space <- recycleVector(space, length(ranges))
    }
    ranges <- split(ranges, space)
    values <- split(values, space)
  } else {
    ranges <- RangesList(ranges)
    values <- SplitXDataFrame(values)
    if (length(space))
      names(ranges) <- names(values) <- space
  }
  if (!is.null(universe) && !isSingleString(universe))
    stop("'universe' must be a single string")
  universe(ranges) <- universe
  new("RangedData", ranges = ranges, values = values)
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
            col <- lapply(values(x), `[[`, i)
            names(col) <- NULL
            do.call("c", col) ## FIXME: broken for e.g. factors
          })

setMethod("$", "RangedData", function(x, name) x[[name]])

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
                   values <- elements(values(x))
                   nrows <- sapply(values, nrow)
                   inds <- seq_len(length(x))
                   spaces <- factor(rep(inds, nrows), inds)
                   svalues <- NULL
                   if (!is.null(value))
                     svalues <- split(value, spaces)
                   for (j in seq_len(length(x))) {
                     values[[j]][[i]] <- svalues[[j]]
                   }
                   x@values <- initialize(x@values, elements = values)
                   x
                 })

setReplaceMethod("$", "RangedData", function(x, name, value) {
  x[[name]] <- value
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
              } else if (is.character(i) || is.factor(i)) {
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
                if (is(i, "RangesList")) {
                  ### FIXME: not sure about this at all
                  i <- !is.na(unlist(overlap(i,ranges,multiple=FALSE)))
                }
                checkIndex(i, nrow(x))
                dummy <- seq_len(nrow(x))
                names(dummy) <- rownames(x)
                if (is.factor(i))
                  i <- as.character(i)
                i <- dummy[i]
                if (any(is.na(i))) ## cannot subset by NAs yet
                  stop("invalid rownames specified")
              } else i <- seq_len(nrow(x))
              values <- elements(values(x))
              ranges <- elements(ranges(x))
              w <- cumsum(c(1, sapply(values, nrow)))
              si <- split(i, factor(findInterval(i, w), seq_along(w)))
              for (k in seq_len(length(x))) {
                values[[k]] <- values[[k]][si[[k]] - w[k] + 1, j, drop=FALSE]
                ranges[[k]] <- ranges[[k]][si[[k]] - w[k] + 1]
              }
              ranges <- initialize(x@ranges, elements = ranges)
              values <- initialize(x@values, elements = values)
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
  nms <- lapply(rds, ## figure out names like 'c' on an ordinary vector
                function(rd) structure(logical(length(rd)), names = names(rd)))
  nms <- names(do.call("c", nms))
  names(rds) <- NULL # critical for dispatch to work
  ranges <- do.call("c", lapply(rds, ranges))
  names(ranges) <- nms
  rd@ranges <- ranges
  values <- do.call("c", lapply(rds, values))
  names(values) <- nms
  rd@values <- values
  rd
})

setMethod("split", "RangedData", function(x, f, drop = FALSE) {
  if (length(f) > nrow(x) || nrow(x) %% length(f) > 0)
    stop("nrow(x) is not a multiple of length(f)")
  splitInd <- split(seq_len(nrow(x)), f, drop)
  do.call("RangedDataList", lapply(splitInd, function(ind) x[ind,]))
})

setMethod("rbind", "RangedData", function(..., deparse.level=1) {
  args <- list(...)
  rls <- lapply(args, ranges)
  rl <- rls[[1]]
  if (!all(sapply(sapply(rls, universe), identical, universe(rl))))
    stop("All args in '...' must have the same universe as 'x'")
  dfs <- lapply(args, values)
  df <- dfs[[1]]
  nmsList <- lapply(args, names)
  if (any(sapply(nmsList, is.null))) {
    if (!all(unlist(lapply(args, length)) == length(args[[1]])))
      stop("If any args are missing names, all must have same length")
    nms <- seq_len(length(args[[1]]))
  } else nms <- unique(unlist(nmsList))
  for (nm in nms) {
    rl[[nm]] <- do.call("c", lapply(rls, `[[`, nm))
    df[[nm]] <- do.call("rbind", lapply(dfs, `[[`, nm))
  }
  initialize(args[[1]], ranges = rl, values = df)
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

setAs("XRle", "RangedData",
      function(from)
      {
        RangedData(successiveIRanges(as.integer(from@lengths)),
                   XDataFrame(score = from@values))
      })

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Show
###

setMethod("show", "RangedData", function(object) {
  nr <- nrow(object)
  cat("A", class(object), "object with", ncol(object), "cols on", nr,
      "ranges in", length(object), "sequences\n")
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
  new("RangedDataList", elements=rds, NAMES=NAMES)
}
