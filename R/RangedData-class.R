### =========================================================================
### RangedData objects
### -------------------------------------------------------------------------

## For keeping data with your ranges

## There are two design aims:
## 1) Efficiency when data is large (i.e. apply by chromosome)
## 2) Convenience when data is not so large (i.e. unrolling the data)

## The ranges are stored in a RangesList, while the data is stored in
## a SplitDataFrameList. The RangesList is uncompressed, because
## users will likely want to apply over each Ranges separately, as
## they are usually in separate spaces. Also, it is difficult to
## compress RangesLists, as lists containing Views or IntervalTrees
## are uncompressible. The SplitDataFrameList should be compressed,
## because it's cheap to create from a split factor and, more
## importantly, cheap to get and set columns along the entire dataset,
## which is common. Usually the data columns are atomic vectors and
## thus trivially compressed. It does, however, incur a slight
## performance penalty when applying over the RangedData.

### FIXME: probably needs own metadata slot
setClass("RangedData", contains = "DataTable",
         representation(ranges = "RangesList", values = "SplitDataFrameList"),
         prototype = prototype(ranges = new("SimpleRangesList"),
                               values = new("CompressedSplitDataFrameList")))

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Accessor methods.
###

setGeneric("values", function(x, ...) standardGeneric("values"))
setMethod("values", "RangedData", function(x) x@values)

setGeneric("ranges", function(x, ...) standardGeneric("ranges"))
setMethod("ranges", "RangedData", function(x) x@ranges)

setGeneric("ranges<-", function(x, ..., value) standardGeneric("ranges<-"))
setReplaceMethod("ranges", "RangedData",
                 function(x, value) {
                   if (extends(class(value), "RangesList")) {
                     if (!identical(lapply(ranges(x), names), lapply(value, names)))
                       stop("'value' must have same length and names as current 'ranges'")
                   } else if (extends(class(value), "IRanges")) {
                     values <- split(value, space(x))
                   } else {
                     stop("'value' must extend class RangesList or IRanges")
                   }
                   x@ranges <- value
                   x
                 })

## range delegates
setMethod("start", "RangedData", function(x) start(ranges(x)))
setMethod("end", "RangedData", function(x) end(ranges(x)))
setMethod("width", "RangedData", function(x) width(ranges(x)))
setReplaceMethod("start", "RangedData",
                 function(x, check=TRUE, value) {
                   start(ranges(x), check=check) <- value
                   x
                 })
setReplaceMethod("end", "RangedData",
                 function(x, check=TRUE, value) {
                   end(ranges(x), check=check) <- value
                   x
                 })
setReplaceMethod("width", "RangedData",
                 function(x, check=TRUE, value) {
                   width(ranges(x), check=check) <- value
                   x
                 })
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
  do.call(range, rangeLists)
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
            rn <- as.character(value[[1]])
            cn <- as.character(value[[2]])
            inds <- rep(seq_len(length(x@ranges)),
                        unlist(lapply(x@ranges, length)))
            if (is.null(value[[1]]))
              rns <- vector("list", length(inds))
            else {
              if (length(rn) != nrow(x))
                stop("invalid rownames length")
              rns <- split(rn, inds)
            }
            values <- unlist(values(x))
            dimnames(values) <- value
            ranges <- as.list(ranges(x))
            for(i in seq_len(length(x@ranges))) {
              ##dimnames(values[[i]]) <- list(rns[[i]], cn)
              names(ranges[[i]]) <- rns[[i]]
            }
            x@values <- split(values, inds)
            names(x@values) <- names(x)
              if (is(x@ranges, "CompressedList"))
                x@ranges <- newCompressedList(class(x@ranges), ranges)
              else
                x@ranges <- newSimpleList(class(x@ranges), ranges)
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
  hasDots <- (((nargs() - !missing(space)) - !missing(universe)) > 1)
  if (is(ranges, "RangesList")) {
    if (!is.null(space))
      warning("since 'class(ranges)' extends RangesList, 'space' argument is ignored")
    if (is.null(names(ranges)))
      space <- rep(as.character(seq_len(length(ranges))),
                   each = elementLengths(ranges))
    else
      space <- rep(names(ranges), each = elementLengths(ranges))
    N <- sum(elementLengths(ranges))
    NAMES <- unlist(lapply(ranges, names))
  } else if (!is(ranges, "Ranges")) {
    coerced <- try(as(ranges, "RangedData"), silent=TRUE)
    if (is(coerced, "RangedData"))
      return(coerced)
    stop("'ranges' must be a Ranges or directly coercible to RangedData")
  } else {
    N <- length(ranges)
    NAMES <- names(ranges)
  }
  if (hasDots) 
    values <- DataFrame(...) ## at least one column specified
  else
    values <- new2("DataFrame", nrows = N, check=FALSE)
  if (N != nrow(values)) {
    if (nrow(values) > N)
      stop("length of value(s) in '...' greater than length of 'ranges'")
    if (nrow(values) == 0 || N %% nrow(values) != 0)
      stop("length of 'ranges' not a multiple of length of value(s) in '...'")
    rind <- recycleVector(seq_len(nrow(values)), N)
    values <- values[rind,,drop=FALSE]
  }
  rownames(values) <- NAMES ## ensure these are identical
  if (length(space) > 1) {
    if (length(space) != N) {
      if (length(space) > N)
        stop("length of 'space' greater than length of 'ranges'")
      if (N %% length(space) != 0)
        stop("length of 'ranges' not a multiple of 'space' length")
      space <- recycleVector(space, N)
    }
    if (!is(ranges, "RangesList"))
      ranges <- split(ranges, space)
    values <- split(values, space)
  } else {
    if (!is(ranges, "RangesList"))
      ranges <- RangesList(ranges)
    values <- SplitDataFrameList(values, compress = TRUE)
    if (length(space) == 1)
      names(ranges) <- names(values) <- space
  }
  if (!is.null(universe) && !isSingleString(universe))
    stop("'universe' must be a single string")
  universe(ranges) <- universe
  new2("RangedData", ranges = ranges, values = values, check=FALSE)
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Subsetting.
###

## The extraction operator delegates to the values (extracts columns)

setMethod("[[", "RangedData",
          function(x, i, j, ...)
          {
            dotArgs <- list(...)
            if (length(dotArgs) > 0)
                dotArgs <- dotArgs[names(dotArgs) != "exact"]
            if (!missing(j) || length(dotArgs) > 0)
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
            if (is.na(i) || (is.character(i) && !(i %in% colnames(x))))
              return(NULL)
            ##col <- lapply(values(x), function(v) v[i])
            ##names(col) <- NULL ## use rbind() to handle factor levels
            ##do.call(rbind, col)[[1]]
            unlist(values(x))[[i]] # very fast if 'values' is compressed
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
                   ##values <- as.list(values(x))
                   nrows <- elementLengths(values(x))
                   inds <- seq_len(length(x))
                   spaces <- factor(rep.int(inds, nrows), inds)
                   ##svalues <- NULL
                   ##if (!is.null(value))
                   ##  svalues <- split(value, spaces)
                   ##for (j in seq_len(length(x))) {
                   ##  values[[j]][[i]] <- svalues[[j]]
                   ##}
                   ##x@values <- initialize(values(x), elements = values)
                   values <- unlist(values(x))
                   values[[i]] <- value
                   x@values <- split(values, spaces)
                   names(x@values) <- names(x)
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
          function(x, i, j, ..., drop=FALSE)
          {
            if (length(list(...)) > 0)
              stop("parameters in '...' not supported")
            if (missing(i) && missing(j))
              return(x)
            checkIndex <- function(i, lx, nms) {
              if (!is.atomic(i))
                return("invalid subscript type")
              if (!is.null(i) && any(is.na(i)))
                return("subscript contains NAs")
              if (is.numeric(i)) {
                if (any(i < -lx) || any(i > lx))
                  return("subscript out of bounds")
                if (any(i < 0) && any(i > 0))
                  return("negative and positive indices cannot be mixed")
              } else if (is.logical(i)) {
                if (length(i) > lx)
                  return("subscript out of bounds")
              } else if ((is.character(i) || is.factor(i))) {
                  i <- match(i, nms)
                  if (any(is.na(i)))
                    return("mismatching names")
              } else if (!is.null(i)) {
                return("invalid subscript type")
              }
              NULL
            }
            mstyle <- nargs() > 2
            if (mstyle) {
              if (!missing(j)) {
                prob <- checkIndex(j, ncol(x), colnames(x))
                if (!is.null(prob))
                  stop("selecting cols: ", prob)
              } else
                j <- seq_len(ncol(x))
              if (!missing(i)) {
                if (is(i, "RangesList")) {
                  i <- !is.na(unlist(overlap(i,ranges,multiple=FALSE)))
### FIXME: could do this if Ranges supported NAs, then ordering is possible
                  ##i <- overlap(ranges, i, multiple=FALSE, drop=TRUE)
                }
                prob <- checkIndex(i, nrow(x), rownames(x))
                if (!is.null(prob))
                  stop("selecting rows: ", prob)
                dummy <- seq_len(nrow(x))
                names(dummy) <- rownames(x)
                if (is.factor(i))
                  i <- as.character(i)
                i <- dummy[i]
                if (any(is.na(i))) ## cannot subset by NAs yet
                  stop("invalid rownames specified")
              } else {
                i <- seq_len(nrow(x))
              }
              values <- unlist(values(x))
              ranges <- as.list(ranges(x))
              w <- cumsum(c(1, sapply(ranges, length)))
              sf <- factor(findInterval(i, w), levels = seq_along(w),
                           labels = c(names(ranges), ""))
              si <- split(i, sf)
              values <- values[i, j, drop=FALSE]
              values <- split(values, sf, drop=TRUE)
              for (k in seq_len(length(x))) {
                ranges[[k]] <- ranges[[k]][si[[k]] - w[k] + 1]
              }
              if (drop) {
                whichDrop <- which(unlist(lapply(ranges, length)) == 0)
                if (length(whichDrop) > 0) {
                  ranges <- ranges[-whichDrop]
                }
              }
              if (is(x@ranges, "CompressedList"))
                ranges <- newCompressedList(class(x@ranges), ranges)
              else
                ranges <- newSimpleList(class(x@ranges), ranges)
            } else {
              values <- values(x)
              ranges <- ranges(x)
              if (!missing(i)) {
                prob <- checkIndex(i, length(x), names(x))
                if (!is.null(prob))
                  stop("selecting spaces: ", prob)
                ranges <- ranges[i]
                values <- values[i]
              }
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
    stop("all arguments in '...' must be RangedData objects")
  nms <- lapply(rds, ## figure out names like 'c' on an ordinary vector
                function(rd) structure(logical(length(rd)), names = names(rd)))
  nms <- names(do.call(c, nms))
  names(rds) <- NULL # critical for dispatch to work
  ranges <- do.call(c, lapply(rds, ranges))
  names(ranges) <- nms
  rd@ranges <- ranges
  values <- do.call(c, lapply(rds, values))
  names(values) <- nms
  rd@values <- values
  rd
})

setMethod("split", "RangedData", function(x, f, drop = FALSE) {
  if (length(f) > nrow(x) || nrow(x) %% length(f) > 0)
    stop("nrow(x) is not a multiple of length(f)")
  splitInd <- split(seq_len(nrow(x)), f, drop)
  do.call(RangedDataList, lapply(splitInd, function(ind) x[ind,]))
})

setMethod("rbind", "RangedData", function(..., deparse.level=1) {
  args <- list(...)
  rls <- lapply(args, ranges)
  rl <- rls[[1]]
  if (!all(sapply(sapply(rls, universe), identical, universe(rl))))
    stop("All args in '...' must have the same universe as 'x'")
  ##dfs <- lapply(args, values)
  ##df <- dfs[[1]]
  df <- do.call("rbind", lapply(args, function(x) unlist(values(x))))
  nmsList <- lapply(args, names)
  if (any(sapply(nmsList, is.null))) {
    if (!all(unlist(lapply(args, length)) == length(args[[1]])))
      stop("If any args are missing names, all must have same length")
    nms <- seq_len(length(args[[1]]))
  } else nms <- unique(unlist(nmsList))
  for (nm in nms) {
    rli <- lapply(rls, `[[`, nm)
    rl[[nm]] <- do.call(c, rli[!sapply(rli, is.null)])
    ##dfi <- lapply(dfs, `[[`, nm)
    ##df[[nm]] <- do.call(rbind, dfi[!sapply(dfi, is.null)])
  }
  counts <- unlist(lapply(rls, function(x) lapply(x, length)))
  if (is.numeric(nms))
    f <- rep(rep(nms, length(args)), counts)
  else f <- factor(rep(unlist(nmsList), counts), names(rl))
  df <- split(df, f)
  names(df) <- names(rl)
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

setAs("RangedData", "DataFrame",
      function(from)
      {
        DataFrame(as.data.frame(ranges(from)), values(from))
      })

setAs("Rle", "RangedData",
      function(from)
      {
          RangedData(successiveIRanges(runLength(from)),
                     DataFrame(score = runValue(from)))
      })

setAs("RangesList", "RangedData",
      function(from)
      {
        xdfs <- do.call("SplitDataFrameList", lapply(from, function(x) {
          xdf <- new2("DataFrame", nrows = length(x), check=FALSE)
          rownames(xdf) <- names(x)
          xdf
        }))
        new2("RangedData", ranges = from, values = xdfs, check=FALSE)
      })

setAs("Ranges", "RangedData",
      function(from)
      {
        RangedData(from)
      })

setMethod("as.env", "RangedData", function(x, enclos = parent.frame()) {
  env <- callNextMethod()
  makeActiveBinding("ranges", function() {
    val <- ranges(x)
    rm(list="ranges", envir=env)
    assign("ranges", val, env) ## cache for further use
    val
  }, env)
  env
})

.RangedData_fromDataFrame <- function(from) {
  required <- c("start", "end")
  if (!all(required %in% colnames(from)))
    stop("'from' must at least include a 'start' and 'end' column")
  datacols <- setdiff(colnames(from), c(required, "space", "width"))
  RangedData(IRanges(from$start, from$end), from[,datacols,drop=FALSE],
             space = from$space)
}

setAs("data.frame", "RangedData", .RangedData_fromDataFrame)
setAs("DataTable", "RangedData", .RangedData_fromDataFrame)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Show
###

setMethod("show", "RangedData", function(object) {
  nr <- nrow(object)
  nc <- ncol(object)
  lo <- length(object)
  cat(class(object), ": ",
      nr, ifelse(nr == 1, " range by ", " ranges by "),
      nc, ifelse(nc == 1, " column on ", " columns on "),
      lo, ifelse(lo == 1, " sequence\n", " sequences\n"), sep = "")
  cat(labeledLine("colnames", colnames(object)))
  if (!is.null(names(object)))
    cat(labeledLine("names", names(object)))
})

### =========================================================================
### RangedDataList objects
### -------------------------------------------------------------------------

### Lists of RangedData objects

setClass("RangedDataList",
         prototype = prototype(elementType = "RangedData"),
         contains = "SimpleList")

RangedDataList <- function(...)
{
  listData <- list(...)
  if (length(listData) == 1 && is.list(listData[[1]]))
    listData <- listData[[1]]
  newSimpleList("RangedDataList", listData)
}

setMethod("unlist", "RangedDataList",
          function(x, recursive = TRUE, use.names = TRUE) {
            if (!missing(recursive))
              warning("'recursive' argument currently ignored")
            ans <- do.call(c, as.list(x))
            if (!use.names)
              names(ans) <- NULL
            ans
          })
