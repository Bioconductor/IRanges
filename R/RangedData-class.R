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

setGeneric("values<-", function(x, ..., value) standardGeneric("values<-"))
setReplaceMethod("values", "RangedData",
                 function(x, value) {
                   if (extends(class(value), "SplitDataFrameList")) {
                     if (!identical(elementLengths(values(x)), elementLengths(value)))
                       stop("'value' must have same elementLengths as current 'values'")
                   } else if (extends(class(value), "DataFrame")) {
                     value <- split(value, space(x))
                   } else {
                     stop("'value' must extend class SplitDataFrameList or DataFrame")
                   }
                   x@values <- value
                   x
                 })

setGeneric("ranges", function(x, ...) standardGeneric("ranges"))
setMethod("ranges", "RangedData", function(x) x@ranges)

setGeneric("ranges<-", function(x, ..., value) standardGeneric("ranges<-"))
setReplaceMethod("ranges", "RangedData",
                 function(x, value) {
                   if (extends(class(value), "RangesList")) {
                     if (!identical(lapply(ranges(x), names), lapply(value, names)))
                       stop("'value' must have same length and names as current 'ranges'")
                   } else if (extends(class(value), "IRanges")) {
                     value <- split(value, space(x))
                   } else {
                     stop("'value' must extend class RangesList or IRanges")
                   }
                   x@ranges <- value
                   x
                 })

## range delegates
setMethod("start", "RangedData",
          function(x) {
            unlist(start(ranges(x)), use.names=FALSE)
          })
setMethod("end", "RangedData",
          function(x) {
            unlist(end(ranges(x)), use.names=FALSE)
          })
setMethod("width", "RangedData",
          function(x) {
            unlist(width(ranges(x)), use.names=FALSE)
          })
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
setMethod("elementLengths", "RangedData", function(x) elementLengths(ranges(x)))

setMethod("space", "RangedData", function(x) space(ranges(x)))
setMethod("universe", "RangedData", function(x) universe(ranges(x)))
setReplaceMethod("universe", "RangedData",
                 function(x, value) {
                   universe(x@ranges) <- value
                   x
                 })

setGeneric("score", function(x, ...) standardGeneric("score"))
setMethod("score", "RangedData",
          function(x) {
              score <- x[["score"]]
              ## if (is.null(score) && ncol(x) > 0 && is.numeric(x[[1L]]))
              ##     score <- x[[1L]]
              score
          })

setGeneric("score<-", function(x, ..., value) standardGeneric("score<-"))
setReplaceMethod("score", "RangedData",
                 function(x, value) {
                     if (!is.numeric(value))
                         stop("score must be numeric")
                     if (length(value) != nrow(x))
                         stop("number of scores must equal the number of rows")
                     x[["score"]] <- value
                     x
                 })

setMethod("range", "RangedData", function(x, ..., na.rm) {
  args <- list(x, ...)
  rangeLists <- lapply(args, ranges)
  do.call(range, rangeLists)
})

## values delegates
setMethod("nrow", "RangedData",
          function(x) {
            sum(nrow(values(x)))
          })
setMethod("ncol", "RangedData",
          function(x) {
            ncol(values(x))[[1L]]
          })
setMethod("rownames", "RangedData",
          function(x, do.NULL = TRUE, prefix = "row") {
            rn <-
              unlist(rownames(values(x), do.NULL = do.NULL, prefix = prefix),
                     use.names=FALSE)
            if (length(rn) == 0)
              rn <- NULL
            rn
          })
setMethod("colnames", "RangedData",
          function(x, do.NULL = TRUE, prefix = "col") {
            if (length(x) == 0)
              character()
            else
              colnames(values(x), do.NULL = do.NULL, prefix = prefix)[[1L]]
          })
setReplaceMethod("rownames", "RangedData",
                 function(x, value) {
                   if (!is.null(value)) {
                     if (length(value) != nrow(x)) {
                       stop("invalid 'row.names' length")
                     } else {
                       if (!is.character(value))
                         value <- as.character(value)
                       ends <- cumsum(elementLengths(x))
                       value <-
                         new("CompressedCharacterList",
                             unlistData = value,
                             partitioning = PartitioningByEnd(ends))
                     }
                   }
                   ranges <- ranges(x)
                   for(i in seq_len(length(ranges))) {
                     names(ranges[[i]]) <- value[[i]]
                   }
                   x@ranges <- ranges
                   rownames(x@values) <- value
                   x
                 })
setReplaceMethod("colnames", "RangedData",
                 function(x, value) {
                   colnames(x@values) <- value
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

.valid.RangedData.names <- function(x) {
  nms <- names(x)
  if (length(nms) != length(x))
    "length(names(x)) must equal length(x)"
  else if (!is.character(nms) || anyMissing(nms) || anyDuplicated(nms))
    "names(x) must be a character vector without any NA's or duplicates"
  else NULL
}

.valid.RangedData <- function(x) {
  c(.valid.RangedData.ranges(x), .valid.RangedData.names(x))
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
      names(ranges) <- as.character(seq_len(length(ranges)))
    space <-
      factor(rep.int(names(ranges), elementLengths(ranges)),
             levels = names(ranges))
    N <- sum(elementLengths(ranges))
    NAMES <- unlist(lapply(ranges, names), use.names=FALSE)
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
    if (!is(ranges, "RangesList")) {
      if (length(space) != N) {
        if (length(space) > N)
          stop("length of 'space' greater than length of 'ranges'")
        if (N %% length(space) != 0)
          stop("length of 'ranges' not a multiple of 'space' length")
        space <- recycleVector(space, N)
      }
      ranges <- split(ranges, space)
    }
    values <- split(values, space)
  } else {
    if (!is(ranges, "RangesList"))
      ranges <- RangesList(ranges)
    values <- SplitDataFrameList(values, compress = TRUE)
    if (!length(space))
      space <- "1"
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
            if (is.na(i) || (is.character(i) &&
                !(i %in% c("space", "ranges", colnames(x)))))
              NULL
            else if (i == "space")
              space(x)
            else if (i == "ranges")
              unlist(ranges(x), use.names=FALSE)
            else
              unlist(values(x), use.names=FALSE)[[i]]
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
                   if (is.numeric(i) && (i < 1L || i > ncol(x) + 1L))
                     stop("subscript out of bounds")
                   if (i == "space")
                     stop("cannot replace \"space\" information")
                   if (i == "ranges") {
                     ranges(x) <- value
                   } else {
                     nrx <- nrow(x)
                     lv <- length(value)
                     if (!is.null(value) && (nrx != lv)) {
                       if ((nrx == 0) || (nrx %% lv != 0))
                         stop(paste(lv, "elements in value to replace",
                                    nrx, "elements"))
                       else
                         value <- rep(value, length.out = nrx)
                     }
                     nrows <- elementLengths(values(x))
                     inds <- seq_len(length(x))
                     spaces <- factor(rep.int(inds, nrows), inds)
                     values <- unlist(values(x), use.names=FALSE)
                     values[[i]] <- value
                     x@values <- split(values, spaces)
                     names(x@values) <- names(x)
                   }
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
              if (is.numeric(i)) {
                if (!is.integer(i))
                  i <- as.integer(i)
                if (anyMissingOrOutside(i, -lx, lx))
                  return("subscript contains NAs or out of bounds indices")
                if (anyMissingOrOutside(i, 0L, lx) &&
                    anyMissingOrOutside(i, -lx, 0L))
                  return("negative and positive indices cannot be mixed")
              } else if (is.logical(i)) {
                if (anyMissing(i))
                  return("subscript contains NAs")
                if (length(i) > lx)
                  return("subscript out of bounds")
              } else if ((is.character(i) || is.factor(i))) {
                if (anyMissing(i))
                  return("subscript contains NAs")
                if (anyMissing(match(i, nms)))
                  return("mismatching names")
              } else if (!is.null(i)) {
                return("invalid subscript type")
              }
              NULL
            }
            mstyle <- nargs() > 2
            if (mstyle) {
              ranges <- ranges(x)
              values <- values(x)
              if (!missing(j)) {
                prob <- checkIndex(j, ncol(x), colnames(x))
                if (!is.null(prob))
                  stop("selecting cols: ", prob)
                values <- values[, j, drop=FALSE]
              }
              if (!missing(i)) {
                if (is(i, "RangesList")) {
                  i <- !is.na(unlist(findOverlaps(ranges, i, multiple=FALSE)))
### FIXME: could do this if Ranges supported NAs, then ordering is possible
                  ##i <- findOverlaps(i, ranges, multiple=FALSE, drop=TRUE)
                } else if (is(i, "LogicalList")) {
                  xeltlen <- elementLengths(ranges(x))
                  whichRep <- whichAsVector(xeltlen != elementLengths(i))
                  for (k in whichRep)
                    i[[k]] <- rep(i[[k]], length.out = xeltlen[k])
                  i <- unlist(i, use.names=FALSE)
                } else if (is(i, "IntegerList")) {
                  itemp <-
                    LogicalList(lapply(elementLengths(ranges(x)), rep,
                                       x = FALSE))
                  for (k in seq_len(length(itemp)))
                    itemp[[k]][i[[k]]] <- TRUE
                  i <- unlist(itemp, use.names=FALSE)
                }
                prob <- checkIndex(i, nrow(x), rownames(x))
                if (!is.null(prob))
                  stop("selecting rows: ", prob)
                if (is.numeric(i) && any(i < 0))
                  i <- setdiff(seq(nrow(x)), -i)
                if (is.logical(i)) {
                  igroup <-
                    factor(rep.int(seq_len(length(x)), elementLengths(x)),
                           levels = seq_len(length(x)))
                  if (length(i) < nrow(x))
                    i <- rep(i, length.out = nrow(x))
                } else {
                  if (is.null(i))
                    i <- integer(0)
                  if (is.factor(i))
                    i <- as.character(i)
                  if (is.character(i)) {
                    dummy <- seq_len(nrow(x))
                    names(dummy) <- rownames(x)
                    i <- dummy[i]
                    if (anyMissing(i)) ## cannot subset by NAs yet
                      stop("invalid rownames specified")
                  }
                  starts <- cumsum(c(1L, head(elementLengths(x), -1)))
                  igroup <-
                    factor(findInterval(i, starts), levels = seq_len(length(x)))
                  if (anyDuplicated(runValue(Rle(igroup))))
                    stop("cannot mix row indices from different spaces")
                  i <- i - (starts - 1L)[as.integer(igroup)]
                }
                isplit <- split(i, igroup)
                names(isplit) <- names(x)
                ranges <- seqselect(ranges, isplit)
                values <- seqselect(values, isplit)
                if (drop) {
                  ok <- (elementLengths(ranges) > 0)
                  ranges <- ranges[ok]
                  values <- values[ok]
                }
              }
            } else {
              if (!missing(i)) {
                prob <- checkIndex(i, length(x), names(x))
                if (!is.null(prob))
                  stop("selecting spaces: ", prob)
                ranges <- ranges(x)[i]
                values <- values(x)[i]
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

setMethod("seqselect", "RangedData",
          function(x, start=NULL, end=NULL, width=NULL)
          initialize(x,
                     ranges =
                     seqselect(ranges(x), start=start, end=end, width=width),
                     values =
                     seqselect(values(x), start=start, end=end, width=width))
          )

setReplaceMethod("seqselect", "RangedData",
                 function(x, start = NULL, end = NULL, width = NULL, value)
                 stop("operation not supported")
                 )

setReplaceMethod("window", "RangedData",
                 function(x, start = NA, end = NA, width = NA, keepLength = TRUE, ..., value)
                 stop("operation not supported")
                 )

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Combining and splitting.
###

setMethod("c", "RangedData", function(x, ..., recursive = FALSE) {
  if (recursive)
    stop("'recursive' mode not supported")
  if (missing(x))
    rds <- unname(list(...))
  else
    rds <- unname(list(x, ...))
  rd <- rds[[1L]]
  if (!all(sapply(rds, is, "RangedData")))
    stop("all arguments in '...' must be RangedData objects")
  nms <- lapply(rds, ## figure out names like 'c' on an ordinary vector
                function(rd) structure(logical(length(rd)), names = names(rd)))
  nms <- names(do.call(c, nms))
  names(rds) <- NULL # critical for dispatch to work
  ranges <- do.call(c, lapply(rds, ranges))
  values <- do.call(c, lapply(rds, values))
  rm(rds); gc()
  names(ranges) <- nms
  rd@ranges <- ranges
  rm(ranges)
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
  args <- unname(list(...))
  rls <- lapply(args, ranges)
  if (!all(sapply(sapply(rls, universe), identical, universe(rls[[1L]]))))
    stop("All args in '...' must have the same universe")
  nms <- unique(unlist(lapply(args, names), use.names=FALSE))
  rls <- lapply(rls, function(x) {y <- as.list(x)[nms];names(y) <- nms;y})
  dfs <-
    lapply(args, function(x) {y <- as.list(values(x))[nms];names(y) <- nms;y})
  safe.c <- function(...) {
    x <- list(...)
    do.call(c, x[!sapply(x, is.null)])
  }
  rls <- IRangesList(do.call(Map, c(list(safe.c), rls)))
  safe.rbind <- function(...) {
    x <- list(...)
    do.call(rbind, x[!sapply(x, is.null)])
  }
  dfs <- SplitDataFrameList(do.call(Map, c(list(safe.rbind), dfs)))
  for (i in seq_len(length(rls)))
    names(rls[[i]]) <- rownames(dfs[[i]])
  initialize(args[[1L]], ranges = rls, values = dfs)
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
                       as.data.frame(values(x))[-1L],
                       row.names = row.names,
                       stringsAsFactors = FALSE)
          })

setAs("RangedData", "DataFrame",
      function(from)
      {
        DataFrame(as.data.frame(ranges(from)), values(from))
      })

setAs("Rle", "RangedData",
      function(from)
      {
        new2("RangedData",
             ranges = IRangesList("1" = successiveIRanges(runLength(from))),
             values =
             SplitDataFrameList("1" = DataFrame(score = runValue(from))),
             metadata = metadata(from),
             check = FALSE)
      })

setAs("RleList", "RangedData",
      function(from)
      {
        ranges <-
          IRangesList(lapply(from, function(x)
                             successiveIRanges(runLength(x))))
        values <-
          SplitDataFrameList(lapply(from, function(x)
                                    DataFrame(score = runValue(x))))
        if (is.null(names(from))) {
          nms <- as.character(seq_len(length(from)))
          names(ranges) <- nms
          names(values) <- nms
        }
        new2("RangedData",
             ranges = ranges, values = values,
             metadata = metadata(from),
             elementMetadata = elementMetadata(from),
             check = FALSE)
      })

setAs("Ranges", "RangedData",
      function(from)
      {
        RangedData(from)
      })

setAs("RangesList", "RangedData",
      function(from)
      {
        dfs <- do.call("SplitDataFrameList", lapply(from, function(x) {
          df <- new2("DataFrame", nrows = length(x), check=FALSE)
          rownames(df) <- names(x)
          df
        }))
        new2("RangedData", ranges = from, values = dfs,
             metadata = metadata(from),
             elementMetadata = elementMetadata(from),
             check = FALSE)
      })

setMethod("as.env", "RangedData", function(x, enclos = parent.frame()) {
  env <- callNextMethod()
  makeAccessorBinding <- function(fun, name = deparse(substitute(fun))) {
    makeActiveBinding(name, function() {
      val <- fun(x)
      rm(list=name, envir=env)
      assign(name, val, env) ## cache for further use
      val
    }, env)
  }
  makeAccessorBinding(ranges)
  makeAccessorBinding(space)
  makeAccessorBinding(start)
  makeAccessorBinding(width)
  makeAccessorBinding(end)
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
  cat(class(object), " with ",
      nr, ifelse(nr == 1, " row and ", " rows and "),
      nc, ifelse(nc == 1, " value column across ", " value columns across "),
      lo, ifelse(lo == 1, " space\n", " spaces\n"), sep = "")
  if (nr > 0) {
    k <- ifelse(nr <= 12L, nr, min(nr, 10L))
    subset  <- object[seq_len(k),]
    rangesSubset <- unlist(ranges(subset), use.names=FALSE)
    valuesSubset <- unlist(values(subset), use.names=FALSE)
    out <-
      cbind(space = space(subset),
            ranges = showAsCell(rangesSubset),
            "|" = rep.int("|", k))
    if (nc > 0)
      out <-
        cbind(out,
              as.matrix(format.data.frame(do.call(data.frame,
                        lapply(valuesSubset, showAsCell)))))
    if (is.null(rownames(subset)))
      rownames(out) <- seq_len(k)
    else
      rownames(out) <- rownames(subset)
    classinfo <-
      matrix(c("<character>", "<IRanges>", "|",
               unlist(lapply(valuesSubset, function(x)
                             paste("<", class(x), ">", sep = "")),
                      use.names = FALSE)), nrow = 1,
             dimnames = list("", colnames(out)))
    out <- rbind(classinfo, out)
    print(out, quote = FALSE, right = TRUE)
    diffK <- nr - k
    if (diffK > 0)
      cat("...\n<", diffK, " more rows>\n", sep="")
  }
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
  if (length(listData) == 1 && is.list(listData[[1L]]))
    listData <- listData[[1L]]
  newSimpleList("RangedDataList", listData)
}

setMethod("unlist", "RangedDataList",
          function(x, recursive = TRUE, use.names = TRUE) {
            if (!missing(recursive))
              warning("'recursive' argument currently ignored")
            for (i in seq_len(length(x))) {
              nms <- paste(names(x)[i], names(ranges(x[[i]])), sep = ".")
              names(x[[i]]@ranges) <- nms
              names(x[[i]]@values) <- nms
            }
            ans <- do.call(c, unname(as.list(x)))
            if (!use.names)
              names(ans) <- NULL
            ans
          })
