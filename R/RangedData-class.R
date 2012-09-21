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

setClass("RangedData", contains = c("DataTable", "List"),
         representation(ranges = "RangesList", values = "SplitDataFrameList"),
         prototype = prototype(ranges = new("SimpleRangesList"),
                               values = new("CompressedSplitDataFrameList")))

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Accessor methods.
###

setMethod("values", "RangedData", function(x) x@values)

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
                   if (is.null(rownames(value)) && !is.null(rownames(x)))
                     rownames(value) <- rownames(x)
                   else if (!identical(rownames(value), rownames(values(x))))
                     stop("rownames of 'value', if non-NULL, must match the ",
                          "rownames of 'x'")
                   x@values <- value
                   x
                 })

setMethod("ranges", "RangedData", function(x) x@ranges)

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
            start(unlist(ranges(x), use.names=FALSE))
          })
setMethod("end", "RangedData",
          function(x) {
            end(unlist(ranges(x), use.names=FALSE))
          })
setMethod("width", "RangedData",
          function(x) {
            width(unlist(ranges(x), use.names=FALSE))
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

setMethod("columnMetadata", "RangedData", function(x) {
  columnMetadata(values(x))
})

setReplaceMethod("columnMetadata", "RangedData", function(x, value) {
  columnMetadata(values(x)) <- value
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

RangedData <- function(ranges = IRanges(), ..., space = NULL, universe = NULL)
{
  hasDots <- (((nargs() - !missing(space)) - !missing(universe)) > 1)

  if (!is.null(universe) && !isSingleString(universe))
    stop("'universe' must be a single string")

  if (is(ranges, "RangesList")) {
    if (!is.null(space))
      warning("since 'class(ranges)' extends RangesList, 'space' argument is ignored")
    if (is.null(names(ranges)))
      names(ranges) <- as.character(seq_len(length(ranges)))
    space <-
      Rle(factor(names(ranges), levels = names(ranges)),
          elementLengths(ranges))
    N <- sum(elementLengths(ranges))
    NAMES <- unlist(lapply(ranges, names), use.names=FALSE)
  } else {
    if (!is(ranges, "Ranges")) {
      coerced <- try(as(ranges, "RangedData"), silent=TRUE)
      if (is(coerced, "RangedData"))
        return(coerced)
      stop("'ranges' must be a Ranges or directly coercible to RangedData")
    }
    N <- length(ranges)
    NAMES <- names(ranges)
    if (is.null(space)) {
      if (N == 0)
        space <- Rle(factor())
      else
        space <- Rle(factor("1"))
    } else if (!is(space, "Rle")) {
      space <- Rle(space)
    }
    if (!is.factor(runValue(space)))
      runValue(space) <- factor(runValue(space))
    if (length(space) != N) {
      if (length(space) == 0L)
        stop("'space' is a 0-length vector but length of 'ranges' is > 0")
      ## We make an exception to the "length(space) must be <= N" rule when
      ## N != 0L so we can support the direct creation of RangedData objects
      ## with 0 rows across 1 or more user-specified spaces like in:
      ##     RangedData(ranges=IRanges(), space=letters)
      if (N != 0L && length(space) > N)
        stop("length of 'space' greater than length of 'ranges'")
      if (N %% length(space) != 0)
        stop("length of 'ranges' not a multiple of 'space' length")
      space <- rep(space, length.out = N)
    }
    if (!is(ranges, "IRanges"))
        ranges <- as(ranges, "IRanges")
    ranges <- split(ranges, space)
  }
  universe(ranges) <- universe

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
  values <- split(values, space)

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
                     values <- unlist(values(x))
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
                if (anyMissingOrOutside(i, upper = lx))
                  return("subscript contains NAs or out of bounds indices")
                if (anyMissingOrOutside(i, 0L, lx) &&
                    anyMissingOrOutside(i, upper = 0L))
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
                if (is(i, "RangesList"))
                  stop("'[' subsetting by RangesList is defunct.\n",
                       "Use 'subsetByOverlaps' instead.")
                if (is(i, "LogicalList")) {
                  xeltlen <- elementLengths(ranges(x))
                  whichRep <- which(xeltlen != elementLengths(i))
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
  names(ranges) <- nms
  rd@ranges <- ranges
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

setAs("RleViewsList", "RangedData", function(from) {
  subject <- subject(from)
  from_ranges <- restrict(ranges(from), 1L, elementLengths(subject),
                          keep.all.ranges = TRUE)
### FIXME: do we want to insert NAs for out of bounds views?
  score <- seqselect(subject, from_ranges)
  score_part <- seqapply(width(from_ranges), PartitioningByWidth)
  score_ranges <- ranges(score)
  ol <- findOverlaps(score_ranges, score_part)
  offset <- (start(from_ranges) - start(score_part))[seqapply(ol, subjectHits)]
  ranges <- shift(ranges(ol, score_ranges, score_part), offset)
  viewNames <- lapply(from_ranges, function(x) {
    if (is.null(names(x)))
      seq_len(length(x))
    else names(x)
  })
  RangedData(ranges,
             score = unlist(runValue(score), use.names = FALSE)[queryHits(ol)],
             view = unlist(viewNames, use.names = FALSE)[subjectHits(ol)])
})

setAs("Ranges", "RangedData",
      function(from)
      {
        RangedData(from)
      })

setAs("RangesList", "RangedData",
    function(from)
    {
        from_names <- names(from)
        if (is.null(from_names) || anyDuplicated(from_names))
            stop("cannot coerce a RangesList object with no names ",
                 "or duplicated names to a RangedData object")
        unlisted_from <- unlist(from, use.names=FALSE)
        unlisted_values <- mcols(unlisted_from)
        mcols(unlisted_from) <- NULL
        ans_ranges <- relist(unlisted_from, skeleton=from)
        metadata(ans_ranges) <- metadata(from)
        if (!is(unlisted_values, "DataFrame")) {
            if (!is.null(unlisted_values))
                warning("could not propagate the inner metadata columns of ",
                        "'from' (accessed with 'mcols(unlist(from))') ",
                        "to the data columns (aka values) of the returned ",
                        "RangedData object")
            unlisted_values <- new2("DataFrame",
                                    nrows=length(unlisted_from),
                                    check=FALSE)
        }
        ans_values <- newCompressedList0("CompressedSplitDataFrameList",
                                         unlisted_values,
                                         PartitioningByEnd(ans_ranges))
        new2("RangedData",
             ranges=ans_ranges,
             values=ans_values,
             #metadata=metadata(from),
             elementMetadata=elementMetadata(from),
             check=FALSE)
    }
)

.fromRangedDataToCompressedIRangesList <- function(from)
{
    ans <- ranges(from)
    ## Propagate 'values(from)'.
    ans_unlisted_values <- unlist(values(from), use.names=FALSE)
    mcols(ans@unlistData) <- ans_unlisted_values
    ans
}

setAs("RangedData", "CompressedIRangesList",
    .fromRangedDataToCompressedIRangesList
)
setAs("RangedData", "IRangesList", .fromRangedDataToCompressedIRangesList)
setAs("RangedData", "RangesList", .fromRangedDataToCompressedIRangesList)

setMethod("as.env", "RangedData", function(x, enclos = parent.frame()) {
  env <- callNextMethod(x, enclos)
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
    nms <- rownames(object)
    if (nr < 20) {
      ranges <- unlist(ranges(object), use.names=FALSE)
      values <- unlist(values(object), use.names=FALSE)
      out <-
        cbind(space = as.character(space(object)), ranges = showAsCell(ranges),
              "|" = rep.int("|", nr))
      if (nc > 0)
        out <-
          cbind(out,
                as.matrix(format(do.call(data.frame,
                                         lapply(values, showAsCell)))))
      if (is.null(nms))
        rownames(out) <- as.character(seq_len(nr))
      else
        rownames(out) <- nms
      classinfo <-
        matrix(c("<factor>", "<IRanges>", "|",
                 unlist(lapply(values, function(x)
                               paste("<", class(x), ">", sep = "")),
                        use.names = FALSE)), nrow = 1,
               dimnames = list("", colnames(out)))
    } else {
      top <- object[1:9, ]
      topRanges <- unlist(ranges(top), use.names=FALSE)
      topValues <- unlist(values(top), use.names=FALSE)
      bottom <- object[(nr-8L):nr, ]
      bottomRanges <- unlist(ranges(bottom), use.names=FALSE)
      bottomValues <- unlist(values(bottom), use.names=FALSE)
      out <-
        rbind(cbind(space = as.character(space(top)),
                    ranges = showAsCell(topRanges),
                    "|" = rep.int("|", 9)),
              rbind(rep.int("...", 3)),
              cbind(space = as.character(space(bottom)),
                    ranges = showAsCell(bottomRanges),
                    "|" = rep.int("|", 9)))
      if (nc > 0)
        out <-
          cbind(out,
                rbind(as.matrix(format(do.call(data.frame,
                                                lapply(topValues,
                                                       showAsCell)))),
                rbind(rep.int("...", nc)),
                rbind(as.matrix(format(do.call(data.frame,
                                                lapply(bottomValues,
                                                       showAsCell)))))))
      if (is.null(nms)) {
        rownames(out) <- c(as.character(1:9), "...", as.character((nr-8L):nr))
      } else {
        rownames(out) <- c(head(nms, 9), "...", tail(nms, 9))
      }
      classinfo <-
        matrix(c("<factor>", "<IRanges>", "|",
                 unlist(lapply(topValues, function(x)
                               paste("<", class(x), ">", sep = "")),
                        use.names = FALSE)), nrow = 1,
               dimnames = list("", colnames(out)))
    }
    out <- rbind(classinfo, out)
    print(out, quote = FALSE, right = TRUE)
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
  newList("RangedDataList", listData)
}

setMethod("unlist", "RangedDataList",
          function(x, recursive = TRUE, use.names = TRUE) {
            if (!missing(recursive))
              warning("'recursive' argument currently ignored")
            ans <- do.call(rbind, unname(as.list(x)))
            if (!use.names)
              rownames(ans) <- NULL
            ans
          })

setMethod("stack", "RangedDataList",
          function(x, index.var = "name") {
            rd <- do.call(rbind, unname(as.list(x)))
            spaces <- unlist(lapply(x, space), use.names=FALSE)
            ids <- names(x)
            if (is.null(ids))
              ids <- seq_len(length(x))
            spaceOrd <- order(factor(spaces, names(rd)))
            rd[[index.var]] <- rep(factor(ids), sapply(x, nrow))[spaceOrd]
            rd
          })
