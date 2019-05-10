### =========================================================================
### RangedData objects
### -------------------------------------------------------------------------

## For keeping data with your ranges

## There are two design aims:
## 1) Efficiency when data is large (i.e. apply by chromosome)
## 2) Convenience when data is not so large (i.e. unrolling the data)

## The ranges are stored in a IntegerRangesList, while the data is stored
## in a SplitDataFrameList. The IntegerRangesList is uncompressed, because
## users will likely want to apply over each IntegerRanges separately,
## as they are usually in separate spaces. Also, it is difficult to
## compress RangesLists, as lists containing Views or NCLists
## are uncompressible. The SplitDataFrameList should be compressed,
## because it's cheap to create from a split factor and, more
## importantly, cheap to get and set columns along the entire dataset,
## which is common. Usually the data columns are atomic vectors and
## thus trivially compressed. It does, however, incur a slight
## performance penalty when applying over the RangedData.

setClass("RangedData", contains = c("DataTable", "List"),
         representation(ranges = "IntegerRangesList",
                        values = "SplitDataFrameList"),
         prototype = prototype(ranges = new("SimpleIRangesList"),
                               values = new("CompressedSplitDataFrameList")))

wmsg2 <- function(...)
    paste0("  ",
           paste0(strwrap(paste0(c(...), collapse="")), collapse="\n  "))

RangedData_is_deprecated_msg <-
  c("RangedData objects are deprecated. ",
    "Please migrate your code to use GRanges or GRangesList objects instead. ",
    "See IMPORTANT NOTE in ?RangedData")

RangedData_method_is_defunct_msg <- function(what)
  c("RangedData objects are deprecated and the ", what, " is now defunct. ",
    "Please migrate your code to use GRanges or GRangesList objects instead. ",
    "See IMPORTANT NOTE in ?RangedData")

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Accessor methods.
###

setMethod("values", "RangedData",
    function(x) {
        .Deprecated(msg=wmsg2(RangedData_is_deprecated_msg))
        x@values
    })

setReplaceMethod("values", "RangedData",
                 function(x, value) {
                   .Deprecated(msg=wmsg2(RangedData_is_deprecated_msg))
                   if (extends(class(value), "SplitDataFrameList")) {
                     if (!identical(elementNROWS(values(x)),
                                    elementNROWS(value)))
                       stop("'value' must have same elementNROWS ",
                            "as current 'values'")
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

setMethod("ranges", "RangedData",
    function(x, use.names=TRUE, use.mcols=FALSE) x@ranges
)

setReplaceMethod("ranges", "RangedData",
                 function(x, value) {
                   .Deprecated(msg=wmsg2(RangedData_is_deprecated_msg))
                   if (extends(class(value), "IntegerRangesList")) {
                     if (!identical(lapply(ranges(x), names), lapply(value, names)))
                       stop("'value' must have same length and names as current 'ranges'")
                   } else if (extends(class(value), "IRanges")) {
                     value <- split(value, space(x))
                   } else {
                     stop("'value' must extend class IntegerRangesList or IRanges")
                   }
                   x@ranges <- value
                   x
                 })

## range delegates
setMethod("start", "RangedData",
          function(x) {
            .Deprecated(msg=wmsg2(RangedData_is_deprecated_msg))
            start(unlist(ranges(x), use.names=FALSE))
          })
setMethod("end", "RangedData",
          function(x) {
            .Deprecated(msg=wmsg2(RangedData_is_deprecated_msg))
            end(unlist(ranges(x), use.names=FALSE))
          })
setMethod("width", "RangedData",
          function(x) {
            .Deprecated(msg=wmsg2(RangedData_is_deprecated_msg))
            width(unlist(ranges(x), use.names=FALSE))
          })
setReplaceMethod("start", "RangedData",
                 function(x, ..., value) {
                   .Deprecated(msg=wmsg2(RangedData_is_deprecated_msg))
                   start(ranges(x), ...) <- value
                   x
                 })
setReplaceMethod("end", "RangedData",
                 function(x, ..., value) {
                   .Deprecated(msg=wmsg2(RangedData_is_deprecated_msg))
                   end(ranges(x), ...) <- value
                   x
                 })
setReplaceMethod("width", "RangedData",
                 function(x, ..., value) {
                   .Deprecated(msg=wmsg2(RangedData_is_deprecated_msg))
                   width(ranges(x), ...) <- value
                   x
                 })
setMethod("length", "RangedData",
    function(x) {
        .Deprecated(msg=wmsg2(RangedData_is_deprecated_msg))
        length(ranges(x))
    })
setMethod("names", "RangedData",
    function(x) {
        .Deprecated(msg=wmsg2(RangedData_is_deprecated_msg))
        names(ranges(x))
    })
setReplaceMethod("names", "RangedData",
                 function(x, value) {
                   .Deprecated(msg=wmsg2(RangedData_is_deprecated_msg))
                   if (!is.null(value) && !is.character(value))
                     stop("'value' must be NULL or a character vector")
                   names(x@ranges) <- value
                   names(x@values) <- value
                   x
                 })
setMethod("elementNROWS", "RangedData",
    function(x) {
        .Deprecated(msg=wmsg2(RangedData_is_deprecated_msg))
        elementNROWS(ranges(x))
    })

setMethod("space", "RangedData",
    function(x) {
        .Deprecated(msg=wmsg2(RangedData_is_deprecated_msg))
        space(ranges(x))
    })

setGeneric("universe", function(x) standardGeneric("universe"))
setMethod("universe", "RangedData",
    function(x) {
        .Deprecated(msg=wmsg2(RangedData_is_deprecated_msg))
        universe(ranges(x))
    })

setGeneric("universe<-", function(x, value) standardGeneric("universe<-"))
setReplaceMethod("universe", "RangedData",
                 function(x, value) {
                   .Deprecated(msg=wmsg2(RangedData_is_deprecated_msg))
                   universe(x@ranges) <- value
                   x
                 })

## values delegates
setMethod("nrow", "RangedData",
          function(x) {
            .Deprecated(msg=wmsg2(RangedData_is_deprecated_msg))
            sum(nrow(values(x)))
          })
setMethod("ncol", "RangedData",
          function(x) {
            .Deprecated(msg=wmsg2(RangedData_is_deprecated_msg))
            ncol(values(x))[[1L]]
          })
setMethod("rownames", "RangedData",
          function(x, do.NULL = TRUE, prefix = "row") {
            .Deprecated(msg=wmsg2(RangedData_is_deprecated_msg))
            rn <-
              unlist(rownames(values(x), do.NULL = do.NULL, prefix = prefix),
                     use.names=FALSE)
            if (length(rn) == 0)
              rn <- NULL
            rn
          })
setMethod("colnames", "RangedData",
          function(x, do.NULL = TRUE, prefix = "col") {
            .Deprecated(msg=wmsg2(RangedData_is_deprecated_msg))
            if (length(x) == 0)
              character()
            else
              colnames(values(x), do.NULL = do.NULL, prefix = prefix)[[1L]]
          })
setReplaceMethod("rownames", "RangedData",
                 function(x, value) {
                   .Deprecated(msg=wmsg2(RangedData_is_deprecated_msg))
                   if (!is.null(value)) {
                     if (length(value) != nrow(x)) {
                       stop("invalid 'row.names' length")
                     } else {
                       if (!is.character(value))
                         value <- as.character(value)
                       ends <- cumsum(elementNROWS(x))
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
                   .Deprecated(msg=wmsg2(RangedData_is_deprecated_msg))
                   colnames(x@values) <- value
                   x
                 })

setMethod("columnMetadata", "RangedData", function(x) {
  .Deprecated(msg=wmsg2(RangedData_is_deprecated_msg))
  columnMetadata(values(x))
})

setReplaceMethod("columnMetadata", "RangedData", function(x, value) {
  .Deprecated(msg=wmsg2(RangedData_is_deprecated_msg))
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
  else if (!is.character(nms) || S4Vectors:::anyMissing(nms) || anyDuplicated(nms))
    "names(x) must be a character vector without any NA's or duplicates"
  else NULL
}

.valid.RangedData <- function(x) {
  .Deprecated(msg=wmsg2(RangedData_is_deprecated_msg))
  c(.valid.RangedData.ranges(x), .valid.RangedData.names(x))
}

setValidity2("RangedData", .valid.RangedData)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Constructor.
###

## creates a single-element RangedData (unless splitter (space) is specified)

RangedData <- function(ranges = IRanges(), ..., space = NULL)
{
  .Deprecated(msg=wmsg2(RangedData_is_deprecated_msg))
  hasDots <- (((nargs() - !missing(space))) > 1)
  if (is(ranges, "IntegerRangesList") && !is(ranges, "IntegerRanges")) {
    if (!is.null(space))
      warning("since 'class(ranges)' extends IntegerRangesList, 'space' argument is ignored")
    if (is.null(names(ranges)))
      names(ranges) <- as.character(seq_len(length(ranges)))
    space <-
      Rle(factor(names(ranges), levels = names(ranges)),
          elementNROWS(ranges))
    N <- sum(elementNROWS(ranges))
    NAMES <- unlist(lapply(ranges, names), use.names=FALSE)
  } else {
    if (!is(ranges, "IntegerRanges")) {
      coerced <- try(as(ranges, "RangedData"), silent=TRUE)
      if (is(coerced, "RangedData"))
        return(coerced)
      stop("'ranges' must be an IntegerRanges or directly coercible to RangedData")
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

  if (hasDots) {
    args <- list(...)
    if (length(args) == 1L && is(args[[1L]], "SplitDataFrameList")) {
      values <- unlist(args[[1L]], use.names=FALSE)
    } else {
      values <- DataFrame(...)
    }
  }
  else
    values <- S4Vectors:::make_zero_col_DataFrame(N)
  if (N != nrow(values)) {
    if (nrow(values) > N)
      stop("length of value(s) in '...' greater than length of 'ranges'")
    if (nrow(values) == 0 || N %% nrow(values) != 0)
      stop("length of 'ranges' not a multiple of length of value(s) in '...'")
    rind <- S4Vectors:::recycleVector(seq_len(nrow(values)), N)
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
            .Deprecated(msg=wmsg2(RangedData_is_deprecated_msg))
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
                   .Deprecated(msg=wmsg2(RangedData_is_deprecated_msg))
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
                     nrows <- elementNROWS(values(x))
                     inds <- seq_len(length(x))
                     spaces <- factor(rep.int(inds, nrows), inds)
                     values <- unlist(values(x), use.names=FALSE)
                     values[[i]] <- value
                     x@values <- split(values, spaces)
                     names(x@values) <- names(x)
                   }
                   x
                 })

### Supported index types: numeric, logical, character, NULL and missing.
## Two index modes:
## - list style ([i]):  subsets by range space (e.g. chromosome)
## - matrix style ([i,j]): subsets the data frame
setMethod("[", "RangedData",
          function(x, i, j, ..., drop=FALSE)
          {
            .Deprecated(msg=wmsg2(RangedData_is_deprecated_msg))
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
                if (S4Vectors:::anyMissingOrOutside(i, upper = lx))
                  return("subscript contains NAs or out of bounds indices")
                if (S4Vectors:::anyMissingOrOutside(i, 0L, lx) &&
                    S4Vectors:::anyMissingOrOutside(i, upper = 0L))
                  return("negative and positive indices cannot be mixed")
              } else if (is.logical(i)) {
                if (S4Vectors:::anyMissing(i))
                  return("subscript contains NAs")
                if (length(i) > lx)
                  return("subscript out of bounds")
              } else if ((is.character(i) || is.factor(i))) {
                if (S4Vectors:::anyMissing(i))
                  return("subscript contains NAs")
                if (S4Vectors:::anyMissing(match(i, nms)))
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
                if (is(i, "IntegerRangesList"))
                  stop("subsetting a RangedData object ",
                       "by an IntegerRangesList subscript is not supported")
                if (is(i, "LogicalList")) {
                  x_eltNROWS <- elementNROWS(ranges(x))
                  whichRep <- which(x_eltNROWS != elementNROWS(i))
                  for (k in whichRep)
                    i[[k]] <- rep(i[[k]], length.out = x_eltNROWS[k])
                  i <- unlist(i, use.names=FALSE)
                } else if (is(i, "IntegerList")) {
                  itemp <-
                    LogicalList(lapply(elementNROWS(ranges(x)), rep,
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
                    factor(rep.int(seq_len(length(x)), elementNROWS(x)),
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
                    if (S4Vectors:::anyMissing(i)) ## cannot subset by NAs yet
                      stop("invalid rownames specified")
                  }
                  starts <- cumsum(c(1L, head(elementNROWS(x), -1)))
                  igroup <-
                    factor(findInterval(i, starts), levels = seq_len(length(x)))
                  if (anyDuplicated(runValue(Rle(igroup))))
                    stop("cannot mix row indices from different spaces")
                  i <- i - (starts - 1L)[as.integer(igroup)]
                }
                isplit <- split(i, igroup)
                names(isplit) <- names(x)
                ranges <- S4Vectors:::subset_List_by_List(ranges, isplit)
                values <- S4Vectors:::subset_List_by_List(values, isplit)
                if (drop) {
                  ok <- (elementNROWS(ranges) > 0)
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

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Combining and splitting.
###

setMethod("c", "RangedData", function(x, ..., recursive = FALSE) {
  .Deprecated(msg=wmsg2(RangedData_is_deprecated_msg))
  if (!identical(recursive, FALSE))
    stop("\"c\" method for RangedData objects ",
         "does not support the 'recursive' argument")
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

setMethod("rbind", "RangedData", function(..., deparse.level=1) {
  .Deprecated(msg=wmsg2(RangedData_is_deprecated_msg))
  args <- unname(list(...))
  rls <- lapply(args, ranges)
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

### The 2 functions, as.data.frame.IntegerRangesList() and
### as.data.frame.DataFrameList() are needed for as.data.frame.RangedData().
###
### A new as.data.frame,List method was implemented in BioC 2.15 and
### is now used by all List classes. Because the RangedData class is being 
### phased out, we want to retain the old behavior. In order to do that 
### we have to keep these 2 helpers because as.data.frame.RangedData() 
### uses old methods from both IntegerRangesList and DataFrameList.
###
### These helpers are not exported.
.as.data.frame.IntegerRangesList <- function(x, row.names=NULL, optional=FALSE,
                                             ...)
{
    if (!(is.null(row.names) || is.character(row.names)))
        stop("'row.names'  must be NULL or a character vector")
    x <- as(x, "CompressedIRangesList")
    spaceLevels <- seq_len(length(x))
    if (length(names(x)) > 0) {
        spaceLabels <- names(x)
    } else {
        spaceLabels <- as.character(spaceLevels)
    }
    data.frame(space =
               factor(rep.int(seq_len(length(x)), elementNROWS(x)),
                      levels = spaceLevels,
                      labels = spaceLabels),
               as.data.frame(unlist(x, use.names = FALSE)),
               row.names = row.names,
               stringsAsFactors = FALSE)
}

.as.data.frame.DataFrameList <- function(x, row.names=NULL, 
                                         optional=FALSE, ...)
{
    if (!(is.null(row.names) || is.character(row.names)))
        stop("'row.names' must be NULL or a character vector")
    if (!missing(optional) || length(list(...)))
        warning("'optional' and arguments in '...' ignored")
    stacked <- stack(x)
    if (is.null(row.names))
        row.names <- rownames(stacked)
    as.data.frame(stacked, row.names = row.names, optional = optional)
}

.as.data.frame.RangedData <- function(x, row.names=NULL, optional=FALSE, ...)
{
    .Deprecated(msg=wmsg2(RangedData_is_deprecated_msg))
    if (!(is.null(row.names) || is.character(row.names)))
        stop("'row.names'  must be NULL or a character vector")
    if (!missing(optional) || length(list(...)))
        warning("'optional' and arguments in '...' ignored")
    data.frame(.as.data.frame.IntegerRangesList(ranges(x)),
               .as.data.frame.DataFrameList(values(x))[-1L],
               row.names = row.names,
               stringsAsFactors = FALSE)
}
setMethod("as.data.frame", "RangedData", .as.data.frame.RangedData)

setAs("RangedData", "DataFrame",
      function(from)
      {
        DataFrame(as.data.frame(ranges(from)),
                  unlist(values(from), use.names=FALSE))
      })

.fromRangedDataToCompressedIRangesList <- function(from)
{
    .Deprecated(msg=wmsg2(RangedData_is_deprecated_msg))
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

setMethod("as.env", "RangedData", function(x, enclos = parent.frame(2)) {
  .Deprecated(msg=wmsg2(RangedData_is_deprecated_msg))
  env <- S4Vectors:::makeEnvForNames(x, colnames(x), enclos)
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


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Show
###

.show_RangedData <- function(object) {
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
                 unlist(lapply(values, function(x) {
                     paste("<", classNameForDisplay(x), ">", sep = "")
                 }), use.names = FALSE)), nrow = 1,
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
                 unlist(lapply(topValues, function(x) {
                     paste("<", classNameForDisplay(x), ">", sep = "")
                 }), use.names = FALSE)), nrow = 1,
               dimnames = list("", colnames(out)))
    }
    out <- rbind(classinfo, out)
    print(out, quote = FALSE, right = TRUE)
  }
}

setMethod("show", "RangedData",
    function(object) {
        .Deprecated(msg=wmsg2(RangedData_is_deprecated_msg))
         suppressWarnings(.show_RangedData(object))
    })

