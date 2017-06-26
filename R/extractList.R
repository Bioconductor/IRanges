### =========================================================================
### Group elements of a vector-like object into a list-like object
### -------------------------------------------------------------------------
###
### What should go in this file?
###
### - All "relist" methods defined in IRanges should go here.
### - extractList() generic and default method.
###
### TODO: Maybe put the default methods for the reverse transformations here
### (unlist, unsplit, and unsplit<-).
###


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### relist()
###

setMethod("relist", c("ANY", "PartitioningByEnd"),
    function(flesh, skeleton)
    {
        ans_class <- relistToClass(flesh)
        skeleton_len <- length(skeleton)
        if (skeleton_len == 0L) {
            flesh_len2 <- 0L
        } else {
            flesh_len2 <- end(skeleton)[skeleton_len]
        }
        if (NROW(flesh) != flesh_len2)
            stop("shape of 'skeleton' is not compatible with 'NROW(flesh)'")
        if (extends(ans_class, "CompressedList"))
            return(newCompressedList0(ans_class, flesh, skeleton))
        if (!extends(ans_class, "SimpleList"))
            stop("don't know how to split or relist a ", class(flesh),
                 " object as a ", ans_class, " object")
        listData <- lapply(skeleton, function(i) extractROWS(flesh, i))

        ## TODO: Once "window" methods have been revisited/tested and
        ## 'window(flesh, start=start, end=end)' is guaranteed to do the
        ## right thing for any 'flesh' object (in particular it subsets a
        ## data.frame-like object along the rows), then replace the line above
        ## by the code below (which should be more efficient):

        #skeleton_start <- start(skeleton)
        #skeleton_end <- end(skeleton)
        #FUN <- function(start, end) window(flesh, start=start, end=end)
        #names(skeleton_start) <- names(skeleton)
        #listData <- mapply(FUN, skeleton_start, skeleton_end)

        ## or, if we don't trust mapply():

        #skeleton_start <- start(skeleton)
        #skeleton_end <- end(skeleton)
        #X <- seq_len(skeleton_len)
        #names(X) <- names(skeleton)
        #listData <- lapply(X, function(i) window(flesh,
        #                                         start=skeleton_start[i],
        #                                         end=skeleton_end[i]))

        S4Vectors:::new_SimpleList_from_list(ans_class, listData)
    }
)

setMethod("relist", c("ANY", "List"),
    function(flesh, skeleton)
    {
        relist(flesh, PartitioningByEnd(skeleton))
    }
)

setMethod("relist", c("Vector", "list"),
    function(flesh, skeleton)
    {
        relist(flesh, PartitioningByEnd(skeleton))
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### splitAsList()
###

### 'f' is assumed to be an integer vector with no NAs.
.splitAsList_by_integer <- function(x, f, drop)
{
    if (length(f) > NROW(x))
        stop("'f' cannot be longer than 'NROW(x)' when it's an integer vector")
    if (!identical(drop, FALSE))
        warning("'drop' is ignored when 'f' is an integer vector")
    f_is_not_sorted <- S4Vectors:::isNotSorted(f)
    if (f_is_not_sorted) {
        idx <- base::order(f)
        f <- f[idx]
        x <- extractROWS(x, idx)
    }
    tmp <- Rle(f)
    f <- cumsum(runLength(tmp))
    names(f) <- as.character(runValue(tmp))
    f <- PartitioningByEnd(f)
    relist(x, f)
}

### 'f' is assumed to be a factor with no NAs.
.splitAsList_by_factor <- function(x, f, drop)
{
    x_NROW <- NROW(x)
    f_len <- length(f)
    f_levels <- levels(f)
    f <- as.integer(f)
    if (f_len > x_NROW)
        f <- head(f, n=x_NROW)
    f_is_not_sorted <- S4Vectors:::isNotSorted(f)
    if (f_is_not_sorted) {
        idx <- base::order(f)
        x <- extractROWS(x, idx)
    }
    f <- tabulate(f, nbins=length(f_levels))
    names(f) <- f_levels
    if (drop)
        f <- f[f != 0L]
    f <- cumsum(f)
    f <- PartitioningByEnd(f)
    relist(x, f)
}

### 'f' is assumed to be an integer-Rle object with no NAs.
.splitAsList_by_integer_Rle <- function(x, f, drop)
{
    if (length(f) > NROW(x))
        stop("'f' cannot be longer than data when it's an integer-Rle")
    if (!identical(drop, FALSE))
        warning("'drop' is ignored when 'f' is an integer-Rle")
    f_vals <- runValue(f)
    f_lens <- runLength(f)
    f_is_not_sorted <- S4Vectors:::isNotSorted(f_vals)
    if (f_is_not_sorted) {
        idx <- base::order(f_vals)
        xranges <- successiveIRanges(f_lens)[idx]
        f_vals <- f_vals[idx]
        f_lens <- f_lens[idx]
        x <- extractROWS(x, xranges)
    }
    tmp <- Rle(f_vals, f_lens)
    f <- cumsum(runLength(tmp))
    names(f) <- as.character(runValue(tmp))
    f <- PartitioningByEnd(f)
    relist(x, f)
}

### 'f' is assumed to be an Rle object with no NAs.
.splitAsList_by_Rle <- function(x, f, drop)
{
    x_NROW <- NROW(x)
    f_len <- length(f)
    f_vals <- runValue(f)
    if (!is.factor(f_vals)) {
        f_vals <- as.factor(f_vals)
        if (f_len > x_NROW) {
            runValue(f) <- f_vals
            f <- head(f, n=x_NROW)
            f_vals <- runValue(f)
        }
    } else if (f_len > x_NROW) {
        f <- head(f, n=x_NROW)
        f_vals <- runValue(f)
    }
    f_lens <- runLength(f)
    f_levels <- levels(f_vals)
    f_vals <- as.integer(f_vals)
    f_is_not_sorted <- S4Vectors:::isNotSorted(f_vals)
    if (f_is_not_sorted) {
        idx <- base::order(f_vals)
        xranges <- successiveIRanges(f_lens)[idx]
        x <- extractROWS(x, xranges)
        f <- S4Vectors:::tabulate2(f_vals, nbins=length(f_levels),
                                   weight=f_lens)
        if (drop) {
            f_levels <- f_levels[f != 0L]
            f <- f[f != 0L]
        }
    } else if (length(f_vals) == length(f_levels) || drop) {
        if (drop) f_levels <- as.character(runValue(f))
        f <- f_lens
    } else {
        f <- integer(length(f_levels))
        f[f_vals] <- f_lens
    }
    names(f) <- f_levels
    f <- cumsum(f)
    f <- PartitioningByEnd(f)
    relist(x, f)
}

setGeneric("splitAsList", signature=c("x", "f"),
    function(x, f, drop=FALSE, ...) standardGeneric("splitAsList")
)

toFactor <- function(x) {
    if (is(x, "Rle")) {
        runValue(x) <- as.factor(runValue(x))
        x
    } else as.factor(x)
}

### Took this out of the still-in-incubation LazyList package
interaction2 <- function(factors) {
  nI <- length(factors)
  nx <- length(factors[[1L]])
  factors <- lapply(factors, toFactor)
  useRle <- any(vapply(factors, is, logical(1), "Rle"))
  if (useRle) {
    group <- as(factors[[1L]], "Rle")
    runValue(group) <- as.integer(runValue(group))
  } else {
    group <- as.integer(factors[[1L]])
  }
  ngroup <- nlevels(factors[[1L]])
  for (i in tail(seq_len(nI), -1L)) {
    index <- factors[[i]]
    if (useRle) {
      offset <- as(index, "Rle")
      runValue(offset) <- ngroup * (as.integer(runValue(offset)) - 1L)
    } else {
      offset <- ngroup * (as.integer(index) - 1L)
    }
    group <- group + offset
    ngroup <- ngroup * nlevels(index)
  }
  if (useRle) {
      runValue(group) <- structure(runValue(group),
                                   levels=as.character(seq_len(ngroup)),
                                   class="factor")
      group
  } else {
      structure(group, levels=as.character(seq_len(ngroup)), class="factor")
  }
}

normSplitFactor <- function(f, x) {
  if (is(f, "formula")) {
    if (length(f) == 3L)
      stop("formula 'f' should not have a left hand side")
    f <- S4Vectors:::formulaValues(x, f)
  }
  if (is.list(f) || is(f, "List")) {
      if (length(f) == 1L) {
          f <- toFactor(f[[1L]])
      } else {
          f <- interaction2(f)
      }
  }
  f_len <- length(f)
  if (f_len < NROW(x)) {
    if (f_len == 0L)
      stop("split factor has length 0 but 'NROW(x)' is > 0")
    if (NROW(x) %% f_len != 0L)
      warning("'NROW(x)' is not a multiple of split factor length")
    f <- rep(f, length.out=NROW(x))
  }
  f
}

## about 3X faster than as.factor on a ~450k tx ids
## caveats: no NAs, and radix sort of levels does not support all encodings
## todo: Would be faster if sort() returned grouping info,
##       but then we might coalesce this with the order/split.
## todo: if we could pass na.rm=TRUE to grouping(), NAs would be handled
as.factor2 <- function(x) {
    if (is.factor(x))
        return(x)
    if (is.null(x))
        return(factor())
    g <- grouping(x)
    p <- PartitioningByEnd(relist(g))
    levs <- as.character(x[g[end(p)]])
    if (is.character(x)) {
        o <- order(levs, method="radix")
        map <- integer(length(levs)) # or rep(NA_integer_, length(levs)) for NAs
        map[o] <- seq_along(o)
        ref <- map[togroup(p)]
        levs <- levs[o]
    } else {
        ref <- togroup(p)
    }
    f <- integer(length(x))
    f[g] <- ref
    structure(f, levels=levs, class="factor")
}

splitAsList_default <- function(x, f, drop=FALSE)
{
    if (!isTRUEorFALSE(drop))
        stop("'drop' must be TRUE or FALSE")

    f <- normSplitFactor(f, x)
    if (anyNA(f)) {
        keep_idx <- which(!is.na(f))
        x <- extractROWS(x, keep_idx)
        f <- f[keep_idx]
    }
    
    if (is.integer(f))
        return(.splitAsList_by_integer(x, f, drop))
    if (!is(f, "Rle")) {
        f <- as.factor2(f)
        return(.splitAsList_by_factor(x, f, drop))
    }
    ## From now on, 'f' is guaranteed to be an Rle.
    f_vals <- runValue(f)
    if (!((is.vector(f_vals) && is.atomic(f_vals)) || is.factor(f_vals)))
        stop("'f' must be an atomic vector or a factor (possibly in Rle form)")
    if (is.integer(f_vals))
        return(.splitAsList_by_integer_Rle(x, f, drop))
    return(.splitAsList_by_Rle(x, f, drop))
}

setMethod("splitAsList", c("ANY", "ANY"),
    function(x, f, drop=FALSE) splitAsList_default(x, f, drop=drop)
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### extractList()
###
### Would extractGroups be a better name for this?
### Or extractGroupedROWS? (analog to extractROWS, except that the ROWS are
### grouped).
###

### 'x' must be a vector-like object and 'i' a list-like object.
### Must return a list-like object parallel to 'i' and with same "shape" as
### 'i' (i.e. same elementNROWS). If 'i' has names, they should be
### propagated to the returned value. The list elements of the returned value
### must have the class of 'x'.
setGeneric("extractList", function(x, i) standardGeneric("extractList"))

### Default method.
setMethod("extractList", c("ANY", "ANY"),
    function(x, i)
    {
        if (is(i, "Ranges"))
            return(relist(extractROWS(x, i), i))
        if (is.list(i)) {
            unlisted_i <- unlist(i, recursive=FALSE, use.names=FALSE)
        } else if (is(i, "List")) {
            ## The various "unlist" methods for List derivatives don't know
            ## how to operate recursively and don't support the 'recursive'
            ## arg.
            unlisted_i <- unlist(i, use.names=FALSE)
        } else {
            stop("'i' must be a list-like object")
        }
        relist(extractROWS(x, unlisted_i), i)
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### regroupBySupergroup()
###
### A very efficient way to concatenate groups of successive list elements
### in 'x'.
### 'x' must be a list-like object (typically a CompressedList object).
### 'supergroups' must be an object that defines a partitioning of
### 'seq_along(x)' (i.e. it could be used to do
### 'relist(seq_along(x), supergroups)'). It will be immediately replaced with
### 'PartitioningByEnd(supergroups)' so it should be an object that is
### accepted by the PartitioningByEnd() constructor (note that this constructor
### is a no-op if 'supergroups' is already a PartitioningByEnd object).
### Return a list-like object of the same elementType() as 'x' and parallel
### to 'supergroups'. The names on 'supergroups' are propagated but not the
### metadata columns.
###
### Some properties:
### - Behaves as an endomorphism on a CompressedList or PartitioningByEnd
###   object.
### - This
###       regroupBySupergroup(x, length(x))[[1L]]
###   is equivalent to
###       unlist(x, use.names=FALSE)
###
### Other possible names for regroupBySupergroup: regroup,
### mergeGroupsInSupergroups, combineGroupsOfListElements,
### unlistGroupsOfListElements, unlistBySupergroup.
###
### TODO: Maybe export and document this?

regroupBySupergroup <- function(x, supergroups)
{
    supergroups <- PartitioningByEnd(supergroups)
    x_breakpoints <- end(PartitioningByEnd(x))
    ans_breakpoints <- x_breakpoints[end(supergroups)]
    nleading0s <- length(supergroups) - length(ans_breakpoints)
    if (nleading0s != 0L)
        ans_breakpoints <- c(rep.int(0L, nleading0s), ans_breakpoints)
    ans_partitioning <- PartitioningByEnd(ans_breakpoints,
                                          names=names(supergroups))
    if (is(x, "PartitioningByEnd"))
        return(ans_partitioning)
    relist(unlist(x, use.names=FALSE), ans_partitioning)
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### resplit() and regroup()
###
### Similar to regroupBySupergroup() but there is no assumption that
### the new grouping is a super-grouping of the current grouping. For
### resplit(), the grouping is expressed as a factor, although it is
### effectively a synonym of regroup(), since the latter coerces the
### input to a Grouping.
###

resplit <- function(x, f) {
    regroup(x, f)
}

regroup <- function(x, g) {
    g <- as(g, "Grouping")
    gends <- end(PartitioningByEnd(g))
    xg <- x[unlist(g, use.names=FALSE)]
    p <- PartitioningByEnd(end(PartitioningByEnd(xg))[gends])
    names(p) <- names(g)
    relist(unlist(xg, use.names=FALSE, recursive=FALSE), p)
}
