### =========================================================================
### CompressedList objects
### -------------------------------------------------------------------------


setClass("CompressedList",
    contains="List",
    representation(
        "VIRTUAL",
        unlistData="ANY",
        partitioning="PartitioningByEnd"
    )
)

setMethod("classNameForDisplay", "CompressedList",
    function(x) sub("^Compressed", "", class(x))
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Validity.
###

.valid.CompressedList.partitioning <- function(x)
{
    dataLength <- NROW(x@unlistData)
    if (nobj(x@partitioning) != dataLength)
        "improper partitioning"
    else NULL
}
.valid.CompressedList.unlistData <- function(x)
{
    ## FIXME: workaround to support CompressedNormalIRangesList
    ## elementTypeX <- elementType(x)
    elementTypeX <- elementType(new(class(x)))
    if (!extends(class(x@unlistData), elementTypeX))
        paste("the 'unlistData' slot must be of class", elementTypeX)
    else NULL
}
.valid.CompressedList <- function(x)
{
    c(.valid.CompressedList.unlistData(x),
      .valid.CompressedList.partitioning(x))
}
setValidity2("CompressedList", .valid.CompressedList)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### updateObject()
###

### This just implements the generic updateObject strategy that consists
### in calling updateObject() on each **proper** CompressedList slot i.e.
### on the slots added by the CompressedList class, or, said otherwise, on
### the slots that are not inherited.
setMethod("updateObject", "CompressedList",
    function(object, ..., verbose=FALSE)
    {
        ## The 'unlistData' slot could be an Rle, DataFrame, IRanges or
        ## GRanges object, or any vector-like object that needs an update.
        object@unlistData <- updateObject(object@unlistData,
                                          ..., verbose=verbose)

        ## The 'partitioning' slot is a PartitioningByEnd object which derives
        ## from IPosRanges so its elementType slot might need to be updated.
        ## See "updateObject" method for IPosRanges objects for more
        ## information.
        object@partitioning <- updateObject(object@partitioning,
                                            ..., verbose=verbose)

        callNextMethod()
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Getters and setters
###

setMethod("length", "CompressedList", function(x) length(x@partitioning))

setMethod("names", "CompressedList", function(x) names(x@partitioning))

setMethod("elementNROWS", "CompressedList",
    function(x)
    {
        ans <- elementNROWS(x@partitioning)
        names(ans) <- names(x)
        ans
    }
)

setReplaceMethod("names", "CompressedList",
    function(x, value)
    {
        names(x@partitioning) <- value
        x
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Constructor
###
### Use
###     IRanges:::newCompressedList0(getClass("MyClass"),
###                                  unlistData, partitioning)
### when calling this from another package.
###

.reconcile_mcols <- function(x) {
  x_mcols <- mcols(x, use.names=FALSE)
  if (is(x_mcols, "DataFrame") &&
      nrow(x_mcols) == 0L && ncol(x_mcols) == 0L)
    {
      x_mcols <- make_zero_col_DFrame(length(x))
      mcols(x) <- x_mcols
    }
  x
}

### Low-level. NOT exported.
newCompressedList0 <- function(Class, unlistData, partitioning)
{
    ## Note that 'unlistData_target_class' could also be obtained
    ## with 'getClassDef(Class)@slots[["unlistData"]]', in which
    ## case the class name would be returned with the "package" attribute.
    unlistData_target_class <- getSlots(Class)[["unlistData"]]

    ## 'unlistData' must derive from the class expected by the "unlistData"
    ## slot. If it doesn't (e.g. if 'Class' is "CompressedSplitDFrameList"
    ## and 'unlistData' is an ordinary data.frame), then we coerce it. Note
    ## that this coercion could fail.
    if (!is(unlistData, unlistData_target_class))
        unlistData <- as(unlistData, unlistData_target_class)
    ans <- new2(Class, unlistData=unlistData,
                       partitioning=partitioning, check=FALSE)
    .reconcile_mcols(ans)
}

### Low-level. NOT exported.
### Stuff to put in elementMetadata slot can be passed either with
###   new_CompressedList_from_list(..., elementMetadata=somestuff)
### or with
###   new_CompressedList_from_list(..., mcols=somestuff)
### The latter is the new recommended form.
new_CompressedList_from_list <- function(Class, x, ..., mcols)
{
    if (!extends(Class, "CompressedList"))
        stop("class ", Class, " must extend CompressedList")
    if (!is.list(x))
        stop("'x' must be a list")
    ans_elementType <- elementType(new(Class))
    if (!all(sapply(x, function(xi) extends(class(xi), ans_elementType))))
        stop("all elements in 'listData' must be ", ans_elementType, " objects")
    ans_partitioning <- PartitioningByEnd(x)
    if (length(x) == 0L) {
        if (missing(mcols))
            return(new2(Class, partitioning=ans_partitioning, ..., check=FALSE))
        return(new2(Class, partitioning=ans_partitioning, ...,
                           elementMetadata=mcols, check=FALSE))
    }
    ans_unlistData <- S4Vectors:::compress_listData(x, ans_elementType)
    if (missing(mcols)) {
        ans <- new2(Class, unlistData=ans_unlistData,
                           partitioning=ans_partitioning, ...,
                           check=FALSE)
    } else {
        ans <- new2(Class, unlistData=ans_unlistData,
                           partitioning=ans_partitioning, ...,
                           elementMetadata=mcols,
                           check=FALSE)
    }
    .reconcile_mcols(ans)
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### unlist()
###

### Overwrite method for List objects with super fast method for CompressedList
### objects.
setMethod("unlist", "CompressedList",
    function(x, recursive=TRUE, use.names=TRUE)
    {
        if (!isTRUEorFALSE(use.names))
            stop("'use.names' must be TRUE or FALSE")
        unlisted_x <- x@unlistData
        if (use.names)
            unlisted_x <- S4Vectors:::set_unlisted_names(unlisted_x, x)
        unlisted_x
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion.
###

coerceToCompressedList <- function(from, element.type = NULL, ...) {
  if (is(from, S4Vectors:::listClassName("Compressed", element.type)))
    return(from)
  if (is.list(from) || (is(from, "List") && !is(from, "DataFrame"))) {
    if (is.list(from)) {
      v <- S4Vectors:::compress_listData(from, element.type)
    } else {
      v <- unlist(from, use.names = FALSE)
    }
    part <- PartitioningByEnd(from)
  } else {
    v <- from
    part <- PartitioningByEnd(seq_len(NROW(from)))
  }
  if (!is.null(element.type)) {
    v <- S4Vectors:::coercerToClass(element.type)(v, ...)
  }
  to <- relist(v, part)
  names(to) <- names(from)
  to
}

setAs("ANY", "CompressedList", function(from) coerceToCompressedList(from))

setListCoercions <- function(type) {
  CompressedClass <- S4Vectors:::listClassName("Compressed", type)
  SimpleClass <- S4Vectors:::listClassName("Simple", type)
  Class <- S4Vectors:::listClassName("", type)
  hasCompressedList <- CompressedClass != "CompressedList"
  if (hasCompressedList) {
    setAs("ANY", CompressedClass, CoercerToList(type, compress = TRUE))
  }
  setAs("ANY", SimpleClass, CoercerToList(type, compress = FALSE))
  setAs("ANY", Class, CoercerToList(type, compress = hasCompressedList))
  setAs("SimpleList", Class, CoercerToList(type, compress = FALSE))
  setAs("list", Class, CoercerToList(type, compress = FALSE))
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Subsetting.
###

setMethod("extractROWS", "CompressedList",
    function(x, i)
    {
        i <- normalizeSingleBracketSubscript(i, x, as.NSBS=TRUE)
        ans_eltNROWS <- extractROWS(width(x@partitioning), i)
        ans_breakpoints <- suppressWarnings(cumsum(ans_eltNROWS))
        nbreakpoints <- length(ans_breakpoints)
        if (nbreakpoints != 0L && is.na(ans_breakpoints[[nbreakpoints]]))
            stop(wmsg("Subsetting operation on ", class(x), " object 'x' ",
                      "produces a result that is too big to be ",
                      "represented as a CompressedList object. ",
                      "Please try to coerce 'x' to a SimpleList object ",
                      "first (with 'as(x, \"SimpleList\")')."))
        idx_on_unlisted_x <- IRanges(end=extractROWS(end(x@partitioning), i),
                                     width=ans_eltNROWS)
        ans_unlistData <- extractROWS(x@unlistData, idx_on_unlisted_x)
        ans_partitioning <- new2("PartitioningByEnd",
                                 end=ans_breakpoints,
                                 NAMES=extractROWS(names(x), i),
                                 check=FALSE)
        ans_elementMetadata <- extractROWS(x@elementMetadata, i)
        initialize(x, unlistData=ans_unlistData,
                      partitioning=ans_partitioning,
                      elementMetadata=ans_elementMetadata)
    }
)

setMethod("getListElement", "CompressedList",
    function(x, i, exact=TRUE)
    {
        i2 <- normalizeDoubleBracketSubscript(i, x, exact=exact,
                                              allow.NA=TRUE,
                                              allow.nomatch=TRUE)
        if (is.na(i2))
            return(NULL)
        unlisted_x <- unlist(x, use.names=FALSE)
        x_partitioning <- PartitioningByEnd(x)
        window_start <- start(x_partitioning)[i2]
        window_end <- end(x_partitioning)[i2]
        S4Vectors:::Vector_window(unlisted_x,
                                  start=window_start,
                                  end=window_end)
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Concatenation
###

.concatenate_CompressedList_objects <-
    function(x, objects=list(), use.names=TRUE, ignore.mcols=FALSE, check=TRUE)
{
    objects <- S4Vectors:::prepare_objects_to_bind(x, objects)
    all_objects <- c(list(x), objects)

    ## 1. Take care of the parallel slots

    ## Call method for Vector objects to concatenate all the parallel slots
    ## (only "elementMetadata" in the case of CompressedList) and stick them
    ## into 'ans'. Note that the resulting 'ans' can be an invalid object
    ## because its "elementMetadata" slot can be longer (i.e. have more rows)
    ## than 'ans' itself so we use 'check=FALSE' to skip validation.
    ans <- callNextMethod(x, objects, use.names=use.names,
                                      ignore.mcols=ignore.mcols,
                                      check=FALSE)

    ## 2. Take care of the non-parallel slots

    ## Concatenate the "unlistData" slots.
    unlistData_list <- lapply(all_objects, slot, "unlistData")
    ## Skip validation here too (we'll validate the final object).
    ans_unlistData <- bindROWS(unlistData_list[[1L]],
                               objects=unlistData_list[-1L],
                               check=FALSE)

    ## Concatenate the "partitioning" slots.
    ans_breakpoints <- cumsum(unlist(lapply(all_objects, elementNROWS),
                                     use.names=use.names))
    ans_partitioning <- PartitioningByEnd(ans_breakpoints)

    ## Update 'ans' and validate it (if the caller has set 'check' to TRUE).
    BiocGenerics:::replaceSlots(ans, unlistData=ans_unlistData,
                                     partitioning=ans_partitioning,
                                     check=check)
}

setMethod("bindROWS", "CompressedList", .concatenate_CompressedList_objects)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Looping.
###

### Cannot really avoid the cost of extracting X[[i]] for all valid i but tries
### to minimize this cost by using 2 tricks:
###   1. Avoids looping on values of i for which X[[i]] has length 0. Instead
###      FUN(X[[i]], ...) is computed only once (because it's the same for all
###      these values of i) and placed at the corresponding positions in the
###      returned list.
###   2. Turn off object validation during the main loop. Note that there is no
###      reason to restrict this trick to CompressedList objects and the same
###      trick could be used in the "lapply" method for List objects.
### Does NOT propagate the names.
lapply_CompressedList <- function(X, FUN, ...)
{
    FUN <- match.fun(FUN)
    ans <- vector(mode="list", length=length(X))
    unlisted_X <- unlist(X, use.names=FALSE)
    X_partitioning <- PartitioningByEnd(X)
    X_elt_width <- width(X_partitioning)
    empty_idx <- which(X_elt_width == 0L)
    if (length(empty_idx) != 0L) 
        ans[empty_idx] <- list(FUN(extractROWS(unlisted_X, integer(0)), ...))
    non_empty_idx <- which(X_elt_width != 0L)
    if (length(non_empty_idx) == 0L)
        return(ans)
    X_elt_start <- start(X_partitioning)
    X_elt_end <- end(X_partitioning)
    ans[non_empty_idx] <-
      lapply(non_empty_idx,
             function(i)
                 FUN(extractROWS(unlisted_X,
                                 IRanges(X_elt_start[i], X_elt_end[i])),
                     ...))
    ans
}

setMethod("lapply", "CompressedList",
    function(X, FUN, ...)
    {
        ans <- lapply_CompressedList(X, FUN, ...)
        names(ans) <- names(X)
        ans
    }
)

setMethod("revElements", "CompressedList",
    function(x, i)
    {
        i <- normalizeSingleBracketSubscript(i, x, as.NSBS=TRUE)
        if (length(i) == 0L)
            return(x)
        x_eltNROWS <- elementNROWS(x)
        offset <- cumsum(c(0L, x_eltNROWS[-length(x_eltNROWS)]))
        rev <- logical(length(x))
        rev <- replaceROWS(rev, i, TRUE)
        ii <- S4Vectors:::fancy_mseq(x_eltNROWS, offset=offset, rev=rev)
        x@unlistData <- extractROWS(x@unlistData, ii)
        x
    }
)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Recycling
###

repLengthOneElements <- function(x, times) {
    x@unlistData <- rep(x@unlistData, times)
    x@partitioning@end <- cumsum(times)
    x
}

recycleListElements <- function(x, newlen) {
    x_eltNROWS <- elementNROWS(x)
    if (identical(x_eltNROWS, newlen)) {
        return(x)
    }
    if (all(x_eltNROWS == 1L)) {
        ans <- repLengthOneElements(x, newlen)
    } else {
        if (any(x_eltNROWS == 0L & newlen > 0L)) {
            if (is(x, "AtomicList")) {
                x[x_eltNROWS == 0L & newlen > 0L] <- list(NA)
            } else {
                stop("recycling of zero-length elements not supported")
            }
            x_eltNROWS <- elementNROWS(x)
        }
        times <- ceiling(newlen / x_eltNROWS)
        times[x_eltNROWS == 0L] <- 0L
        ans_ir <- rep(as(PartitioningByEnd(x), "IRanges"), times)
        remainder <- newlen %/% x_eltNROWS
        if (any(remainder > 0L)) {
            last <- cumsum(times)
            width(ans_ir)[last[remainder > 0]] <- remainder[remainder > 0]
            warning("Some element lengths are not multiples of their ",
                    "corresponding element length in ", deparse(substitute(x)))
        }
        ans <- relist(extractROWS(unlist(x, use.names=FALSE), ans_ir),
                      PartitioningByWidth(newlen))
    }
    ans
}
