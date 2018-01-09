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

### Hackery to avoid R CMD check warning for using an internal...
islistfactor <- function(x) {
    eval(as.call(list(quote(.Internal),
                      substitute(islistfactor(x, FALSE), list(x=x)))))
}

compress_listData <- function(x, elementType = NULL) {
    if (length(x) > 0L) {
        if (islistfactor(x)) {
            x <- unlist(x, recursive=FALSE, use.names=FALSE)
        } else if (length(dim(x[[1L]])) < 2L) {
            x <- do.call(c, unname(x))
        } else {
            x <- do.call(rbind, unname(x))
        }
    } else {
        x <- vector()
    }
    x
}

.reconcileMetadatacols <- function(x) {
  x_mcols <- mcols(x)
  if (is(x_mcols, "DataFrame") &&
      nrow(x_mcols) == 0L && ncol(x_mcols) == 0L)
    {
      x_mcols <- S4Vectors:::make_zero_col_DataFrame(length(x))
      mcols(x) <- x_mcols
    }
  x
}

### Low-level. NOT exported.
newCompressedList0 <- function(Class, unlistData, partitioning)
{
    ans <- new2(Class, unlistData=unlistData,
                partitioning=partitioning, check=FALSE)
    .reconcileMetadatacols(ans)
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
    ans_unlistData <- compress_listData(x, ans_elementType)
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
    .reconcileMetadatacols(ans)
}


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
### Coercion.
###

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

setAs("ANY", "CompressedList", function(from) coerceToCompressedList(from))

coerceToCompressedList <- function(from, element.type = NULL, ...) {
  if (is(from, S4Vectors:::listClassName("Compressed", element.type)))
    return(from)
  if (is.list(from) || (is(from, "List") && !is(from, "DataFrame"))) {
    if (is.list(from)) {
      v <- compress_listData(from, element.type)
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

.bindROWS <- function(...)
{
    args <- list(...)
    if (length(dim(args[[1L]])) >= 2L)
        return(rbind(...))
    concatenateObjects(args[[1L]], args)
}

.concatenate_CompressedList_objects <-
    function(.Object, objects, use.names=TRUE, ignore.mcols=FALSE)
{
    if (!is.list(objects))
        stop("'objects' must be a list")
    if (!isTRUEorFALSE(use.names))
        stop("'use.names' must be TRUE or FALSE")
    if (!isTRUEorFALSE(ignore.mcols))
        stop("'ignore.mcols' must be TRUE or FALSE")

    NULL_idx <- which(S4Vectors:::sapply_isNULL(objects))
    if (length(NULL_idx) != 0L)
        objects <- objects[-NULL_idx]
    if (length(objects) == 0L) {
        if (length(.Object) != 0L)
            .Object <- .Object[integer(0)]
        return(.Object)
    }

    ## TODO: Implement (in C) fast 'elementIs(objects, class)' that does
    ##
    ##     sapply(objects, is, class, USE.NAMES=FALSE)
    ##
    ## and use it here. 'elementIs(objects, "NULL")' should work and be
    ## equivalent to 'sapply_isNULL(objects)'.
    if (!all(vapply(objects, is, logical(1), "CompressedList",
                    USE.NAMES=FALSE)))
        stop(wmsg("the objects to concatenate must be CompressedList ",
                  "objects (or NULLs)"))

    names(objects) <- NULL  # so lapply(objects, ...) below returns an
                            # unnamed list

    ## Concatenate "unlistData" slots.
    unlistData_list <- lapply(objects, slot, "unlistData")
    ans_unlistData <- do.call(.bindROWS, unlistData_list)

    ## Concatenate "partitioning" slots.
    ans_breakpoints <- cumsum(unlist(lapply(objects, elementNROWS)))
    ans_partitioning <- PartitioningByEnd(ans_breakpoints)

    .Object <- newCompressedList0(class(.Object),
                                  ans_unlistData, ans_partitioning)

    ## Call method for Vector objects to concatenate all the parallel
    ## slots (only "elementMetadata" in the case of CompressedList) and
    ## stick them into '.Object'.
    callNextMethod()
}

setMethod("concatenateObjects", "CompressedList",
    .concatenate_CompressedList_objects
)


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
    old_validity_status <- S4Vectors:::disableValidity()
    S4Vectors:::disableValidity(TRUE)
    on.exit(S4Vectors:::disableValidity(old_validity_status))
    ans[non_empty_idx] <-
      lapply(non_empty_idx,
             function(i)
                 FUN(extractROWS(unlisted_X,
                                 IRanges(X_elt_start[i], X_elt_end[i])),
                     ...))
    S4Vectors:::disableValidity(old_validity_status)
    for (i in non_empty_idx) {
        obj <- ans[[i]]
        if (isS4(obj) && !isTRUE(validObject(obj, test=TRUE)))
            stop("invalid output element of class \"", class(obj), "\"")
    }
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
### classNameForDisplay()
###

setMethod("classNameForDisplay", "CompressedList",
    function(x) sub("^Compressed", "", class(x))
)

