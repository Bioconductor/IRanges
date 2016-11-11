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

setUnlistDataNames <- function(unlisted_x, grouping, use.names, x_class)
{
    ## If 'use.names' is FALSE or 'x' has no *outer* names, then we don't
    ## do anything to 'ans' i.e. we just keep whatever names/rownames are
    ## on it (which are the *inner* names/rownames of 'x'). Note that this
    ## behavior is NOT consistent with unlist,List or base::unlist as
    ## both of them will return a vector with no names/rownames when
    ## 'use.names' is FALSE.
    ## FIXME: Make unlist,CompressedList and unlist,List behave
    ## consistently in *any* situation.
    ## Otherwise (i.e. if 'use.names' is TRUE and 'x' has *outer* names),
    ## we make up new names/rownames for 'ans' by prepending the *outer*
    ## names of 'x' to its *inner* names/rownames. Note that this differs
    ## from what base::unlist does but THIS IS A FEATURE and is consistent
    ## with what unlist,List does.
    if (use.names && !is.null(x_names <- names(grouping))) {
        if (length(dim(unlisted_x)) < 2L) {
            ans_ROWNAMES <- names(unlisted_x)
        } else {
            ans_ROWNAMES <- rownames(unlisted_x)
        }
        nms <- rep.int(x_names, elementNROWS(grouping))
        ans_ROWNAMES <- S4Vectors:::make_unlist_result_names(nms, ans_ROWNAMES)
        if (length(dim(unlisted_x)) < 2L) {
            res <- try(names(unlisted_x) <- ans_ROWNAMES, silent=TRUE)
            what <- "names"
        } else {
            res <- try(rownames(unlisted_x) <- ans_ROWNAMES, silent=TRUE)
            what <- "rownames"
        }
        if (is(res, "try-error"))
            warning("failed to set ", what, " on the ",
                    "unlisted ", x_class, " object")
    }
    unlisted_x
}

setMethod("unlist", "CompressedList",
    function(x, recursive=TRUE, use.names=TRUE)
    {
        if (!isTRUEorFALSE(use.names))
            stop("'use.names' must be TRUE or FALSE")
        setUnlistDataNames(x@unlistData, x@partitioning, use.names, class(x))
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
        i <- normalizeDoubleBracketSubscript(i, x, exact=exact,
                                             error.if.nomatch=FALSE)
        if (is.na(i))
            return(NULL)
        unlisted_x <- unlist(x, use.names=FALSE)
        x_partitioning <- PartitioningByEnd(x)
        window_start <- start(x_partitioning)[i]
        window_end <- end(x_partitioning)[i]
        S4Vectors:::Vector_window(unlisted_x,
                                  start=window_start,
                                  end=window_end)
    }
)

setReplaceMethod("[[", "CompressedList",
                 function(x, i, j,..., value)
                 {
                     nameValue <- if (is.character(i)) i else ""
                     i <- S4Vectors:::normargSubset2_iOnly(x, i, j, ...,
                              .conditionPrefix="[[<-,CompressedList-method: ")
                     if (is.null(value)) {
                         if (i <= length(x)) # if name did not exist, could be +1
                             x <- x[-i]
                     } else {
                         value <- try(as(value, elementType(x)), silent = TRUE)
                         if (inherits(value, "try-error"))
                             stop("cannot coerce 'value' to a ", elementType(x),
                                  " instance")
                         listData <- as.list(x, use.names = FALSE)
                         listData[[i]] <- value
                         widths <- elementNROWS(x)
                         names(widths) <- NULL
                         widths[i] <- NROW(value)
                         if ((i == length(x) + 1L) &&
                             (!is.null(names(x)) || nchar(nameValue) > 0)) {
                             NAMES <- names(x)
                             if (is.null(NAMES))
                                 NAMES <- rep.int("", length(x))
                             NAMES[i] <- nameValue
                         } else {
                             NAMES <- names(x)
                         }
                         slot(x, "unlistData", check=FALSE) <-
                           compress_listData(listData, elementType(x))
                         slot(x, "partitioning", check=FALSE) <-
                           new2("PartitioningByEnd", end = cumsum(widths),
                                NAMES = NAMES, check=FALSE)
                         if (i > length(x))
                           x <- S4Vectors:::rbindRowOfNAsToMetadatacols(x)
                         x
                     }
                 })

setReplaceMethod("$", "CompressedList",
                 function(x, name, value) {
                     x[[name]] <- value
                     x
                 })


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Combining.
###

.bindROWS <- function(...)
{
    args <- list(...)
    if (length(dim(args[[1L]])) >= 2L)
        return(rbind(...))
    if (!is.factor(args[[1L]]))
        return(c(...))
    ans_levels <- unique(unlist(lapply(args, levels)))
    x <- unlist(lapply(args, as.character))
    factor(x, levels=ans_levels)
}

### Not exported.
combine_CompressedList_objects <- function(Class, objects,
                                           use.names=TRUE, ignore.mcols=FALSE)
{
    if (!isSingleString(Class))
        stop("'Class' must be a single character string")
    if (!extends(Class, "CompressedList"))
        stop("'Class' must be the name of a class that extends CompressedList")
    if (!is.list(objects))
        stop("'objects' must be a list")
    if (!isTRUEorFALSE(use.names))
        stop("'use.names' must be TRUE or FALSE")
    ### TODO: Support 'use.names=TRUE'.
    if (use.names)
        stop("'use.names=TRUE' is not supported yet")
    if (!isTRUEorFALSE(ignore.mcols))
        stop("'ignore.mcols' must be TRUE or FALSE")

    if (length(objects) != 0L) {
        ## TODO: Implement (in C) fast 'elementIsNull(objects)' in S4Vectors
        ## that does 'sapply(objects, is.null, USE.NAMES=FALSE)', and use it
        ## here.
        null_idx <- which(sapply(objects, is.null, USE.NAMES=FALSE))
        if (length(null_idx) != 0L)
            objects <- objects[-null_idx]
    }
    if (length(objects) == 0L)
        return(new(Class))

    ## TODO: Implement (in C) fast 'elementIs(objects, class)' in S4Vectors
    ## that does 'sapply(objects, is, class, USE.NAMES=FALSE)', and use it
    ## here. 'elementIs(objects, "NULL")' should work and be equivalent to
    ## 'elementIsNull(objects)'.
    if (!all(sapply(objects, is, "CompressedList", USE.NAMES=FALSE)))
        stop("the objects to combine must be CompressedList objects (or NULLs)")
    objects_names <- names(objects)
    names(objects) <- NULL  # so lapply(objects, ...) below returns an
                            # unnamed list

    ## Combine "unlistData" slots.
    unlistData_slots <- lapply(objects, function(x) x@unlistData)
    ans_unlistData <- do.call(.bindROWS, unlistData_slots)

    ## Combine "partitioning" slots.
    ans_breakpoints <- cumsum(unlist(lapply(objects, elementNROWS)))
    ans_partitioning <- PartitioningByEnd(ans_breakpoints)

    ans <- newCompressedList0(Class, ans_unlistData, ans_partitioning)

    ## Combine "mcols" slots.
    if (!ignore.mcols) {
        ans_mcols <- do.call(S4Vectors:::rbind_mcols, objects)
        rownames(ans_mcols) <- NULL
        mcols(ans) <- ans_mcols
    }
    ans
}

setMethod("c", "CompressedList",
    function (x, ..., ignore.mcols=FALSE, recursive=FALSE)
    {
        if (!identical(recursive, FALSE))
            stop("\"c\" method for CompressedList objects ",
                 "does not support the 'recursive' argument")
        if (missing(x)) {
            objects <- list(...)
            x <- objects[[1L]]
        } else {
            objects <- list(x, ...)
        }
        combine_CompressedList_objects(class(x), objects,
                                       use.names=FALSE,
                                       ignore.mcols=ignore.mcols)
    }
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

.updateCompressedList <- function(X, listData) {
    elementTypeX <- elementType(X)
    if (!all(sapply(listData,
                    function(Xi) extends(class(Xi), elementTypeX))))
        stop("'FUN' must return elements of class ", elementTypeX)
    if (length(listData) == 0) {
        end <- integer(0)
    } else {
        end <- cumsum(unlist(lapply(listData, NROW), use.names = FALSE))
    }
    initialize(X,
               unlistData = compress_listData(listData, elementTypeX),
               partitioning = 
               new2("PartitioningByEnd", end = end, NAMES = names(X),
                    check=FALSE))
}

setMethod("endoapply", "CompressedList",
          function(X, FUN, ...) {
              .updateCompressedList(X, lapply_CompressedList(X, FUN, ...))
          })

setMethod("mendoapply", "CompressedList",
          function(FUN, ..., MoreArgs = NULL) {
              .updateCompressedList(list(...)[[1L]],
                                    mapply(FUN = match.fun(FUN), ...,
                                           MoreArgs = MoreArgs,
                                           SIMPLIFY = FALSE))
          })

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

