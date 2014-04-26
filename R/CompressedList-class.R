### =========================================================================
### CompressedList objects
### -------------------------------------------------------------------------

setClass("CompressedList",
         contains="List",
         representation(
                        "VIRTUAL",
                        partitioning="PartitioningByEnd",
                        unlistData="ANY"
                       )
         )

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Accessor methods.
###

setMethod("elementLengths", "CompressedList",
    function(x)
    {
        ans <- elementLengths(x@partitioning)
        names(ans) <- names(x)
        ans
    }
)

setMethod("length", "CompressedList", function(x) length(x@partitioning))

setMethod("names", "CompressedList", function(x) names(x@partitioning))

setReplaceMethod("names", "CompressedList",
                 function(x, value)
                 {
                     names(x@partitioning) <- value
                     x
                 })

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Constructor.
###
### Use
###     IRanges:::newCompressedList0(getClass("MyClass"),
###                                  unlistData, partitioning)
### when calling this from another package.
###

newCompressedList0 <- function(Class, unlistData, partitioning)
{
    ans <- new2(Class, unlistData=unlistData,
                partitioning=partitioning, check=FALSE)
    reconcileMetadatacols(ans)
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
### Subsetting.
###

setMethod("extractROWS", "CompressedList",
    function(x, i)
    {
        i <- normalizeSingleBracketSubscript(i, x, as.NSBS=TRUE)
        ir <- IRanges(end=extractROWS(end(x@partitioning), i),
                      width=extractROWS(width(x@partitioning), i))
        ans_unlistData <- extractROWS(x@unlistData, ir)
        ans_partitioning <- new2("PartitioningByEnd",
                                 end=cumsum(width(ir)),
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
        i <- IRanges(start(x_partitioning)[i], end(x_partitioning)[i])
        extractROWS(unlisted_x, i)
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
                         widths <- elementLengths(x)
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
                           compress_listData(listData)
                         slot(x, "partitioning", check=FALSE) <-
                           new2("PartitioningByEnd", end = cumsum(widths),
                                NAMES = NAMES, check=FALSE)
                         if (i > length(x))
                           x <- rbindRowOfNAsToMetadatacols(x)
                         x
                     }
                 })

setReplaceMethod("$", "CompressedList",
                 function(x, name, value) {
                     x[[name]] <- value
                     x
                 })


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Combining and splitting.
###

.bindROWS <- function(...)
{
    args <- list(...)
    if (length(dim(args[[1L]])) < 2L)
        return(c(...))
    rbind(...)
}

### Not exported. 'x' *must* be an unnamed list of length >= 1 (not checked).
unlist_list_of_CompressedList <- function(x)
{
    ans_unlistData <- do.call(.bindROWS, lapply(x, slot, "unlistData"))
    ans_eltlens <- unlist(lapply(x, elementLengths))
    ans <- relist(ans_unlistData, PartitioningByEnd(cumsum(ans_eltlens)))
    ans_mcols <- do.call(rbind.mcols, x)
    rownames(ans_mcols) <- NULL
    mcols(ans) <- ans_mcols
    ans
}

## NOTE: while the 'c' function does not have an 'x', the generic does.
## c() is a primitive, so 'x' can be missing; dispatch is by position,
## although sometimes this does not work so well, so it's best to keep
## names off the parameters whenever feasible.
setMethod("c", "CompressedList",
          function(x, ..., recursive = FALSE) {
              if (!identical(recursive, FALSE))
                  stop("\"c\" method for CompressedList objects ",
                       "does not support the 'recursive' argument")
              if (missing(x))
                  tls <- unname(list(...))
              else
                  tls <- unname(list(x, ...))
              if (!all(sapply(tls, is, "CompressedList")))
                  stop("all arguments in '...' must be CompressedList objects")
              ecs <- sapply(tls, elementType)
              if (!all(sapply(ecs, extends, ecs[[1L]])))
                  stop("all arguments in '...' must have an element class ",
                       "that extends that of the first argument")
              unlist_list_of_CompressedList(tls)
          })

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

setMethod("aggregate", "CompressedList",
          function(x, by, FUN, start = NULL, end = NULL, width = NULL,
                   frequency = NULL, delta = NULL, ..., simplify = TRUE)
          {
              if (!missing(by) && is(by, "RangesList")) {
                  if (length(x) != length(by))
                      stop("for Ranges 'by', 'length(x) != length(by)'")
                  y <- as.list(x)
                  result <-
                    lapply(structure(seq_len(length(x)), names = names(x)),
                           function(i)
                               aggregate(y[[i]], by = by[[i]], FUN = FUN,
                                         frequency = frequency, delta = delta,
                                         ..., simplify = simplify))
                  ans <- try(SimpleAtomicList(result), silent = TRUE)
                  if (inherits(ans, "try-error"))
                      ans <- newList("SimpleList", result)
              } else {
                  ans <- callNextMethod()
              }
              ans
          })

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
               unlistData = compress_listData(listData),
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
        if (length(x) == 0L)
            return(x)
        elt_lens <- elementLengths(x)
        offset <- cumsum(c(0L, elt_lens[-length(elt_lens)]))
        rev <- logical(length(x))
        i <- replaceROWS(rev, i, TRUE)
        ii <- S4Vectors:::fancy_mseq(elt_lens, offset=offset, rev=rev)
        x@unlistData <- x@unlistData[ii]
        x
    }
)


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
        nms <- rep.int(x_names, elementLengths(grouping))
        ans_ROWNAMES <- make.unlist.result.names(nms, ans_ROWNAMES)
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
        if (!identical(recursive, TRUE))
            stop("\"unlist\" method for CompressedList objects ",
                 "does not support the 'recursive' argument")
        if (!isTRUEorFALSE(use.names))
            stop("'use.names' must be TRUE or FALSE")
        setUnlistDataNames(x@unlistData, x@partitioning, use.names, class(x))
    }
)

coerceToCompressedList <- function(from, element.type = NULL, ...) {
  if (is(from, listClassName("Compressed", element.type)))
    return(from)
  if (is.list(from) || is(from, "List")) {
    if (is.list(from)) {
      v <- compress_listData(from)
    } else {
      v <- unlist(from, use.names = FALSE)
    }
    part <- PartitioningByEnd(from)
  } else {
    v <- from
    part <- PartitioningByEnd(seq_len(length(from)))
  }
  if (!is.null(element.type)) {
    v <- S4Vectors:::coercerToClass(element.type)(v, ...)
  }
  to <- relist(v, part)
  names(to) <- names(from)
  to
}
