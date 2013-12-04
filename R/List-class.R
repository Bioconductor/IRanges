### =========================================================================
### List objects
### -------------------------------------------------------------------------
###
### List objects are Vector objects with "[[", "elementType" and
### "elementLengths" methods.
###

setClass("List",
    contains="Vector",
    representation(
        "VIRTUAL",
        elementType="character"
    ),
    prototype(elementType="ANY")
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Accessor methods.
###

setGeneric("elementType", function(x, ...) standardGeneric("elementType"))
setMethod("elementType", "List", function(x) x@elementType)
setMethod("elementType", "vector", function(x) mode(x))

setGeneric("elementLengths", function(x) standardGeneric("elementLengths"))

setMethod("elementLengths", "ANY",
    function(x)
    {
        x <- as.list(x)
        ans <-
          try(.Call2("sapply_NROW", x, PACKAGE="IRanges"), silent=TRUE)
        if (!inherits(ans, "try-error")) {
            names(ans) <- names(x)
            return(ans)
        }
        ## From here, 'length(x)' is guaranteed to be != 0
        return(sapply(x, NROW))
    }
)

setMethod("elementLengths", "List",
    function(x)
    {
        y <- as.list(x)
        if (length(y) == 0L) {
            ans <- integer(0)
            ## We must return a named integer(0) if 'x' is named
            names(ans) <- names(x)
            return(ans)
        }
        if (length(dim(y[[1L]])) < 2L)
            return(elementLengths(y))
        return(sapply(y, NROW))
    }
)

setGeneric("isEmpty", function(x) standardGeneric("isEmpty"))
setMethod("isEmpty", "ANY",
          function(x)
          {
              if (is.atomic(x))
                  return(length(x) == 0L)
              if (!is.list(x) && !is(x, "List"))
                  stop("isEmpty() is not defined for objects of class ",
                       class(x))
              ## Recursive definition
              if (length(x) == 0)
                  return(logical(0))
              sapply(x, function(xx) all(isEmpty(xx)))
          })
### A List object is considered empty iff all its elements are empty.
setMethod("isEmpty", "List", function(x) all(elementLengths(x) == 0L))

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Constructors.
###

compress_listData <- function(x) {
    if (length(x) > 0L) {
        if (length(dim(x[[1L]])) < 2L) {
            x <- do.call(c, unname(x))
        } else {
            x <- do.call(rbind, unname(x))
        }
    } else {
        x <- vector()
    }
    x
}

reconcileMetadatacols <- function(x) {
  x_mcols <- mcols(x)
  if (is(x_mcols, "DataFrame") &&
      nrow(x_mcols) == 0L && ncol(x_mcols) == 0L)
    {
      x_mcols <- new("DataFrame", nrows=length(x))
      mcols(x) <- x_mcols
    }
  x
}

### NOT exported.
### Value for elementMetadata slot can be passed either with
###   newList(..., elementMetadata=somestuff)
### or with
###   newList(..., mcols=somestuff)
### The latter is the new recommended form.
newList <- function(Class, listData, ..., mcols)
{
    if (!extends(Class, "SimpleList") && !extends(Class, "CompressedList"))
        stop("class ", Class, " must extend SimpleList or CompressedList")
    if (!is.list(listData))
        stop("'listData' must be a list object")
    if (is.array(listData)) { # drop any unwanted dimensions
        tmp_names <- names(listData)
        dim(listData) <- NULL # clears the names
        names(listData) <- tmp_names
    }
    class(listData) <- "list"
    ans_elementType <- elementType(new(Class))
    if (!all(sapply(listData,
                    function(x) extends(class(x), ans_elementType))))
        stop("all elements in 'listData' must be ", ans_elementType, " objects")
    if (extends(Class, "SimpleList")) {
        if (missing(mcols))
            return(new2(Class, listData=listData, ..., check=FALSE))
        return(new2(Class, listData=listData, ..., elementMetadata=mcols,
                    check=FALSE))
    }
    ans_partitioning <- PartitioningByEnd(listData)
    if (length(listData) == 0L) {
        if (missing(mcols))
            return(new2(Class, partitioning=ans_partitioning, ..., check=FALSE))
        return(new2(Class, partitioning=ans_partitioning, ...,
                    elementMetadata=mcols, check=FALSE))
    }
    ans_unlistData <- compress_listData(listData)
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
    reconcileMetadatacols(ans)
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "show" method.
###

setMethod("show", "List",
          function(object)
          {
              lo <- length(object)
              cat(classNameForDisplay(object), " of length ", lo,
                  "\n", sep = "")
              if (!is.null(names(object)))
                cat(BiocGenerics:::labeledLine("names", names(object)))
          })


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Subsetting.
###

### Assumes 'x' and 'i' are parallel List objects (i.e. same length).
### Returns TRUE iff 'i' contains non-NA positive values that are compatible
### with the shape of 'x'.
.is_valid_NL_subscript <- function(i, x)
{
    unlisted_i <- unlist(i, use.names=FALSE)
    if (!is.integer(unlisted_i))
        unlisted_i <- as.integer(unlisted_i)
    if (anyMissingOrOutside(unlisted_i, lower=1L))
        return(FALSE)
    x_eltlens <- elementLengths(x)
    i_eltlens <- elementLengths(i)
    if (any(unlisted_i > rep.int(x_eltlens, i_eltlens)))
        return(FALSE)
    return(TRUE)
}

### Assumes 'x' and 'i' are parallel List objects (i.e. same length).
### Returns the name of one of the 3 supported fast paths ("LL", "NL", "RL")
### or NA if no fast path can be used.
.select_fast_path <- function(i, x)
{
    ## LEPType (List Element Pseudo-Type): same as "elementType" except for
    ## RleList objects.
    if (is(i, "RleList")) {
        i_runvals <- runValue(i)
        i_LEPType <- elementType(i_runvals)
    } else {
        i_LEPType <- elementType(i)
    }
    if (extends(i_LEPType, "logical")) {
        ## 'i' is a List of logical vectors or logical-Rle objects.
        ## We select the "LL" fast path ("Logical List").
        return("LL")
    }
    if (extends(i_LEPType, "numeric")) {
        ## 'i' is a List of numeric vectors or numeric-Rle objects.
        if (is(i, "RleList")) {
            i2 <- i_runvals
        } else {
            i2 <- i
        }
        if (.is_valid_NL_subscript(i2, x)) {
            ## We select the "NL" fast path ("Number List").
            return("NL")
        }
    }
    if (extends(i_LEPType, "Ranges")) {
        ## 'i' is a List of Ranges objects.
        ## We select the "RL" fast path ("Ranges List").
        return("RL")
    }
    return(NA_character_)
}

### Assumes 'x' and 'i' are parallel List objects (i.e. same length).
### Truncate or recycle each list element of 'i' to the length of the
### corresponding element in 'x'.
.adjust_elt_lengths <- function(i, x)
{
    x_eltlens <- unname(elementLengths(x))
    i_eltlens <- unname(elementLengths(i))
    idx <- which(x_eltlens != i_eltlens)
    ## FIXME: This is rough and doesn't follow exactly the truncate-or-recycle
    ## semantic of normalizeSingleBracketSubscript() on a logical vector or
    ## logical-Rle object.
    for (k in idx)
        i[[k]] <- rep(i[[k]], length.out=x_eltlens[k])
    return(i)
}

### Assumes 'x' and 'i' are parallel List objects (i.e. same length),
### and 'i' is a List of logical vectors or logical-Rle objects.
.unlist_LL_subscript <- function(i, x)
{
    i <- .adjust_elt_lengths(i, x)
    unlist(i, use.names=FALSE)
}

### Assumes 'x' and 'i' are parallel List objects (i.e. same length),
### and 'i' is a List of numeric vectors or numeric-Rle objects.
.unlist_NL_subscript <- function(i, x)
{
    offsets <- c(0L, end(PartitioningByEnd(x))[-length(x)])
    i <- i + offsets
    unlist(i, use.names=FALSE)
}

### Assumes 'x' and 'i' are parallel List objects (i.e. same length),
### and 'i' is a List of Ranges objects.
.unlist_RL_subscript <- function(i, x)
{
    unlisted_i <- unlist(i, use.names=FALSE)
    offsets <- c(0L, end(PartitioningByEnd(x))[-length(x)])
    shift(unlisted_i, shift=rep.int(offsets, elementLengths(i)))
}

### Fast subset by List of logical vectors or logical-Rle objects.
### Assumes 'x' and 'i' are parallel List objects (i.e. same length).
.fast_subset_List_by_LL <- function(x, i)
{
    ## Unlist 'x' and 'i'.
    unlisted_x <- unlist(x, use.names=FALSE)
    unlisted_i <- .unlist_LL_subscript(i, x)

    ## Subset.
    unlisted_ans <- extractROWS(unlisted_x, unlisted_i)

    ## Relist.
    group <- rep.int(seq_along(x), elementLengths(x))
    group <- extractROWS(group, unlisted_i)
    ans_skeleton <- PartitioningByEnd(group, NG=length(x), names=names(x))
    ans <- as(relist(unlisted_ans, ans_skeleton), class(x))
    metadata(ans) <- metadata(x)
    ans
}

### Fast subset by List of numeric vectors or numeric-Rle objects.
### Assumes 'x' and 'i' are parallel List objects (i.e. same length).
.fast_subset_List_by_NL <- function(x, i)
{
    ## Unlist 'x' and 'i'.
    unlisted_x <- unlist(x, use.names=FALSE)
    unlisted_i <- .unlist_NL_subscript(i, x)

    ## Subset.
    unlisted_ans <- extractROWS(unlisted_x, unlisted_i)

    ## Relist.
    ans_breakpoints <- cumsum(unname(elementLengths(i)))
    ans_skeleton <- PartitioningByEnd(ans_breakpoints, names=names(x))
    ans <- as(relist(unlisted_ans, ans_skeleton), class(x))
    metadata(ans) <- metadata(x)
    ans
}

### Fast subset by List of Ranges objects.
### Assumes 'x' and 'i' are parallel List objects (i.e. same length).
.fast_subset_List_by_RL <- function(x, i)
{
    ## Unlist 'x' and 'i'.
    unlisted_x <- unlist(x, use.names=FALSE)
    unlisted_i <- .unlist_RL_subscript(i, x)

    ## Subset.
    unlisted_ans <- extractROWS(unlisted_x, unlisted_i)

    ## Relist.
    ans_breakpoints <- cumsum(unlist(sum(width(i)), use.names=FALSE))
    ans_skeleton <- PartitioningByEnd(ans_breakpoints, names=names(x))
    ans <- as(relist(unlisted_ans, ans_skeleton), class(x))
    metadata(ans) <- metadata(x)
    ans
}

### Subset a List object by a list-like subscript.
subset_List_by_List <- function(x, i)
{
    li <- length(i)
    if (is.null(names(i))) {
        lx <- length(x)
        if (li > lx)
            stop("list-like subscript is longer than ",
                 "list-like object to subset")
        if (li < lx)
            x <- x[seq_len(li)]
    } else {
        if (is.null(names(x)))
            stop("cannot subscript an unnamed list-like object ",
                 "by a named list-like object")
        if (!identical(names(i), names(x))) {
            i2x <- match(names(i), names(x))
            if (anyMissing(i2x))
                stop("list-like subscript has names not in ",
                     "list-like object to subset")
            x <- x[i2x]
        }
    }
    ## From here, 'x' and 'i' are guaranteed to have the same length.
    if (li == 0L)
        return(x)
    if (!is(x, "SimpleList")) {
        ## We'll try to take a fast path.
        if (is(i, "List")) {
            fast_path <- .select_fast_path(i, x)
        } else {
            i2 <- as(i, "List")
            i2_elttype <- elementType(i2)
            if (length(i2) == li && all(sapply(i, is, i2_elttype))) {
                fast_path <- .select_fast_path(i2, x)
                if (!is.na(fast_path))
                    i <- i2
            } else {
                fast_path <- NA_character_
            }
        }
        if (!is.na(fast_path)) {
            fast_path_FUN <- switch(fast_path,
                                    LL=.fast_subset_List_by_LL,
                                    NL=.fast_subset_List_by_NL,
                                    RL=.fast_subset_List_by_RL)
            return(fast_path_FUN(x, i))  # fast path
        }
    }
    ## Slow path (loops over the list elements of 'x').
    for (k in seq_len(li))
        x[[k]] <- extractROWS(x[[k]], i[[k]])
    return(x)
}

.adjust_value_length <- function(value, i_len)
{
    value_len <- length(value)
    if (value_len == i_len)
        return(value)
    if (value_len %% i_len != 0L)
        warning("number of values supplied is not a sub-multiple ",
                "of the number of values to be replaced")
    rep(value, length.out=i_len)
}

### Assumes 'x' and 'i' are parallel List objects (i.e. same length).
.fast_lsubset_List_by_List <- function(x, i, value)
{
    ## Unlist 'x', 'i', and 'value'.
    unlisted_x <- unlist(x, use.names=FALSE)
    fast_path <- .select_fast_path(i, x)
    unlist_subscript_FUN <- switch(fast_path,
                                   LL=.unlist_LL_subscript,
                                   NL=.unlist_NL_subscript,
                                   RL=.unlist_RL_subscript)
    unlisted_i <- unlist_subscript_FUN(i, x)
    if (length(value) != 1L) {
        value <- .adjust_value_length(value, length(i))
        value <- .adjust_elt_lengths(value, i)
    }
    unlisted_value <- unlist(value, use.names=FALSE)

    ## Subset.
    unlisted_ans <- replaceROWS(unlisted_x, unlisted_i, unlisted_value)

    ## Relist.
    ans <- as(relist(unlisted_ans, x), class(x))
    metadata(ans) <- metadata(x)
    ans
}

lsubset_List_by_List <- function(x, i, value)
{
    lx <- length(x)
    li <- length(i)
    if (li == 0L) {
        ## Surprisingly, in that case, `[<-` on standard vectors does not
        ## even look at 'value'. So neither do we...
        return(x)
    }
    lv <- length(value)
    if (lv == 0L)
        stop("replacement has length zero")
    value <- normalizeSingleBracketReplacementValue(value, x)
    if (is.null(names(i))) {
        if (li != lx)
            stop("when list-like subscript is unnamed, it must have the ",
                 "length of list-like object to subset")
        if (!is(x, "SimpleList")) {
            ## We'll try to take a fast path.
            if (is(i, "List")) {
                fast_path <- .select_fast_path(i, x)
            } else {
                i2 <- as(i, "List")
                i2_elttype <- elementType(i2)
                if (length(i2) == li && all(sapply(i, is, i2_elttype))) {
                    fast_path <- .select_fast_path(i2, x)
                    if (!is.na(fast_path))
                        i <- i2
                } else {
                    fast_path <- NA_character_
                }
            }
            if (!is.na(fast_path))
                return(.fast_lsubset_List_by_List(x, i, value))  # fast path
        }
        i2x <- seq_len(li)
    } else {
        if (is.null(names(x)))
            stop("cannot subset an unnamed list-like object ",
                 "by a named list-like subscript")
        i2x <- match(names(i), names(x))
        if (anyMissing(i2x))
            stop("list-like subscript has names not in ",
                 "list-like object to subset")
        if (anyDuplicated(i2x))
            stop("list-like subscript has duplicated names")
    }
    value <- .adjust_value_length(value, li)
    ## Slow path (loops over the list elements of 'x').
    for (k in seq_len(li))
        x[[i2x[k]]] <- replaceROWS(x[[i2x[k]]], i[[k]], value[[k]])
    return(x)
}

setMethod("[", "List",
    function(x, i, j, ..., drop=TRUE)
    {
        if (!missing(j) || length(list(...)) > 0L)
            stop("invalid subsetting")
        if (!missing(i) && (is.list(i) || (is(i, "List") && !is(i, "Ranges"))))
            return(subset_List_by_List(x, i))
        callNextMethod(x, i)
    }
)

setReplaceMethod("[", "List",
    function(x, i, j, ..., value)
    {
        if (!missing(j) || length(list(...)) > 0L)
            stop("invalid subsetting")
        if (!missing(i) && (is.list(i) || (is(i, "List") && !is(i, "Ranges"))))
                return(lsubset_List_by_List(x, i, value))
        callNextMethod(x, i, value=value)
    }
)

setMethod("[[", "List",
    function(x, i, j, ...)
    {
        dotArgs <- list(...)
        if (length(dotArgs) > 0L)
            dotArgs <- dotArgs[names(dotArgs) != "exact"]
        if (!missing(j) || length(dotArgs) > 0L)
            stop("incorrect number of subscripts")
        ## '...' is either empty or contains only the 'exact' arg.
        getListElement(x, i, ...)
    }
)

setMethod("$", "List", function(x, name) x[[name, exact=FALSE]])

setReplaceMethod("[[", "List",
                 function(x, i, j, ..., value)
                 {
                   if (!missing(j) || length(list(...)) > 0)
                     stop("invalid replacement")
                   origLen <- length(x)
                   x <- setListElement(x, i, value)
                   if (origLen < length(x))
                     x <- rbindRowOfNAsToMetadatacols(x)
                   x
                 })

setReplaceMethod("$", "List",
                 function(x, name, value) {
                   x[[name]] <- value
                   x
                 })


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Looping methods.
###

setMethod("lapply", "List",
          function(X, FUN, ...)
          {
              FUN <- match.fun(FUN)
              ii <- seq_len(length(X))
              names(ii) <- names(X)
              lapply(ii, function(i) FUN(X[[i]], ...))
          })

.sapplyDefault <- base::sapply
environment(.sapplyDefault) <- topenv()
setMethod("sapply", "List", .sapplyDefault)

.mapply_List <- function(FUN, ..., MoreArgs = NULL, SIMPLIFY = TRUE,
                         USE.NAMES = TRUE)
{
    seqs <- list(...)
    isListOrVector <- function(x) is.vector(x) | is(x, "List")
    if (any(!sapply(seqs, isListOrVector)))
        stop("all objects in ... should be a vector or 'List'")
    elens <- sapply(seqs, length) ## elementLengths uses NROW, inappropriate
    if (any(elens == 0L))
      return(list())
    N <- max(elens)
    if (any(N %% elens != 0L))
        stop("all object lengths must be multiple of longest object length")
    recycleExtract <- function(x, i) x[[(i - 1L) %% length(x) + 1L]]
    FUNprime <- function(.__INDEX__, ...) {
        do.call(FUN, c(lapply(seqs, recycleExtract, .__INDEX__), ...))
    }
    nms <- names(seqs[[1]])
    if (is.null(nms) && is.character(seqs[[1]]))
      nms <- seqs[[1]]
    mapply(FUNprime, structure(seq_len(N), names = nms),
           MoreArgs = MoreArgs, SIMPLIFY = SIMPLIFY,
           USE.NAMES = USE.NAMES)
}

setMethod("mapply", "List", .mapply_List)

setMethod("endoapply", "List",
          function(X, FUN, ...) {
              elementTypeX <- elementType(X)
              FUN <- match.fun(FUN)
              for (i in seq_len(length(X))) {
                  elt <- FUN(X[[i]], ...)
                  if (!extends(class(elt), elementTypeX))
                      stop("'FUN' must return elements of class ", elementTypeX)
                  X[[i]] <- elt
              }
              X
          })

setMethod("mendoapply", "List",
          function(FUN, ..., MoreArgs = NULL) {
              X <- list(...)[[1L]]
              elementTypeX <- elementType(X)
              FUN <- match.fun(FUN)
              listData <-
                mapply(FUN = FUN, ..., MoreArgs = MoreArgs, SIMPLIFY = FALSE)
              for (i in seq_len(length(listData))) {
                  if (!extends(class(listData[[i]]), elementTypeX))
                      stop("'FUN' must return elements of class ", elementTypeX)
                  X[[i]] <- listData[[i]]
              }
              X
          })

asList <- function(x, ...) {
  if (is(x, "List"))
    return(x)
  if (!is.list(x))
    stop("'x' must be a 'list'")
  cl <- lapply(x, class)
  clnames <- unique(unlist(cl, use.names=FALSE))
  cons <- SimpleList
  if (length(clnames) == 1L) {
    cl <- cl[[1]]
    pkg <- packageSlot(cl)
  } else if (length(clnames)) {
    contains <- lapply(cl, function(x) getClass(x, TRUE)@contains)
    clnames <- c(clnames,
                 unlist(lapply(contains, names), use.names=FALSE))
    contab <- table(factor(clnames, unique(clnames)))
    cl <- names(contab)[contab == length(x)]
    if (length(cl))
      pkg <- sapply(do.call(c, unname(contains))[cl], packageSlot)
  }
  if (length(cl)) {
    constructorName <- function(x) {
      substring(x, 1, 1) <- toupper(substring(x, 1, 1))
      paste(x, "List", sep = "")
    }
    if (is.null(pkg))
      ns <- topenv()
    else ns <- getNamespace(pkg[1])
    consym <- constructorName(cl[1])
    if (exists(consym, ns))
      cons <- get(consym, ns)
    else {
      if (length(cl) == 1L) {
        contains <- getClass(cl, TRUE)@contains
        cl <- names(contains)
        pkg <- sapply(contains, packageSlot)
      } else {
        cl <- tail(cl, -1)
        pkg <- tail(pkg, -1)
      }
      if (length(cl)) {
        if (!length(pkg))
          ns <- list(topenv())
        connms <- constructorName(cl)
        ns <- lapply(pkg, getNamespace)
        coni <- head(which(mapply(exists, connms, ns)), 1)
        if (length(coni))
          cons <- get(connms[coni], ns[[coni]])
      }
    }
  }
  cons(x, ...)
}

## FIXME: these functions should probably be renamed to c[apply], i.e.,
## clapply, cmapply, ctapply, csplit, cby.

seqapply <- function(X, FUN, ...) {
  asList(lapply(X, FUN, ...))
}

mseqapply <- function(FUN, ..., MoreArgs = NULL, USE.NAMES = TRUE) {
  asList(.mapply_List(FUN, ..., MoreArgs = MoreArgs, SIMPLIFY = FALSE,
                        USE.NAMES = USE.NAMES))
}

tseqapply <- function(X, INDEX, FUN = NULL, ...) {
  asList(tapply(X, INDEX, FUN, ..., simplify = FALSE))
}

seqsplit <- function(x, f, drop=FALSE) {
  ans_class <- try(splitAsListReturnedClass(x), silent=TRUE)
  if (inherits(ans_class, "try-error"))
    return(asList(split(x, f, drop)))
  splitAsList(x, f, drop=drop)
}

seqby <- function(data, INDICES, FUN, ...) {
  asList(by(data, INDICES, FUN, ..., simplify = FALSE))
}

setGeneric("revElements", signature="x",
    function(x, i) standardGeneric("revElements")
)

### This method explains the concept of revElements() but is NOT efficient
### because endoapply() loops over the elements of 'i'.
### There is a fast method for CompressedList objects though.
setMethod("revElements", "List",
    function(x, i)
    {
        if (missing(i))
            i <- seq_len(length(x))
        x[i] <- endoapply(x[i], rev)
        x
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion.
###

setAs("List", "list", function(from) as.list(from))

.as.list.List <- function(x, use.names=TRUE)
{
    if (!isTRUEorFALSE(use.names))
        stop("'use.names' must be TRUE or FALSE")
    ans <- lapply(x, identity)
    if (!use.names)
        names(ans) <- NULL
    ans
}
### S3/S4 combo for as.list.List
as.list.List <- function(x, ...) .as.list.List(x, ...)
setMethod("as.list", "List", as.list.List)

setGeneric("as.env", function(x, ...) standardGeneric("as.env"))

setMethod("as.env", "List",
          function(x, enclos = parent.frame(2)) {
              nms <- names(x)
              if (is.null(nms))
                  stop("cannot convert to environment when names are NULL")
              env <- new.env(parent = enclos)
              lapply(nms,
                     function(col) {
                         colFun <- function() {
                             val <- x[[col]]
                             rm(list=col, envir=env)
                             assign(col, val, env)
                             val
                         }
                         makeActiveBinding(col, colFun, env)
                     })
              env
          })

listClassName <- function(impl, element.type) {
  if (is.null(impl))
    impl <- ""
  if (!is.null(element.type)) {
    cl <- c(element.type, names(getClass(element.type)@contains), "")
    cl <- capitalize(cl)
  } else {
    cl <- ""
  }
  listClass <- c(paste0(impl, cl, "List"), paste0(cl, "List"))
  clExists <- which(sapply(listClass, isClass) &
                    sapply(listClass, extends, paste0(impl, "List")))
  if (length(clExists) == 0L) {
    stop("Could not find a '", impl,
         "List' subclass for values of type ",
         paste("'", cl, "'", collapse = ", "))
  }
  listClass[clExists[1L]]
}

coerceToList <- function(from, element.type = NULL, ...) {
  if (is(from, listClassName(NULL, element.type)))
    return(from)
  coerceToCompressedList(from, element.type, ...)
}

setAs("ANY", "List", function(from) {
  coerceToList(from)
})

## Special cased, because integer extends ANY (somehow) and numeric,
## so ambiguities are introduced due to method caching.
setAs("integer", "List", getMethod(coerce, c("ANY", "List")))

### NOT exported. Assumes 'names1' is not NULL.
make.unlist.result.names <- function(names1, names2)
{
    if (is.null(names2))
        return(names1)
    idx2 <- names2 != "" | is.na(names2)
    idx1 <- names1 != "" | is.na(names1)
    idx <- idx1 & idx2
    if (any(idx))
        names1[idx] <- paste(names1[idx], names2[idx], sep = ".")
    idx <- !idx1 & idx2
    if (any(idx))
        names1[idx] <- names2[idx]
    names1
}

setMethod("unlist", "List",
    function(x, recursive=TRUE, use.names=TRUE)
    {
        if (!identical(recursive, TRUE))
            stop("\"unlist\" method for List objects ",
                 "does not support the 'recursive' argument")
        if (!isTRUEorFALSE(use.names))
            stop("'use.names' must be TRUE or FALSE")
        if (length(x) == 0L)
            return(NULL)
        x_names <- names(x)
        if (!is.null(x_names))
            names(x) <- NULL
        xx <- as.list(x)
        if (length(dim(xx[[1L]])) < 2L) {
            ans <- do.call(c, xx)
            ans_names0 <- names(ans)
            if (use.names) {
                if (!is.null(x_names)) {
                    ans_names <- rep.int(x_names, elementLengths(x))
                    ans_names <- make.unlist.result.names(ans_names, ans_names0)
                    try_result <- try(names(ans) <- ans_names, silent=TRUE)
                    if (inherits(try_result, "try-error"))
                        warning("failed to set names on the result ",
                                "of unlisting a ", class(x), " object")
                }
            } else {
                ## This is consistent with base::unlist but is not consistent
                ## with unlist,CompressedList. See comments and FIXME note in
                ## the unlist,CompressedList code for more details.
                if (!is.null(ans_names0))
                    names(ans) <- NULL
            }
        } else {
            ans <- do.call(rbind, xx)
            if (!use.names)
                rownames(ans) <- NULL
        }
        ans
    }
)

setMethod("unsplit", "List", function(value, f, drop = FALSE) {
  value_flat <- unlist(value, use.names = FALSE)
  if (length(value_flat) != length(f))
    stop("Length of 'unlist(value)' must equal length of 'f'")
  seqsplit(value_flat, f, drop = drop) <- value
  value_flat
})

.stack.ind <- function(x, index.var = "name") {
  if (length(names(x)) > 0) {
    spaceLabels <- names(x)
  } else {
    spaceLabels <- seq_len(length(x))
  }
  ind <- Rle(factor(spaceLabels, levels = unique(spaceLabels)),
             elementLengths(x))
  do.call(DataFrame, structure(list(ind), names = index.var))
}

setMethod("stack", "List",
          function(x, index.var = "name", value.var = "value", name.var = NULL)
          {
            df <- DataFrame(.stack.ind(x, index.var),
                            as(unlist(x, use.names=FALSE), "DataFrame"))
            colnames(df)[2] <- value.var
            if (!is.null(name.var)) {
              nms <- as.character(unlist(lapply(x, names)))
              if (length(nms) == 0L)
                nms <- as.character(unlist(lapply(elementLengths(x), seq_len)))
              df[[name.var]] <- factor(nms, unique(nms))
            }
            df
          })


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Evaluating.
###
  
setMethod("eval", c("expression", "List"),
    function(expr, envir, enclos = parent.frame())
          eval(expr, as.env(envir, enclos))
)

setMethod("eval", c("language", "List"),
    function(expr, envir, enclos = parent.frame())
          eval(expr, as.env(envir, enclos))
)

setMethod("with", "List",
          function(data, expr, ...)
          {
            eval(substitute(expr), data, parent.frame())
          })

setMethod("within", "List",
          function(data, expr, ...)
          {
            ## cannot use active bindings here, as they break for replacement
            e <- list2env(as.list(data))
            ##e <- as.env(data)
            eval(substitute(expr), e, parent.frame(2))
            l <- mget(ls(e), e)
            l <- l[!sapply(l, is.null)]
            nD <- length(del <- setdiff(names(data), (nl <- names(l))))
            for (nm in nl)
              data[[nm]] <- l[[nm]]
            for (nm in del) 
              data[[nm]] <- NULL
            data
          })

setMethod("do.call", c("ANY", "List"),
          function (what, args, quote = FALSE, envir = parent.frame()) {
            args <- as.list(args)
            callGeneric()
          })
