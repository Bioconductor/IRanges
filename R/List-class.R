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

setMethod("elementLengths", "ANY", S4Vectors:::sapply_NROW)

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

List <- function(...) {
  list <- list(...)
  if (length(list) == 1 && is.list(list[[1L]])) 
    list <- list[[1L]]
  as(list, "List")
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
    if (S4Vectors:::anyMissingOrOutside(unlisted_i, lower=1L))
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
            if (S4Vectors:::anyMissing(i2x))
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
    if (i_len %% value_len != 0L)
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
        if (S4Vectors:::anyMissing(i2x))
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
### Simple helper functions for some common subsetting operations.
###

### phead() and ptail(): "parallel" versions of head() and tail() for List
### objects. They're just fast equivalents of 'mapply(head, x, n)' and
### 'mapply(tail, x, n)', respectively.
.normarg_n <- function(n, x_eltlens)
{
    if (!is.numeric(n))
        stop("'n' must be an integer vector")
    if (!is.integer(n))
        n <- as.integer(n)
    if (any(is.na(n)))
        stop("'n' cannot contain NAs")
    n <- pmin(x_eltlens, n)
    neg_idx <- which(n < 0L)
    if (length(neg_idx) != 0L)
        n[neg_idx] <- pmax(n[neg_idx] + x_eltlens[neg_idx], 0L)
    n
}

phead <- function(x, n=6L)
{
    x_eltlens <- unname(elementLengths(x))
    n <- .normarg_n(n, x_eltlens)
    unlisted_i <- IRanges(start=rep.int(1L, length(n)), width=n)
    i <- relist(unlisted_i, PartitioningByEnd(seq_along(x)))
    x[i]
}

ptail <- function(x, n=6L)
{
    x_eltlens <- unname(elementLengths(x))
    n <- .normarg_n(n, x_eltlens)
    unlisted_i <- IRanges(end=x_eltlens, width=n)
    i <- relist(unlisted_i, PartitioningByEnd(seq_along(x)))
    x[i]
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### compareRecursively()
###
### NOT exported!
### See R/List-comparison.R for the details.
###

setGeneric("compareRecursively",
    function(x) standardGeneric("compareRecursively")
)


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

mapply_List <- function(FUN, ..., MoreArgs = NULL, SIMPLIFY = TRUE,
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

setMethod("as.env", "List",
          function(x, enclos = parent.frame(2), tform = identity) {
              nms <- names(x)
              if (is.null(nms))
                  stop("cannot convert to environment when names are NULL")
              env <- new.env(parent = enclos)
              lapply(nms,
                     function(col) {
                         colFun <- function() {
                             val <- tform(x[[col]])
                             rm(list=col, envir=env)
                             assign(col, val, env)
                             val
                         }
                         makeActiveBinding(col, colFun, env)
                     })
              env
          })

listClassName <- function(impl, element.type) {
  listClass <- paste0(if (is.null(impl)) "Simple" else impl, "List")
  if (!is.null(element.type)) {
    cl <- c(element.type, names(getClass(element.type)@contains))
    cl <- S4Vectors:::capitalize(cl)
    listClass <- c(paste0(cl, "List"), paste0(cl, "Set"),
                   paste0(impl, cl, "List"), listClass)
  }
  clExists <- which(sapply(listClass, isClass) &
                    sapply(listClass, extends, paste0(impl, "List")))
  listClass[[clExists[[1L]]]]
}

selectListClassName <- function(x) {
  cn <- listClassName("Compressed", x)
  if (cn == "CompressedList")
    cn <- listClassName(NULL, x)
  cn
}

setAs("ANY", "List", function(from) {
  ## since list is directed to SimpleList, we assume 'from' is non-list-like
  relist(from, PartitioningByEnd(seq_len(length(from))))
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

setAs("List", "data.frame", function(from) as.data.frame(from))

### S3/S4 combo for as.data.frame.List
as.data.frame.List <- 
    function(x, row.names=NULL, optional=FALSE, ..., value.name="value",
             use.outer.mcols=FALSE, group_name.as.factor=FALSE)
{
    if (!length(togroup(x)))
        return(data.frame())
    if (!isSingleString(value.name))
        stop("'value.name' must be a single string")
    if (!isTRUEorFALSE(use.outer.mcols))
        stop("'use.outer.mcols' must be TRUE or FALSE")
    if (!isTRUEorFALSE(group_name.as.factor))
        stop("'group_name.as.factor' must be TRUE or FALSE")
    if (!(is.null(row.names) || is.character(row.names)))
        stop("'row.names'  must be NULL or a character vector")

    if (!length(group_name <- names(x)[togroup(x)]))
        group_name <- NA_character_
    if (group_name.as.factor)
        group_name <- factor(group_name, levels=unique(group_name))
    xx <- cbind(data.frame(group=togroup(x), group_name, 
                           stringsAsFactors=FALSE), 
                as.data.frame(unlist(x, use.names=FALSE), 
                              row.names=row.names, optional=optional, ...))
    if (ncol(xx) == 3)
        colnames(xx)[3] <- value.name
    if (use.outer.mcols)
        if (length(md <- mcols(x)[togroup(x), , drop=FALSE]))
            return(cbind(xx, md))

    xx
}
setMethod("as.data.frame", "List", as.data.frame.List)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Evaluating.
###

setMethod("within", "List",
          function(data, expr, ...)
          {
            ## cannot use active bindings here, as they break for replacement
            e <- list2env(as.list(data))
            ##e <- as.env(data)
            S4Vectors:::safeEval(substitute(expr), e, S4Vectors:::top_prenv(expr))
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
