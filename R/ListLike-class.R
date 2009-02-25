### =========================================================================
### ListLike objects
### -------------------------------------------------------------------------
###
### A class can be considered to have a "list semantic" if the "length", "[[",
### and "names" methods are defined for it. In that case it can contain
### the "ListLike" no-slot virtual class in order to inherit the "lapply",
### "sapply" and "as.list" methods defined below.

setClass("ListLike", representation("VIRTUAL"))

setMethod("lapply", c("ListLike", "ANY"),
    function(X, FUN, ...)
    {
        ii <- seq_len(length(X))
        names(ii) <- names(X)
        lapply(ii, function(i) FUN(X[[i]]), ...)
    }
)

### The implementation of the "sapply" method below was copy-pasted as-is from
### base::sapply(). Duplicating code is ugly but unfortunately we have to
### define this generic+method, otherwise, when passed a ListLike object,
### sapply(X) would call base::sapply() which would call base::lapply() behind
### the scene and not IRanges::lapply() (generic function implicitly defined by
### the setMethod statement above).
### This ugliness could be avoided if base::lapply() itself was a generic.
### Anybody willing to bring this on R-devel?
setGeneric("sapply", signature=c("X", "FUN"),
    function(X, FUN, ..., simplify=TRUE, USE.NAMES=TRUE)
        standardGeneric("sapply")
)
setMethod("sapply", c("ListLike", "ANY"),
    function(X, FUN, ..., simplify=TRUE, USE.NAMES=TRUE)
    {
        FUN <- match.fun(FUN)
        answer <- lapply(X, FUN, ...)
        if (USE.NAMES && is.character(X) && is.null(names(answer))) 
            names(answer) <- X
        if (simplify && length(answer)
         && length(common.len <- unique(unlist(lapply(answer, length)))) == 1L)
        {
            if (common.len == 1L) 
                unlist(answer, recursive = FALSE)
            else if (common.len > 1L) 
                array(unlist(answer, recursive = FALSE), dim = c(common.len, 
                    length(X)), dimnames = if (!(is.null(n1 <- names(answer[[1L]])) & 
                    is.null(n2 <- names(answer)))) 
                    list(n1, n2))
            else answer
        }
        else answer
    }
)

### An alternative to the lapply-based as.list() implementation below would be
### to go the other way around:
###
###     setMethod("as.list", "ListLike",
###         function(x, ...)
###         {
###             ii <- seq_len(length(X))
###             names(ii) <- names(X)
###             lapply(ii, function(i) X[[i]])
###         }
###     )
###
###     setMethod("lapply", c("ListLike", "ANY"),
###         function(X, FUN, ...) lapply(as.list(X), FUN, ...)
###     )
###
### But this as.list-based lapply() implementation is not as memory efficient
### on big objects because it coerces X to a temporary list.
setMethod("as.list", "ListLike",
    function(x, ...) lapply(x, identity)
)

