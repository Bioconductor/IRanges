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

### The implicit generic would dispatch on the X, FUN, simplify and USE.NAMES
### args but we don't want that.
setGeneric("sapply", signature="X",
    function(X, FUN, ..., simplify=TRUE, USE.NAMES=TRUE)
        standardGeneric("sapply")
)

setMethod("sapply", "ListLike",
    function(X, FUN, ..., simplify=TRUE, USE.NAMES=TRUE)
    {
        ii <- seq_len(length(X))
        names(ii) <- names(X)
        sapply(ii, function(i) FUN(X[[i]]), ..., simplify=simplify, USE.NAMES=USE.NAMES)
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

