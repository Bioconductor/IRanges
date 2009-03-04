### =========================================================================
### ListLike objects
### -------------------------------------------------------------------------
###
### A class can be considered to have a "list semantic" if the "length", "[[",
### and "names" methods are defined for it. In that case it can contain
### the "ListLike" no-slot virtual class in order to inherit the "lapply",
### "sapply" and "as.list" methods defined below.

setClass("ListLike", representation("VIRTUAL"))

### We provide a default "[[" method for ListLike objects. It contains some
### logic that can be reused by the real "[[" methods for real list-like
### classes. Typically, these real methods will start by doing something like
###     i <- callNextMethod()
### and the 'i' they will get will be a valid index for 'x' i.e. a single
### integer such that 1 <= i <= length(x). Note that this "subscript checking
### and translation" is a common need for "[[" methods in general, not only for
### the "[[" method for ListLike objects. Because of this, the code was put in
### the checkAndTranslateDbleBracketSubscript() function (not exported), so any
### "[[" method can reuse it.
###
### The supported subscript types are (a) single numeric value or (b) character
### string. Return a single integer in both cases. If (a) => check the
### subscript and return it as an integer. If (b) => return position of the
### subscript in 'names(x)'.
### Note that the formal arguments of the "[[" generic are (x, i, j, ...), not
### (x, i, j, ..., exact=TRUE), so we need to support the 'exact' argument thru
### the dots.
checkAndTranslateDbleBracketSubscript <- function(x, i, j, ...)
{
    subscripts <- list(...)
    if ("exact" %in% names(subscripts)) {
        exact <- subscripts[["exact"]]
        subscripts[["exact"]] <- NULL
    } else {
        exact <- TRUE  # default
    }
    if (!missing(i))
        subscripts$i <- i
    if (!missing(j))
        subscripts$j <- j
    if (length(subscripts) != 1L)
        stop("incorrect number of subscripts")
    subscript <- subscripts[[1]]
    if (!is.character(subscript) && !is.numeric(subscript))
        stop("invalid subscript type '", class(subscript), "'")
    if (length(subscript) < 1L)
        stop("attempt to extract less than one element")
    if (length(subscript) > 1L)
        stop("attempt to extract more than one element")
    if (is.na(subscript))
        stop("invalid subscript NA")
    if (is.numeric(subscript)) {
        if (!is.integer(subscript))
            subscript <- as.integer(subscript)
        if (subscript < 1L || length(x) < subscript)
            stop("subscript out of bounds")
        return(subscript)
    }
    ## 'subscript' is a character string
    names_x <- names(x)
    if (is.null(names_x))
        stop("attempt to extract by name when elements have no names")
    #if (subscript == "")
    #    stop("invalid subscript \"\"")
    ans <- charmatch(subscript, names_x)
    if (is.na(ans))
        stop("subscript \"", subscript, "\" matches no name")
    if (ans == 0L) {
        if (isTRUE(exact))
            stop("subscript \"", subscript, "\" matches no name or more than one name")
        stop("subscript \"", subscript, "\" matches more than one name")
    }
    if (isTRUE(exact) && nchar(subscript) != nchar(names_x[ans]))
        stop("subscript \"", subscript, "\" matches no name")
    ans
}
setMethod("[[", "ListLike", checkAndTranslateDbleBracketSubscript)

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

