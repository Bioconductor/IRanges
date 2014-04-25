### =========================================================================
### Subsetting utility functions
### -------------------------------------------------------------------------


setClassUnion("vectorORfactor", c("vector", "factor"))


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Formal representation of a normalized single bracket subscript.
###

setClass("SmartSubscript", representation("VIRTUAL"))

setClass("NormalizedSubscript",
    contains="SmartSubscript",
    representation(
        subscript="integer", # Must be of length 0 if object_len=0, otherwise
                             # length is arbitrary and values must be >= 1 and
                             # <= object_len.
        object_len="integer",  # Single integer >= 0.
        allow_past_object_end="logical"  # TRUE or FALSE
    ),
    prototype(
        object_len=0L,
        allow_past_object_end=FALSE
    )
)

setMethod("length", "NormalizedSubscript", function(x) length(x@subscript))

setGeneric("subscript", function(x) standardGeneric("subscript"))

setMethod("subscript", "NormalizedSubscript", function(x) x@subscript)

### Supplied arguments are trusted. We don't check them!
NormalizedSubscript <- function(subscript=integer(0), object_len=0,
                                allow_past_object_end=FALSE)
{
    new("NormalizedSubscript", subscript=subscript,
        object_len=object_len,
        allow_past_object_end=allow_past_object_end)
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### fromSubscriptToPositiveIntegers()
###
### Not intended to be called directly from "extractROWS" or "replaceROWS"
### methods. Call normalizeSingleBracketSubscript() instead.
###

setGeneric("fromSubscriptToPositiveIntegers", signature="i",  # not exported
    function(i, x, byrow=FALSE, exact=TRUE, allow.append=FALSE)
        standardGeneric("fromSubscriptToPositiveIntegers")
)

setMethod("fromSubscriptToPositiveIntegers", "NULL",
    function(i, x, byrow=FALSE, exact=TRUE, allow.append=FALSE)
        integer(0)
)

setMethod("fromSubscriptToPositiveIntegers", "missing",
    function(i, x, byrow=FALSE, exact=TRUE, allow.append=FALSE)
    {
        if (byrow) {
            x_len <- nrow(x)
        } else {
            x_len <- length(x)
        }
        seq_len(x_len)
    }
)

setMethod("fromSubscriptToPositiveIntegers", "numeric",
    function(i, x, byrow=FALSE, exact=TRUE, allow.append=FALSE)
    {
        if (byrow) {
            x_len <- nrow(x)
        } else {
            x_len <- length(x)
        }
        if (!is.integer(i))
            i <- as.integer(i)
        if (allow.append) {
            if (any(is.na(i)))
                stop("subscript contains NAs")
        } else {
            if (S4Vectors:::anyMissingOrOutside(i, upper=x_len))
                stop("subscript contains NAs or out-of-bounds indices")
        }
        nonzero_idx <- which(i != 0L)
        i <- i[nonzero_idx]
        if (length(i) == 0L)
            return(i)
        any_pos <- any(i > 0L)
        any_neg <- any(i < 0L)
        if (any_neg && any_pos)
            stop("cannot mix negative with positive indices")
        ## From here, indices are guaranteed to be either all positive or
        ## all negative.
        if (any_neg)
            i <- seq_len(x_len)[i]
        i
    }
)

setMethod("fromSubscriptToPositiveIntegers", "logical",
    function(i, x, byrow=FALSE, exact=TRUE, allow.append=FALSE)
    {
        if (byrow) {
            x_len <- nrow(x)
        } else {
            x_len <- length(x)
        }
        if (S4Vectors:::anyMissing(i))
            stop("subscript contains NAs")
        li <- length(i)
        if (!allow.append && li > x_len) {
            if (any(i[(x_len+1L):li]))
                stop("subscript is a logical vector with out-of-bounds ",
                     "TRUE values")
            i <- i[seq_len(x_len)]
        }
        if (li < x_len)
            i <- rep(i, length.out=x_len)
        which(i)
    }
)

fromSubscriptToPositiveIntegers.characterORfactor <-
    function(i, x, byrow=FALSE, exact=TRUE, allow.append=FALSE)
{
    if (byrow) {
        x_len <- nrow(x)
        x_names <- rownames(x)
        what <- "rownames"
    } else {
        x_len <- length(x)
        x_names <- names(x)
        what <- "names"
    }
    if (is.null(x_names)) {
        if (!allow.append)
            stop("cannot subset by character when ", what, " are NULL")
        i <- x_len + seq_along(i)
        return(i)
    }
    if (exact) {
        i <- match(i, x_names, incomparables=c(NA_character_, ""))
    } else {
        i <- pmatch(i, x_names, duplicates.ok=TRUE)
    }
    if (allow.append) {
        na_idx <- which(is.na(i))
        i[na_idx] <- x_len + seq_along(na_idx)
        return(i)
    }
    if (S4Vectors:::anyMissing(i))
        stop("subscript contains invalid ", what)
    i
}

setMethod("fromSubscriptToPositiveIntegers", "character",
    fromSubscriptToPositiveIntegers.characterORfactor
)

setMethod("fromSubscriptToPositiveIntegers", "factor",
    fromSubscriptToPositiveIntegers.characterORfactor
)

setMethod("fromSubscriptToPositiveIntegers", "NormalizedSubscript",
    function(i, x, byrow=FALSE, exact=TRUE, allow.append=FALSE)
    {
        if (byrow) {
            x_len <- nrow(x)
        } else {
            x_len <- length(x)
        }
        if (i@object_len != x_len || i@allow_past_object_end > allow.append)
            stop("subscript is a NormalizedSubscript object that is ",
                 "incompatible\n  with the current subsetting operation")
        subscript(i)
    }
)

### Will succeed only 'i' is a logical-Rle with no NAs.
.try_to_make_IRanges_from_Rle_subscript <- function(i, x_len)
{
    i_vals <- runValue(i)
    if (!is.logical(i_vals))
        return(i)
    if (S4Vectors:::anyMissing(i_vals))
        stop("subscript contains NAs")
    if (length(i) < x_len)
        i <- rep(i, length.out=x_len)
    as(i, "IRanges")
}

setMethod("fromSubscriptToPositiveIntegers", "Rle",
    function(i, x, byrow=FALSE, exact=TRUE, allow.append=FALSE)
    {
        if (byrow) {
            x_len <- nrow(x)
        } else {
            x_len <- length(x)
        }
        i <- .try_to_make_IRanges_from_Rle_subscript(i, x_len)
        if (is(i, "Rle"))
            i <- decodeRle(i)
        callGeneric()
    }
)

setMethod("fromSubscriptToPositiveIntegers", "Ranges",
    function(i, x, byrow=FALSE, exact=TRUE, allow.append=FALSE)
    {
        i <- as.integer(i)
        callGeneric()
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### normalizeSingleBracketSubscript()
###

### Returns a NormalizedSubscript object with values in 'subscript' slot >= 1
### and <= N, where N = length(x) if 'byrow=FALSE' and N = nrow(x)
### if 'byrow=TRUE'.
normalizeSingleBracketSubscript <- function(i, x, byrow=FALSE, exact=TRUE,
                                                  allow.append=FALSE,
                                                  as.smart.subscript=FALSE)
{
    if (!isTRUEorFALSE(byrow))
        stop("'byrow' must be TRUE or FALSE")
    if (!isTRUEorFALSE(exact))
        stop("'exact' must be TRUE or FALSE")
    if (!isTRUEorFALSE(allow.append))
        stop("'allow.append' must be TRUE or FALSE")
    if (!isTRUEorFALSE(as.smart.subscript))
        stop("'as.smart.subscript' must be TRUE or FALSE")
    if (missing(i)) {
        i <- fromSubscriptToPositiveIntegers( , x, byrow=byrow,
                                                   exact=exact,
                                                   allow.append=allow.append)
        if (as.smart.subscript)
            i <- NormalizedSubscript(i, NROW(x), allow.append)
        return(i)
    }
    if (as.smart.subscript) {
        if (is(i, "NormalizedSubscript")) {
            if (i@object_len != NROW(x)
             || i@allow_past_object_end > allow.append)
                stop("subscript is a NormalizedSubscript object that is ",
                     "incompatible\n  with the current subsetting operation")
            return(i)
        }
        if (is(i, "SmartSubscript"))
            return(i)
    }
    i <- fromSubscriptToPositiveIntegers(i, x, byrow=byrow,
                                               exact=exact,
                                               allow.append=allow.append)
    if (as.smart.subscript)
        i <- NormalizedSubscript(i, NROW(x), allow.append)
    i
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### 3 internal generics to ease implementation of [ and [<- subsetting for
### Vector subclasses.
###
### A Vector subclass Foo should only need to implement an "extractROWS" and
### a "replaceROWS" method with signature to make "[" and "[<-" work
### out-of-the-box.
### For replaceROWS(), it's OK to assume that 'value' is "compatible" i.e.
### that it has gone thru normalizeSingleBracketReplacementValue().
### See "extractROWS" and "replaceROWS" methods for IRanges objects for an
### example.
###

setGeneric("extractROWS", signature="x",
    function(x, i) standardGeneric("extractROWS")
)

setMethod("extractROWS", "NULL", function(x, i) NULL)

setMethod("extractROWS", "vectorORfactor",
    function(x, i)
    {
        if (missing(i))
            return(x)
        if (is(i, "Rle"))
            i <- .try_to_make_IRanges_from_Rle_subscript(i, length(x))
        if (!is(i, "Ranges")) {
            i <- normalizeSingleBracketSubscript(i, x)
            return(x[i])
        }
        ## Which one is faster, vector_seqselect or vector_subsetByRanges?
        ans <- .Call2("vector_seqselect", x, start(i), width(i),
                      PACKAGE="IRanges")
        #ans <- .Call2("vector_subsetByRanges", x, start(i), width(i),
        #              PACKAGE="IRanges")
        if (is.factor(x))
            attributes(ans) <- list(levels=levels(x), class="factor")
        ans
    }
)

setGeneric("replaceROWS", signature="x",
    function(x, i, value) standardGeneric("replaceROWS")
)

setMethod("replaceROWS", "vectorORfactor",
    function(x, i, value)
    {
        i <- normalizeSingleBracketSubscript(i, x, allow.append=TRUE)
        x[i] <- value
        x
    }
)

### Dispatch on the 2nd argument!
setGeneric("normalizeSingleBracketReplacementValue", signature="x",
    function(value, x, i)
        standardGeneric("normalizeSingleBracketReplacementValue")
)

### Default method.
setMethod("normalizeSingleBracketReplacementValue", "ANY",
    function(value, x)
    {
        if (is(value, class(x)))
            return(value)
        lv <- length(value)
        value <- try(as(value, class(x)), silent=TRUE)
        if (inherits(value, "try-error"))
            stop("'value' must be a ", class(x), " object (or coercible ",
                 "to a ", class(x), " object)")
        if (length(value) != lv)
            stop("coercing replacement value to ", class(x), "\n",
                 "  changed its length!\n",
                 "  Please do the explicit coercion ",
                 "yourself with something like:\n",
                 "    x[...] <- as(value, \"", class(x), "\")\n",
                 "  but first make sure this coercion does what you want.")
        value
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### normalizeDoubleBracketSubscript()
###
### Supported types for 'i': single NA, numeric and character vectors only.
### Always returns a single integer. When called with 'error.if.nomatch=FALSE',
### returns an NA_integer_ if no match is found. Otherwise (the default),
### raises an error if no match is found so the returned integer is guaranteed
### to be a non-NA positive integer referring to a valid position in 'x'.
###

normalizeDoubleBracketSubscript <- function(i, x, exact=TRUE,
                                            error.if.nomatch=TRUE)
{
    if (!isTRUEorFALSE(exact))
        stop("'exact' must be TRUE or FALSE")
    if (!isTRUEorFALSE(error.if.nomatch))
        stop("'error.if.nomatch' must be TRUE or FALSE")
    if (missing(i))
        stop("subscript is missing")
    if (is.vector(i) && length(i) == 1L && is.na(i)) {
        if (error.if.nomatch)
            stop("subsetting by NA returns no match")
        return(NA_integer_)
    }
    if (!is.numeric(i) && !is.character(i))
        stop("invalid subscript type '", class(i), "'")
    if (length(i) < 1L)
        stop("attempt to extract less than one element")
    if (length(i) > 1L)
        stop("attempt to extract more than one element")
    if (is.numeric(i)) {
        if (!is.integer(i))
            i <- as.integer(i)
        if (i < 1L || length(x) < i)
            stop("subscript is out of bounds")
        return(i)
    }
    ## 'i' is a character string
    x_names <- names(x)
    if (is.null(x_names)) {
        if (error.if.nomatch)
            stop("attempt to extract by name when elements have no names")
        return(NA_integer_)
    }
    #if (i == "")
    #    stop("invalid subscript \"\"")
    if (exact) {
        ans <- match(i, x_names, incomparables=c(NA_character_, ""))
    } else {
        ## Because 'i' has length 1, it doesn't matter whether we use
        ## 'duplicates.ok=FALSE' (the default) or 'duplicates.ok=TRUE' but
        ## the latter seems to be just a little bit faster.
        ans <- pmatch(i, x_names, duplicates.ok=TRUE)
    }
    if (is.na(ans) && error.if.nomatch)
        stop("subscript \"", i, "\" matches no name")
    ans
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### 2 internal generics to ease implementation of [[ and [[<- subsetting for
### new List subclasses.
###

setGeneric("getListElement", signature="x",
    function(x, i, exact=TRUE) standardGeneric("getListElement")
)

setGeneric("setListElement", signature="x",
    function(x, i, value) standardGeneric("setListElement")
)

