### =========================================================================
### Subsetting utility functions
### -------------------------------------------------------------------------


### Returns an integer vector with values >= 1 and <= NROW(x).
normalizeSingleBracketSubscript <- function(i, x)
{
    lx <- NROW(x)
    if (missing(i))
        return(seq_len(lx))
    if (is.null(i))
        return(integer(0))
    if (is(i, "Rle"))
        i <- as.vector(i)
    if (!is.atomic(i))
        stop("invalid subscript type")
    if (is.numeric(i)) {
        if (!is.integer(i))
            i <- as.integer(i)
        if (anyMissingOrOutside(i, upper=lx))
            stop("subscript contains NAs or out of bounds indices")
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
            return(seq_along(x)[i])
        return(i)
    }
    if (is.logical(i)) {
        if (anyMissing(i))
            stop("subscript contains NAs")
        li <- length(i)
        if (li > lx) {
            if (any(i[(lx+1L):li]))
                stop("subscript is a logical vector with out of bounds ",
                     "TRUE values")
            i <- i[seq_len(lx)]
        }
        if (li < lx)
            i <- rep(i, length.out=lx)
        return(which(i))
    }
    if (is.character(i) || is.factor(i)) {
        x_names <- names(x)
        if (is.null(x_names))
            stop("cannot subset by character when names are NULL")
        i <- match(i, x_names, incomparables=c(NA_character_, ""))
        if (anyMissing(i))
            stop("subscript contains invalid names")
        return(i)
    }
    stop("invalid subscript type")
}

### Supported types for 'i': single NA, numeric and character vectors only.
### Always returns a single integer. When called with 'error.if.nomatch=FALSE',
### returns an NA_integer_ if no match is found. Otherwise (the default),
### raises an error if no match is found so the returned integer is guaranteed
### to be a non-NA positive integer referring to a valid position in 'x'.
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
            stop("subscript out of bounds")
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
        ans <- match(i, x_names)
    } else {
        ## Because 'i' has length 1, it doesn't matter whether we use
        ## 'duplicates.ok=FALSE' (the default) or 'duplicates.ok=TRUE' but
        ## the latter seems just a little bit faster.
        ans <- pmatch(i, x_names, duplicates.ok=TRUE)
    }
    if (is.na(ans) && error.if.nomatch)
        stop("subscript \"", i, "\" matches no name")
    ans
}

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
### 2 internal generics to ease implementation of [ and [<- subsetting for
### new Vector subclasses.
###
### Most new Vector subclasses should only need to implement an "extractROWS"
### and a "replaceROWS" method to have "[" and "[<-" work out-of-the-box,
### respectively.
### Must support the following 'i' types: missing, Ranges and anything that
### can be handled by normalizeSingleBracketSubscript().
### For replaceROWS(), it's OK to assume that 'value' is "compatible" i.e.
### that it has gone thru normalizeSingleBracketReplacementValue().
### See "extractROWS" and "replaceROWS" methods for IRanges objects for an
### example.
###

setGeneric("extractROWS", signature="x",
    function(x, i) standardGeneric("extractROWS")
)

setGeneric("replaceROWS", signature="x",
    function(x, i, value) standardGeneric("replaceROWS")
)


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


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### TODO: .bracket.Index() needs to go away
###

## 'allowAppend' allows new elements in 'idx' for appending. Ideally,
## all list-like sequences should allow appending through [<-, but
## this is only supported by DataFrame for now. 
.bracket.Index <-
function(idx, lx, nms = NULL, dup.nms = FALSE, asRanges = FALSE,
         allowAppend = FALSE)
{
    msg <- NULL
    newNames <- character(0)
    if (is(idx, "Rle") && is.integer(runValue(idx)))
      idx <- as.integer(idx)
    if (is.numeric(idx)) {
        if (!is.integer(idx))
            idx <- as.integer(idx)
        if (anyMissingOrOutside(idx, upper = if (!allowAppend) lx
                                             else .Machine$integer.max))
        {
            msg <- "subscript contains NAs or out of bounds indices"
        } else {
            anyPos <- anyMissingOrOutside(idx, upper = 0L)
            anyNeg <- anyMissingOrOutside(idx, 0L)
            if (anyPos && anyNeg)
                msg <- "negative and positive indices cannot be mixed"
        }
    } else if (is.logical(idx)) {
        if (anyMissing(idx))
            msg <- "subscript contains NAs"
        else if (!allowAppend && length(idx) > lx)
            msg <- "subscript out of bounds"
    } else if (is.character(idx) || is.factor(idx)) {
        if (anyMissing(idx))
            msg <- "subscript contains NAs"
        else if (!allowAppend && is.null(nms) && length(idx) > 0)
            msg <- "cannot subset by character when names are NULL"
        else if (!allowAppend) {
            if (dup.nms)
                m <- pmatch(idx, nms, duplicates.ok = TRUE)
            else
                m <- match(idx, nms)
            if (!dup.nms && anyMissing(m))
                msg <- "mismatching names"
        }
    } else if (is(idx, "Rle")) {
        if (anyMissing(runValue(idx)))
            msg <- "subscript contains NAs"
        else if (!allowAppend && length(idx) > lx)
            msg <- "subscript out of bounds"
    } else if (is(idx, "Ranges")) {
        rng <- range(idx)
        if ((length(rng) > 0) && (start(rng) < 1 || end(rng) > lx))
            stop("range index out of bounds")
        else if (anyMissingOrOutside(width(idx), 1L)) {
            idx <- idx[width(idx) > 0L]
        }
    } else if (!is.null(idx)) {
        msg <- "invalid subscript type"
    }
    if (!is.null(msg)) {
        useIdx <- NULL
        idx <- NULL
    } else {
        useIdx <- TRUE
        if (asRanges) {
            if (length(idx) == 0) {
                idx <- IRanges()
            } else if (is.character(idx)) {
                if (allowAppend) {
                    m <- match(idx, nms)
                    nam <- is.na(m)
                    m[nam] <- lx + seq(sum(nam))
                    newNames <- idx[nam]
                    idx <- as(m, "IRanges")
                } else idx <-
                    as(pmatch(idx, nms, duplicates.ok = TRUE), "IRanges")
            } else if (is.logical(idx)) {
                if (all(idx)) {
                    useIdx <- FALSE
                } else {
                    if (length(idx) < lx)
                        idx <- rep(idx, length.out = lx)
                    idx <- as(idx, "NormalIRanges")
                }
            } else if (is.integer(idx)) {
                if (anyNeg)
                    idx <- seq_len(lx)[idx]
                idx <- as(idx, "IRanges")
            } else if (is(idx, "Rle")) {
                if (all(runValue(idx))) {
                    useIdx <- FALSE
                } else {
                    if (length(idx) < lx)
                        idx <- rep(idx, length.out = lx)
                    idx <- as(idx, "NormalIRanges")
                }
            }
            if (length(idx) == 1 && start(idx) == 1 && end(idx) == lx)
                useIdx <- FALSE
        } else {
            if (length(idx) == 0) {
                idx <- integer()
            } else if (is.character(idx)) {
                if (allowAppend) {
                    m <- match(idx, nms)
                    nam <- is.na(m)
                    newNames <- idx[nam]
                    m[nam] <- lx + seq_len(sum(nam))
                    idx <- m
                } else idx <- pmatch(idx, nms, duplicates.ok = TRUE)
            } else if (is.logical(idx)) {
                if (all(idx)) {
                    useIdx <- FALSE
                } else {
                    if (length(idx) < lx)
                        idx <- rep(idx, length.out = lx)
                    idx <- which(idx)
                }
            } else if (is.integer(idx) && anyNeg) {
                idx <- seq_len(lx)[idx]
            } else if (is(idx, "Rle")) {
                if (all(runValue(idx))) {
                    useIdx <- FALSE
                } else {
                    if (length(idx) < lx)
                        idx <- rep(idx, length.out = lx)
                    idx <- which(idx)
                }
            } else if (is(idx, "Ranges")) {
                if (length(idx) == 1 && start(idx) == 1 && end(idx) == lx)
                    useIdx <- FALSE
                else
                    idx <- as.integer(idx)
            }
        }
    }
    list(msg = msg, useIdx = useIdx, idx = idx, newNames = newNames)
}

