### =========================================================================
### IRanges objects
### -------------------------------------------------------------------------
###
### The IRanges class is a simple container for storing a vector of integer
### ranges.
###

setClass("IRanges",
    contains="IPosRanges",
    representation(
        start="integer",
        width="integer",
        NAMES="character_OR_NULL"  # R doesn't like @names !!
    )
)

### A NormalIRanges object is an IRanges object where the ranges are:
###   (a) not empty (i.e. they have a non-null width);
###   (b) not overlapping;
###   (c) ordered from left to right;
###   (d) not even adjacent (i.e. there must be a non empty gap between 2
###       consecutive ranges).
### If 'x' is an IRanges object of length >= 2, then 'x' is normal iff:
###   start(x)[i] <= end(x)[i] < start(x)[i+1] <= end(x)[i+1]
### for every 1 <= i < length(x).
### If length(x) == 1, then 'x' is normal iff width(x)[1] >= 1.
### If length(x) == 0, then 'x' is normal.
setClass("NormalIRanges", contains="IRanges")


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### parallel_slot_names()
###

### Combine the new "parallel slots" with those of the parent class. Make
### sure to put the new parallel slots **first**. See R/Vector-class.R file
### in the S4Vectors package for what slots should or should not be considered
### "parallel".
setMethod("parallel_slot_names", "IRanges",
    function(x) c("start", "width", "NAMES", callNextMethod())
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Getters
###

setMethod("start", "IRanges", function(x, ...) x@start)

setMethod("width", "IRanges", function(x) x@width)

setMethod("names", "IRanges", function(x) x@NAMES)

setMethod("ranges", "IntegerRanges",
    function(x, use.names=TRUE, use.mcols=FALSE)
    {
        if (!isTRUEorFALSE(use.names))
            stop("'use.names' must be TRUE or FALSE")
        if (!isTRUEorFALSE(use.mcols))
            stop("'use.mcols' must be TRUE or FALSE")
        ans_start <- start(x)
        ans_width <- width(x)
        ans_names <- if (use.names) names(x) else NULL
        ans_mcols <- if (use.mcols) mcols(x, use.names=FALSE) else NULL
        new2("IRanges", start=ans_start,
                        width=ans_width,
                        NAMES=ans_names,
                        elementMetadata=ans_mcols,
                        check=FALSE)
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### isEmpty() and isNormal()
###

.isNormal_IRanges <- function(x)
    .Call2("C_isNormal_IRanges", x, PACKAGE="IRanges")

setMethod("isNormal", "IRanges", .isNormal_IRanges)

### Fast methods for NormalIRanges objects.
setMethod("isEmpty", "NormalIRanges", function(x) length(x) == 0L)
setMethod("isNormal", "NormalIRanges", function(x) TRUE)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "max" and "min" methods.
###
### Note: defined for NormalIRanges objects only.
### For an ordinary IRanges object 'x', it's not clear what the semantic
### should be. In particular, should empty ranges be ignored or not? If not
### then we could end up with 'min(x)' > 'max(x)' (e.g. when 'x' is made of 1
### empty range) which is not nice. Another (and more pragmatic) reason for
### not defining these methods for IRanges objects is that I don't need them
### at the moment.
###

setMethod("max", "NormalIRanges",
    function(x, ..., na.rm)
    {
        if (isEmpty(x)) {
            warning("empty ", class(x), " object; returning -Inf")
            -Inf
        } else {
            end(x)[length(x)]
        }
    }
)

setMethod("min", "NormalIRanges",
    function(x, ..., na.rm)
    {
        if (isEmpty(x)) {
            warning("empty ", class(x), " object; returning Inf")
            Inf
        } else {
            start(x)[1L]
        }
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Validity.
###
### Validity of IRanges objects is taken care of by the validity method for
### IPosRanges objects.
###

### NormalIRanges objects
.valid.NormalIRanges <- function(x)
{
    if (!.isNormal_IRanges(x))
        return("object is not normal")
    NULL
}

setValidity2("NormalIRanges", .valid.NormalIRanges)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion.
###

setAs("IntegerRanges", "IRanges",
    function(from) ranges(from, use.mcols=TRUE)
)

### Helper function (not exported) used by the "coerce" methods defined in
### IRanges-utils.R. Believe it or not but the implicit "coerce" methods do
### NOT check that they return a valid object!
newNormalIRangesFromIRanges <- function(x, check=TRUE)
{
    if (!is(x, "IRanges"))
        stop("'x' must be an IRanges object")
    if (!isTRUEorFALSE(check))
        stop("'check' must be TRUE or FALSE")
    ## Check only what needs to be checked.
    if (check) {
        msg <- .valid.NormalIRanges(x)
        if (!is.null(msg))
            stop(wmsg(msg))
    }
    class(x) <- "NormalIRanges"
    x
}

### The returned IRanges instance is guaranteed to be normal.
setAs("logical", "IRanges",
    function(from) as(as(from, "NormalIRanges"), "IRanges")
)

.from_logical_to_NormalIRanges <- function(from)
    .Call2("C_from_logical_to_NormalIRanges", from, PACKAGE="IRanges")

setAs("logical", "NormalIRanges", .from_logical_to_NormalIRanges)

### coercion from integer
.from_integer_to_IRanges <- function(from)
    .Call2("C_from_integer_to_IRanges", from, PACKAGE="IRanges")

setAs("integer", "IRanges", .from_integer_to_IRanges)

setAs("integer", "NormalIRanges",
    function(from) newNormalIRangesFromIRanges(as(from, "IRanges"))
)

setMethod("as.integer", "NormalIRanges", function(x) unlist_as_integer(x))

setAs("numeric", "IRanges", function(from) as(as.integer(from), "IRanges"))

setAs("numeric", "NormalIRanges", 
    function(from) newNormalIRangesFromIRanges(as(as.integer(from), "IRanges")))

### coercion from character
.from_character_to_IRanges <- function(from)
{
    stopifnot(is.character(from))
    if (anyNA(from))
        stop(wmsg("converting a character vector to an IRanges object ",
                  "does not support NAs"))
    error_msg <- wmsg(
        "The character vector to convert to an IRanges object must ",
        "contain strings of the form \"start-end\" or \"start..end\", ",
        "with end >= start - 1, or just \"pos\". For example: \"2501-2900\", ",
        "\"2501..2900\", or \"740\"."
    )
    ## We want to split on the first occurence of  "-" that is preceeded by
    ## a digit (ignoring and removing the spaces in between if any).
    from <- sub("([[:digit:]])[[:space:]]*-", "\\1..", from)
    split2 <- CharacterList(strsplit(from, "..", fixed=TRUE))
    split2_eltNROWS <- elementNROWS(split2)
    if (!all(split2_eltNROWS <= 2L))
        stop(error_msg)
    ans_start <- suppressWarnings(as.integer(heads(split2, n=1L)))
    ans_end <- suppressWarnings(as.integer(tails(split2, n=1L)))
    if (anyNA(ans_start) || anyNA(ans_end))
        stop(error_msg)
    IRanges(ans_start, ans_end, names=names(from))
}
setAs("character", "IRanges", .from_character_to_IRanges)

.from_factor_to_IRanges <- function(from)
{
    from <- setNames(as.character(from), names(from))
    .from_character_to_IRanges(from)
}
setAs("factor", "IRanges", .from_factor_to_IRanges)

setAs("ANY", "IPosRanges", function(from) as(from, "IRanges"))
setAs("ANY", "IntegerRanges", function(from) as(from, "IRanges"))


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Low-level setters for IRanges objects.
###
### All these low-level setters preserve the length of the object.
### The choice was made to implement a "resizing" semantic:
###   (1) changing the start preserves the end (so it changes the width)
###   (2) changing the end preserves the start (so it changes the width)
###   (3) changing the width preserves the start (so it changes the end)
###

.set_IRanges_start <- function(x, value, check=TRUE)
{
    if (!isTRUEorFALSE(check))
        stop("'check' must be TRUE or FALSE")
    ## Fix elementType slot on-the-fly.
    x <- updateObject(x, check=FALSE)
    old_start <- start(x)
    ## Use 'x@start[]' instead of 'x@start' so the right value is recycled.
    x@start[] <- S4Vectors:::numeric2integer(value)
    x@width <- width(x) - start(x) + old_start
    if (check)
        validObject(x)
    x
}

setReplaceMethod("start", "IRanges",
    function(x, ..., value) .set_IRanges_start(x, value)
)

.set_IRanges_end <- function(x, value, check=TRUE)
{
    if (!isTRUEorFALSE(check))
        stop("'check' must be TRUE or FALSE")
    ## Fix elementType slot on-the-fly.
    x <- updateObject(x, check=FALSE)
    ## Use 'x@width[]' instead of 'x@width' so the right value is recycled.
    x@width[] <- width(x) - end(x) + S4Vectors:::numeric2integer(value)
    if (check)
        validObject(x)
    x
}

setReplaceMethod("end", "IRanges",
    function(x, ..., value) .set_IRanges_end(x, value)
)

.set_IRanges_width <- function(x, value, check=TRUE)
{
    if (!isTRUEorFALSE(check))
        stop("'check' must be TRUE or FALSE")
    ## Fix elementType slot on-the-fly.
    x <- updateObject(x, check=FALSE)
    ## Use 'x@width[]' instead of 'x@width' so the right value is recycled.
    x@width[] <- S4Vectors:::numeric2integer(value)
    if (check)
        validObject(x)
    x
}

setReplaceMethod("width", "IRanges",
    function(x, ..., value) .set_IRanges_width(x, value)
)

set_IRanges_names <- function(x, value)
{
    ## Fix elementType slot on-the-fly.
    x <- updateObject(x, check=FALSE)
    x@NAMES <- S4Vectors:::normarg_names(value, class(x), length(x))
    ## No need to validate an IRanges object after setting its names so
    ## this should be safe.
    x
}

setReplaceMethod("names", "IRanges", set_IRanges_names)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Subsetting
###

setMethod("extractROWS", "NormalIRanges",
    function(x, i)
    {
        i <- normalizeSingleBracketSubscript(i, x, as.NSBS=TRUE)
        if (is(x, "NormalIRanges")) {
            if (!isStrictlySorted(i))
                stop("subscript must extract elements at strictly sorted ",
                     "positions when\n  subsetting a ", class(x), " object")
        }
        callNextMethod()
    }
)

setMethod("replaceROWS", "IRanges",
    function(x, i, value)
    {
        i <- normalizeSingleBracketSubscript(i, x, as.NSBS=TRUE)
        ans_start <- replaceROWS(start(x), i, start(value))
        ans_width <- replaceROWS(width(x), i, width(value))
        ans_mcols <- replaceROWS(mcols(x, use.names=FALSE), i,
                                 mcols(value, use.names=FALSE))
        BiocGenerics:::replaceSlots(x, start=ans_start,
                                       width=ans_width,
                                       mcols=ans_mcols,
                                       check=FALSE)
    }
)

setMethod("replaceROWS", "NormalIRanges",
    function(x, i, value)
    {
        ans <- callNextMethod()
        validObject(ans)
        ans
    }
)

