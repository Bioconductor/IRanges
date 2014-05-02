### =========================================================================
### Subsetting utility functions
### -------------------------------------------------------------------------


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### RleNSBS objects.
###

setClass("RleNSBS",  # not exported
    contains="NSBS",
    representation(
        subscript="Rle"
    )
    #prototype(
    #    subscript=Rle(integer(0))
    #)
)

### Construction methods.
### Supplied arguments are trusted so we don't check them!

setMethod("NSBS", "Rle",
    function(i, x, exact=TRUE, upperBoundIsStrict=TRUE)
    {
        x_NROW <- NROW(x)
        i_vals <- runValue(i)
        if (is.logical(i_vals) && length(i_vals) != 0L) {
            if (S4Vectors:::anyMissing(i_vals))
                stop("subscript contains NAs")
            if (length(i) < x_NROW)
                i <- rep(i, length.out=x_NROW)
            i <- as(i, "NormalIRanges")
            return(callGeneric())
        }
        i_vals <- as.integer(NSBS(i_vals, x,
                                  exact=exact,
                                  upperBoundIsStrict=upperBoundIsStrict))
        runValue(i) <- i_vals
        new("RleNSBS", subscript=i,
                       upper_bound=x_NROW,
                       upper_bound_is_strict=upperBoundIsStrict)
    }
)

### Other methods.

setMethod("as.integer", "RleNSBS", function(x) decodeRle(x@subscript))

setMethod("length", "RleNSBS", function(x) length(x@subscript))

setMethod("anyDuplicated", "RleNSBS",
    function(x, incomparables=FALSE, ...) anyDuplicated(x@subscript)
)

setMethod("isStrictlySorted", "RleNSBS",
    function(x) isStrictlySorted(x@subscript)
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### RangesNSBS objects.
###

setClass("RangesNSBS",  # not exported
    contains="NSBS",
    representation(
        subscript="Ranges"
    )
    #prototype(
    #    subscript=IRanges()
    #)
)

### Construction methods.
### Supplied arguments are trusted so we don't check them!

setMethod("NSBS", "Ranges",
    function(i, x, exact=TRUE, upperBoundIsStrict=TRUE)
    {
        if (length(i) == 0L) {
            i <- NULL
            return(callGeneric())
        }
        x_NROW <- NROW(x)
        if (min(start(i)) < 1L ||
            upperBoundIsStrict && max(end(i)) > x_NROW)
            stop("subscript contains out-of-bounds ranges")
        new("RangesNSBS", subscript=i,
                          upper_bound=x_NROW,
                          upper_bound_is_strict=upperBoundIsStrict)
    }
)

### Other methods.

setMethod("as.integer", "RangesNSBS", function(x) as.integer(x@subscript))

setMethod("length", "RangesNSBS", function(x) sum(width(x@subscript)))

setMethod("anyDuplicated", "RangesNSBS",
    function(x, incomparables=FALSE, ...) !isDisjoint(x@subscript)
)

setMethod("isStrictlySorted", "RangesNSBS", function(x) isNormal(x@subscript))


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### "extractROWS" methods for vectorORfactor objects.
###

setMethod("extractROWS", c("vectorORfactor", "Rle"),
    function(x, i)
    {
        i <- normalizeSingleBracketSubscript(i, x, as.NSBS=TRUE)
        callGeneric()
    }
)
setMethod("extractROWS", c("vectorORfactor", "RangesNSBS"),
    function(x, i)
    {
        i <- i@subscript
        callGeneric()
    }
)
setMethod("extractROWS", c("vectorORfactor", "Ranges"),
    function(x, i)
    {
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

setMethod("extractROWS", c("matrix", "Ranges"),
    S4Vectors:::extractROWSWithBracket
)

