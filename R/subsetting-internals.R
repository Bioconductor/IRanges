### =========================================================================
### Subsetting utility functions
### -------------------------------------------------------------------------


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
        i_len <- length(i)
        if (i_len == 0L) {
            i <- NULL
            return(callGeneric())
        }
        x_NROW <- NROW(x)
        if (min(start(i)) < 1L ||
            upperBoundIsStrict && max(end(i)) > x_NROW)
            stop("subscript contains out-of-bounds ranges")
        if (i_len == 1L) {
            ans <- new("WindowNSBS", subscript=c(start(i), end(i)),
                                     upper_bound=x_NROW,
                                     upper_bound_is_strict=upperBoundIsStrict)
            return(ans)
        }
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
                      PACKAGE="S4Vectors")
        #ans <- .Call2("vector_subsetByRanges", x, start(i), width(i),
        #              PACKAGE="S4Vectors")
        if (is.factor(x))
            attributes(ans) <- list(levels=levels(x), class="factor")
        ans
    }
)

setMethod("extractROWS", c("matrix", "Ranges"),
    S4Vectors:::extractROWSWithBracket
)

