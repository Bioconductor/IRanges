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
            ## Return a NativeNSBS object of length 0.
            i <- NULL
            return(callGeneric())
        }
        x_NROW <- NROW(x)
        if (min(start(i)) < 1L ||
            upperBoundIsStrict && max(end(i)) > x_NROW)
            S4Vectors:::.subscript_error("subscript contains out-of-bounds ",
                                         "ranges")
        if (i_len == 1L) {
            ans <- new("RangeNSBS", subscript=c(start(i), end(i)),
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
### "extractROWS" methods for subsetting *by* a Ranges object.
###

setMethod("extractROWS", c("vectorORfactor", "RangesNSBS"),
    function(x, i)
    {
        start <- start(i@subscript)
        width <- width(i@subscript)
        S4Vectors:::extract_ranges_from_vectorORfactor(x, start, width)
    }
)

setMethod("extractROWS", c("Rle", "RangesNSBS"),
    function(x, i)
    {
        start <- start(i@subscript)
        width <- width(i@subscript)
        ans <- S4Vectors:::extract_ranges_from_Rle(x, start, width)
        mcols(ans) <- extractROWS(mcols(x), i)
        ans
    }
)

