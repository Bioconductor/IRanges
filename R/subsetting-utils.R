### =========================================================================
### Subsetting utility functions
### -------------------------------------------------------------------------


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### RangesNSBS objects.
###

setClass("RangesNSBS",  # not exported
    contains="NSBS",
    representation(
        subscript="IRanges"
    )
)

### Construction methods.
### Supplied arguments are trusted so we don't check them!

setMethod("NSBS", "IntegerRanges",
    function(i, x, exact=TRUE, strict.upper.bound=TRUE, allow.NAs=FALSE)
    {
        i_len <- length(i)
        if (i_len == 0L) {
            ## Return a NativeNSBS object of length 0.
            i <- NULL
            return(callGeneric())
        }
        x_NROW <- NROW(x)
        if (is(i, "IPos"))
            i <- i@pos_runs  # TODO: Use collapse() when it's available
        i_start <- start(i)
        i_end <- end(i)
        if (min(i_start) < 1L || strict.upper.bound && max(i_end) > x_NROW)
            S4Vectors:::.subscript_error("subscript contains out-of-bounds ",
                                         "ranges")
        if (i_len > 1L) {
            ans <- new2("RangesNSBS", subscript=i,
                                      upper_bound=x_NROW,
                                      upper_bound_is_strict=strict.upper.bound,
                                      check=FALSE)
            return(ans)
        }
        if (i_end > i_start) {
            ans <- new2("RangeNSBS", subscript=c(i_start, i_end),
                                     upper_bound=x_NROW,
                                     upper_bound_is_strict=strict.upper.bound,
                                     check=FALSE)
            return(ans)
        }
        ## Return a NativeNSBS object of length <= 1.
        if (i_end == i_start) {
            i <- i_start
        } else {
            i <- NULL
        }
        callGeneric()
    }
)

### Other methods.

setMethod("as.integer", "RangesNSBS",
    function(x) unlist_as_integer(x@subscript)
)

setMethod("length", "RangesNSBS", function(x) sum(width(x@subscript)))

setMethod("anyDuplicated", "RangesNSBS",
    function(x, incomparables=FALSE, ...) !isDisjoint(x@subscript)
)

setMethod("isStrictlySorted", "RangesNSBS", function(x) isNormal(x@subscript))


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### "extractROWS" methods for subsetting *by* an IntegerRanges object.
###

setMethod("extractROWS", c("vector_OR_factor", "RangesNSBS"),
    function(x, i)
    {
        start <- start(i@subscript)
        width <- width(i@subscript)
        S4Vectors:::extract_ranges_from_vector_OR_factor(x, start, width)
    }
)

setMethod("extractROWS", c("array", "RangesNSBS"),
    S4Vectors:::default_extractROWS
)
setMethod("extractROWS", c("data.frame", "RangesNSBS"),
    S4Vectors:::default_extractROWS
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

