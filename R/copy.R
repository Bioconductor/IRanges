### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The copy() generic and its methods.
###

setGeneric("copy", function(x, ...) standardGeneric("copy"))

setMethod("copy", "SharedRaw",
    function(x, start=NA, end=NA, width=NA, lkup=NULL, reverse=FALSE)
    {
        solved_SEW <- solveUserSEW(length(x), start=start, end=end, width=width)
        if (!isTRUEorFALSE(reverse))
            stop("'reverse' must be TRUE or FALSE")
        ans_length <- width(solved_SEW)
        ans <- SharedRaw(ans_length)
        if (reverse)
            SharedVector.reverseCopy(ans, start(solved_SEW), end(solved_SEW),
                                     src=x, lkup=lkup)
        else
            SharedVector.copy(ans, start(solved_SEW), end(solved_SEW),
                              src=x, lkup=lkup)
        return(ans)
    }
)

setMethod("copy", "SharedVector_Pool",
    function(x, start=NA, end=NA, width=NA, lkup=NULL, reverse=FALSE)
    {
        ## FIXME: This limitation must be temporary only. We need copy()
        ## to work on a SharedVector_Pool object of arbitrary length!
        if (length(x) != 1L)
            stop("IRanges internal error: length(x) != 1")
        as(copy(x[[1L]], start=start, end=end, width=width,
                lkup=lkup, reverse=reverse),
           "SharedVector_Pool")
    }
)

