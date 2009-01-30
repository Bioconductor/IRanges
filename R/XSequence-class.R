### =========================================================================
### XSequence objects
### -------------------------------------------------------------------------
###
### The XSequence virtual class is a general container for storing an
### "external sequence".
###

setClass("XSequence",
    contains="Sequence",
    representation(
        "VIRTUAL",
        xdata="SequencePtr",  # an external pointer to the "seed" data
        offset="integer",     # a single integer
        length="integer"      # a single integer
    ),
    prototype(
        offset=0L,
        length=0L
    )
)

setMethod("length", "XSequence", function(x) x@length)

### Extracts a linear subsequence without copying the sequence data!
setMethod("subseq", "XSequence",
    function(x, start=NA, end=NA, width=NA)
    {
        solved_SEW <- try(solveUserSEW(length(x), start=start, end=end, width=width))
        if (is(solved_SEW, "try-error"))
            stop("Invalid sequence coordinates.\n",
                 "  Are you sure the supplied 'start', 'end' and 'width' arguments\n",
                 "  are defining a region that is within the limits of the sequence?")
        x@offset <- x@offset + start(solved_SEW) - 1L
        x@length <- width(solved_SEW)
        x
    }
)

