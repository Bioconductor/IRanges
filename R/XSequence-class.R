### =========================================================================
### XSequence objects
### -------------------------------------------------------------------------
###
### The XSequence class is a general container for storing an "external
### sequence".
###

setClass("XSequence",
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

### Extracts a linear subsequence...
setGeneric("subseq", signature="x",
    function(x, start=NA, end=NA, width=NA) standardGeneric("subseq")
)

### ... without copying the sequence data for an XSequence object!
setMethod("subseq", "XSequence",
    function(x, start=NA, end=NA, width=NA)
    {
        limits <- new2("IRanges", start=1L, width=length(x), check=FALSE)
        limits <- narrow(limits, start=start, end=end, width=width)
        x@offset <- x@offset + start(limits) - 1L
        x@length <- width(limits)
        x
    }
)

setMethod("subseq", "vector",
    function(x, start=NA, end=NA, width=NA)
    {
        limits <- new2("IRanges", start=1L, width=length(x), check=FALSE)
        limits <- narrow(limits, start=start, end=end, width=width)
        x[start(limits) + seq_len(width(limits)) - 1L]
    }
)

setReplaceMethod("[", "XSequence",
    function(x, i, j,..., value)
        stop("attempt to modify the value of a ", class(x), " instance")
)

