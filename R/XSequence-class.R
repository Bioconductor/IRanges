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
        solved_SEW <- solveSubseqSEW(length(x), start, end, width)
        x@offset <- x@offset + start(solved_SEW) - 1L
        x@length <- width(solved_SEW)
        x
    }
)

### Works as long as as.integer() works on 'x'.
setMethod("as.numeric", "XSequence",
    function(x, ...) as.numeric(as.integer(x))
)

setMethod("show", "XSequence",
    function(object)
    {
        lo <- length(object)
        cat("  ", class(object), " instance of length ", lo, "\n", sep="")
        if (lo != 0L)
            cat(" [1] ", toNumSnippet(object, getOption("width")-5), "\n", sep="")
        ## What is correct here? The documentation (?show) says that 'show'
        ## should return an invisible 'NULL' but, on the other hand, the 'show'
        ## method for intergers returns its 'object' argument...
        invisible(object)
    }
)

