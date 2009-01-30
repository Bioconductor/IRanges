### =========================================================================
### Sequence objects
### -------------------------------------------------------------------------
###
### The Sequence virtual class is a general container for storing a sequence
### i.e. an ordered set of elements.
###

setClass("Sequence", representation("VIRTUAL"))

### Extracts a linear subsequence.
setGeneric("subseq", signature="x",
    function(x, start=NA, end=NA, width=NA) standardGeneric("subseq")
)

setMethod("subseq", "vector",
    function(x, start=NA, end=NA, width=NA)
    {
        if (!missing(end)) {
            if (missing(start))
                start <- end - width + 1L
            else if (missing(width))
                width <- end - start + 1L
        }
        .Call("vector_subseq", x, as.integer(start), as.integer(width))
    }
)

setMethod("subseq", "Sequence",
    function(x, start=NA, end=NA, width=NA)
    {
        solved_SEW <- try(solveUserSEW(length(x), start=start, end=end, width=width))
        if (is(solved_SEW, "try-error"))
            stop("Invalid sequence coordinates.\n",
                 "  Are you sure the supplied 'start', 'end' and 'width' arguments\n",
                 "  are defining a region that is within the limits of the sequence?")
        x[start(solved_SEW) + seq_len(width(solved_SEW)) - 1L]
    }
)

### The only reason for defining the replacement version of the "[" operator
### is to let the user know that he can't use it:
###   x <- BString("AbnbIU")
###   x[2] <- "Z" # provokes an error
### If we don't define it, then the user can type the above and believe that
### it actually did something but it didn't.
setReplaceMethod("[", "Sequence",
    function(x, i, j,..., value)
        stop("attempt to modify the value of a ", class(x), " instance")
)

setMethod("rep", "Sequence",
    function(x, times)
        x[rep.int(seq_len(length(x)), times)]
)

### Maybe this is how `!=` should have been defined in the base package so
### nobody would ever need to bother implementing such an obvious thing.
setMethod("!=", signature(e1="Sequence", e2="Sequence"),
    function(e1, e2) !(e1 == e2)
)

