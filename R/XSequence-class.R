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

### Works as long as subseq() and as.numeric() work on 'x'.
### Not exported.
toNumSnippet <- function(x, max.width)
{
    if (length(x) <= 2L)
        return(paste(format(as.numeric(x)), collapse=" "))
    if (max.width < 0L)
        max.width <- 0L
    ## Elt width and nb of elt to display if they were all 0.
    elt_width0 <- 1L
    nelt_to_display0 <- min(length(x), (max.width+1L) %/% (elt_width0+1L))
    ## Effective elt width and nb of elt to display
    elt_width <- format.info(c(
                   as.numeric(subseq(x, end=nelt_to_display0 %/% 2L)),
                   as.numeric(subseq(x, start=-(nelt_to_display0 %/% 2L)))
                 ))[1]
    nelt_to_display <- min(length(x), (max.width+1L) %/% (elt_width+1L))
    if (nelt_to_display == length(x))
        return(paste(format(as.numeric(x), width=elt_width), collapse=" "))
    head <- format(as.numeric(subseq(x, end=(nelt_to_display+1L) %/% 2L)), width=elt_width)
    tail <- format(as.numeric(subseq(x, start=-(nelt_to_display %/% 2L))), width=elt_width)
    ans <- paste(paste(head, collapse=" "), "...", paste(tail, collapse=" "))
    if (nchar(ans) <= max.width || length(head) == 0L)
        return(ans)
    head <- head[-length(head)]
    paste(paste(head, collapse=" "), "...", paste(tail, collapse=" "))
}

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

