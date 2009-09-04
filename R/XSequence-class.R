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
setGeneric("subseq", signature="x",
        function(x, start=NA, end=NA, width=NA) standardGeneric("subseq")
)

## Replace a linear subsequence.
setGeneric("subseq<-", signature="x",
        function(x, start=NA, end=NA, width=NA, value) standardGeneric("subseq<-")
)

### Returns an IRanges instance of length 1.
### Not exported.
solveSubseqSEW <- function(seq_length, start, end, width)
{
    solved_SEW <-
      try(solveUserSEW(seq_length, start=start, end=end, width=width),
          silent = TRUE)
    if (inherits(solved_SEW, "try-error"))
        stop("Invalid sequence coordinates.\n",
             "  Please make sure the supplied 'start', 'end' and 'width' arguments\n",
             "  are defining a region that is within the limits of the sequence.")
    solved_SEW
}

setMethod("subseq", "XSequence",
    function(x, start=NA, end=NA, width=NA)
    {
        solved_SEW <- solveSubseqSEW(length(x), start, end, width)
        x@offset <- x@offset + start(solved_SEW) - 1L
        x@length <- width(solved_SEW)
        x
    }
)

setReplaceMethod("subseq", "XSequence",
    function(x, start=NA, end=NA, width=NA, value)
    {
        solved_SEW <- solveSubseqSEW(length(x), start, end, width)
        if (!is.null(value)) {
            if (!is(value, class(x)))
                stop("'value' must be a ", class(x), " object or NULL")
        }
        c(subseq(x, end=start(solved_SEW)-1L),
          value,
          subseq(x, start=end(solved_SEW)+1L))
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### msubseq() and msubseq<-().
###


### Works as long as c() works on objects of the same class as 'x'.
setMethod("seqselect", "XSequence",
    function(x, start=NULL, end=NULL, width=NULL)
    {
        xv <- Views(x, start=start, end=end, width=width)
        ## TODO: Implement a fast "unlist" method for Views objects.
        do.call(c, as.list(xv))  # i.e. 'unlist(xv)'
    }
)

setMethod("window", "XSequence",
        function(x, start = NULL, end = NULL, width = NULL,
                frequency = NULL, delta = NULL, ...)
        {
            if (is.null(frequency) && is.null(delta)) {
                subseq(x,
                       start = ifelse(is.null(start), NA, start),
                       end = ifelse(is.null(end), NA, end),
                       width = ifelse(is.null(width), NA, width))
            } else {
                if (!is.null(width)) {
                    if (is.null(start))
                        start <- end - width + 1L
                    else if (is.null(end))
                        end <- start + width - 1L
                }
                idx <-
                  stats:::window.default(seq_len(length(x)), start = start, end = end,
                                         frequency = frequency, deltat = delta, ...)
                attributes(idx) <- NULL
                x[idx]
            }
        })

### Works as long as as.integer() works on 'x'.
setMethod("as.numeric", "XSequence",
    function(x, ...) as.numeric(as.integer(x))
)

setMethod("show", "XSequence",
    function(object)
    {
        lo <- length(object)
        cat("  A ", class(object), " instance of length ", lo, "\n", sep="")
        if (lo != 0L)
            cat(" [1] ", toNumSnippet(object, getOption("width")-5), "\n", sep="")
        ## What is correct here? The documentation (?show) says that 'show'
        ## should return an invisible 'NULL' but, on the other hand, the 'show'
        ## method for intergers returns its 'object' argument...
        invisible(object)
    }
)

