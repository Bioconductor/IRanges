### =========================================================================
### Sequence objects
### -------------------------------------------------------------------------
###
### The Sequence virtual class is a general container for storing a sequence
### i.e. an ordered set of elements.
###

setClass("Sequence", representation("VIRTUAL"))


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### subseq() and subseq<-().
###

### Extracts a linear subsequence.
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
    solved_SEW <- try(solveUserSEW(seq_length, start=start, end=end, width=width))
    if (is(solved_SEW, "try-error"))
        stop("Invalid sequence coordinates.\n",
             "  Please make sure the supplied 'start', 'end' and 'width' arguments\n",
             "  are defining a region that is within the limits of the sequence.")
    solved_SEW
}

setMethod("subseq", "Sequence",
    function(x, start=NA, end=NA, width=NA)
    {
        solved_SEW <- solveSubseqSEW(length(x), start, end, width)
        x[as.integer(solved_SEW)]
    }
)

setMethod("subseq", "vector",
    function(x, start=NA, end=NA, width=NA)
    {
        solved_SEW <- solveSubseqSEW(length(x), start, end, width)
        .Call("vector_subsetbyranges", x, start(solved_SEW), width(solved_SEW),
              PACKAGE="IRanges")
    }
)

### Works as long as subseq() works on 'x' and c() works on objects of the
### same class as 'x'.
setReplaceMethod("subseq", "ANY",
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


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### seqextract().
###
### Similar to msubseq() but the subsetting always operates at the top-level.
###

setGeneric("seqextract", signature="x",
    function(x, start=NULL, end=NULL, width=NULL) standardGeneric("seqextract")
)

setMethod("seqextract", "Sequence",
    function(x, start=NULL, end=NULL, width=NULL)
    {
        ir <- IRanges(start=start, end=end, width=width, names=NULL)
        if (any(start(ir) < 1L) || any(end(ir) > length(x)))
            stop("some ranges are out of bounds")
        x[as.integer(ir)]
    }
)

setMethod("seqextract", "vector",
    function(x, start=NULL, end=NULL, width=NULL)
    {
        ir <- IRanges(start=start, end=end, width=width, names=NULL)
        .Call("vector_subsetbyranges", x, start(ir), width(ir), PACKAGE="IRanges")
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Other methods.
###

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

