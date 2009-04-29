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
    function(x, start=NULL, end=NULL, width=NULL) standardGeneric("subseq")
)

## Replace a linear subsequence.
setGeneric("subseq<-", signature="x",
    function(x, start=NA, end=NA, width=NA, value) standardGeneric("subseq<-")
)

### Returns an IRanges instance of length 1 (when vectorized is FALSE)
solveSubseqSEW <- function(seq_length, start, end, width, vectorized = FALSE)
{
  if (is(start, "Ranges"))
    solved_SEW <- start
  else {
    sew <- list(start, end, width)
    sew_len <- sapply(sew, length)
    if (any(sew_len == 0) && all(sew_len == 0 | is.na(sew)))
      return(IRanges())
    seq_length <- rep(seq_length, length = max(sew_len))
    solved_SEW <- try(solveUserSEW(seq_length, start=start, end=end,
                                   width=width))
    if (is(solved_SEW, "try-error"))
      stop("Invalid sequence coordinates.\n  Please make sure the supplied",
           " 'start', 'end' and 'width' arguments\n",
           "  are defining a region that is within the limits of the sequence.")
  }
  if (!vectorized && length(solved_SEW) != 1)
    stop("number of ranges must equal 1")
  solved_SEW
}

setMethod("subseq", "vector",
    function(x, start=NA, end=NA, width=NA)
    {
        solved_SEW <- solveSubseqSEW(length(x), start, end, width, TRUE)
        .Call("vector_subseq", x, start(solved_SEW), width(solved_SEW),
              PACKAGE="IRanges")
    }
)

setMethod("subseq", "Sequence",
    function(x, start=NA, end=NA, width=NA)
    {
        solved_SEW <- solveSubseqSEW(length(x), start, end, width)
        x[mseq(start(solved_SEW), end(solved_SEW))]
    }
)

### Works as long as subseq() works on 'x' and "c" works on objects of the
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

