### =========================================================================
### GappedRanges objects
### -------------------------------------------------------------------------
###
### A GappedRanges object is a vector of gapped ranges.
### A gapped range is conceptually the union of 1 or more non-overlapping
### ranges ordered from left to right.
### More precisely, a gapped range can be represented by a normal IRanges
### object of length >= 1. In particular normality here ensures that the
### individual ranges are non-empty and are separated by non-empty gaps.
### The start of a gapped range is the start of its first range.
### The end of a gapped range is the end of its last range.
### If we ignore the gaps, then a GappedRanges object can be seen as a Ranges
### object.
###


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Some experimental stuff that didn't work very well.
###
### TODO: It could be worth pursuing this so it could be reused in the
### context of the MaskCollection class rework...
###

if (FALSE) {

  setClass("NormalIRangesList",
    contains="IRangesList",
    representation("VIRTUAL"),
    prototype(elementType="NormalIRanges")
  )

  setClass("CompressedNormalIRangesList",
    contains=c("NormalIRangesList", "CompressedIRangesList")
  )

  setAs("CompressedIRangesList", "NormalIRangesList",
    function(from)
    {
        ans <- new("CompressedNormalIRangesList",
                   elementMetadata=from@elementMetadata,
                   elementType="IRanges",  # we need to cheat
                   metadata=from@metadata,
                   partitioning=from@partitioning,
                   unlistData=from@unlistData)
        ans@elementType = "NormalIRanges"
        ans
    }
  )

  ### Pb with this approach: the coercion method above generates invalid
  ### objects! This is because the the unlistData slot of a CompressedList
  ### object is expected to belong to the class (or to s subclass of the
  ### class) specified in the elementType slot. This is not the case here:
  ### it will of class IRanges, not NormalIRanges.

  ### Class definition 1: extending CompressedNormalIRangesList:
  setClass("GappedRanges", contains("CompressedNormalIRangesList", "Ranges"))

  ### Class definition 2: with a CompressedNormalIRangesList slot:
  setClass("GappedRanges",
    contains="Ranges",
    representation(
        cnirl="CompressedNormalIRangesList"
    ),
    prototype(
        elementType="NormalIRanges"
    )
  )

}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The real class definition.
###

setClass("GappedRanges",
    contains="Ranges",
    representation(cirl="CompressedIRangesList"),
    prototype(elementType="NormalIRanges")
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Accessor-like methods.
###

setMethod("length", "GappedRanges", function(x) length(x@cirl))

setMethod("start", "GappedRanges",
    function(x, ...)
        #sapply(start(unname(x@cirl)), function(s) s[1L])
        .Call("GappedRanges_start", x, PACKAGE="IRanges")
)

setMethod("end", "GappedRanges",
    function(x, ...)
        #sapply(end(unname(x@cirl)), function(e) e[length(e)])
        .Call("GappedRanges_end", x, PACKAGE="IRanges")
)

setMethod("names", "GappedRanges", function(x) names(x@cirl))

setReplaceMethod("names", "GappedRanges",
    function(x, value)
    {
        names(x@cirl) <- value
        x
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Validity.
###

### IMPLEMENT ME!
.valid.GappedRanges <- function(x)
{
    NULL
}

setValidity2("GappedRanges", .valid.GappedRanges)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion.
###

setAs("CompressedIRangesList", "GappedRanges",
    function(from) new("GappedRanges", cirl=from)
)

setAs("GappedRanges", "CompressedIRangesList", function(from) from@cirl)
setAs("GappedRanges", "IRangesList", function(from) from@cirl)
setAs("GappedRanges", "RangesList", function(from) from@cirl)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Subsetting.
###

setMethod("[[", "GappedRanges",
    function(x, i, j, ..., exact=TRUE)
    {
        i <- checkAndTranslateDbleBracketSubscript(x, i)
        newNormalIRangesFromIRanges(x@cirl[[i]], check=FALSE)
    }
)

### Without this definition, we inherit the method for Sequence objects
### which returns the same thing but is thousands of times slower!
setMethod("elementLengths", "GappedRanges",
    function(x) elementLengths(x@cirl)
)

setMethod("[", "GappedRanges",
    function(x, i, j, ... , drop=TRUE)
    {
        x@cirl <- x@cirl[i]
        x
    }
)

setMethod("seqselect", "GappedRanges",
    function(x, start=NULL, end=NULL, width=NULL)
    {
        x@cirl <- seqselect(x@cirl, start=start, end=end, width=width)
        x
    }
)

setMethod("window", "GappedRanges",
    function(x, start=NA, end=NA, width=NA, frequency=NULL, delta=NULL, ...)
    {
        x@cirl <- window(x@cirl, start=start, end=end, width=width,
                         frequency=frequency, delta=delta, ...)
        x
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "narrow" endomorphism.
###

### FIXME: This is a quick and dirty implementation that is TOTALLY
### inefficient. It needs to be improved a lot!
### FIXME: It's also broken because it can return a GappedRanges object with
### empty elements (not allowed).
setMethod("narrow", "GappedRanges",
    function(x, start=NA, end=NA, width=NA, use.names=TRUE)
    {
        solved_SEW <- solveUserSEW(width(x), start=start, end=end, width=width)
        start2 <- start(x) + start(solved_SEW) - 1L
        end2 <- start2 + width(solved_SEW) - 1L

        for (i in seq_len(length(x))) {
            x@cirl[[i]] <- restrict(x[[i]], start=start2[i], end=end2[i])
        }
        if (!normargUseNames(use.names))
            names(x) <- NULL
        x
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Combining.
###

setMethod("c", "GappedRanges",
    function(x, ..., recursive=FALSE)
    {
        if (!identical(recursive, FALSE))
            stop("'recursive' argument not supported")
        if (missing(x)) {
            args <- unname(list(...))
            x <- args[[1L]]
        } else {
            args <- unname(list(x, ...))
        }
        if (length(args) == 1L)
            return(x)
        arg_is_null <- sapply(args, is.null)
        if (any(arg_is_null))
            args[arg_is_null] <- NULL  # remove NULL elements by setting them to NULL!
        if (!all(sapply(args, is, class(x))))
            stop("all arguments in '...' must be ", class(x), " objects (or NULLs)")
        x@cirl <- do.call(c, lapply(args, function(xx) xx@cirl))
        x
    }
)

