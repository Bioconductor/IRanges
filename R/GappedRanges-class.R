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

setClass("GappedRanges",
    contains="Ranges",
    representation(cnirl="CompressedNormalIRangesList"),
    prototype(elementType="NormalIRanges")
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Accessor-like methods.
###

setMethod("length", "GappedRanges", function(x) length(x@cnirl))

setMethod("start", "GappedRanges",
    function(x, ...) CompressedNormalIRangesList.min(x@cnirl, FALSE)
)

setMethod("end", "GappedRanges",
    function(x, ...) CompressedNormalIRangesList.max(x@cnirl, FALSE)
)

setGeneric("ngap", function(x) standardGeneric("ngap"))
setMethod("ngap", "GappedRanges", function(x) {elementLengths(x) - 1L})

setMethod("names", "GappedRanges", function(x) names(x@cnirl))

setReplaceMethod("names", "GappedRanges",
    function(x, value)
    {
        names(x@cnirl) <- value
        x
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Validity.
###

.valid.GappedRanges <- function(x)
    .Call2("valid_GappedRanges", x, 0L, PACKAGE="IRanges")

setValidity2("GappedRanges", .valid.GappedRanges)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion.
###

setAs("CompressedNormalIRangesList", "GappedRanges",
    function(from) new("GappedRanges", cnirl=from)
)
setAs("CompressedIRangesList", "GappedRanges",
    function(from) as(as(from, "CompressedNormalIRangesList"), "GappedRanges")
)

setAs("GappedRanges", "CompressedNormalIRangesList", function(from) from@cnirl)
setAs("GappedRanges", "NormalIRangesList", function(from) from@cnirl)
setAs("GappedRanges", "CompressedIRangesList", function(from) from@cnirl)
setAs("GappedRanges", "IRangesList", function(from) from@cnirl)
setAs("GappedRanges", "RangesList", function(from) from@cnirl)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "show" method.
###

setMethod("as.data.frame", "GappedRanges",
    function(x, row.names=NULL, optional=FALSE, ...)
    {
        ans <- callNextMethod(unname(x), row.names, optional, ...)
        ans$ngap <- ngap(x)
        ans$names <- names(x)
        return(ans)
    }
)

setMethod("show", "GappedRanges",
    function(object)
    {
        lo <- length(object)
        cat(class(object), " of length ", lo, "\n", sep="")
        if (lo == 0L) {
            return(NULL)
        } else if (lo < 20L) {
            showme <-
              as.data.frame(object,
                            row.names=paste("[", seq_len(lo), "]", sep=""))
        } else {
            sketch <- function(x)
              c(window(x, 1L, 9L), "...", window(x, length(x)-8L, length(x)))
            showme <-
              data.frame(start=sketch(start(object)),
                         end=sketch(end(object)),
                         width=sketch(width(object)),
                         ngap=sketch(ngap(object)),
                         row.names=c(paste("[", 1:9, "]", sep=""), "...",
                                     paste("[", (lo-8L):lo, "]", sep="")),
                         check.rows=TRUE,
                         check.names=FALSE,
                         stringsAsFactors=FALSE)
            NAMES <- names(object)
            if (!is.null(NAMES))
                showme$names <- sketch(NAMES)
        }
        show(showme)
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Subsetting.
###

### WARNING: We override the *semantic* of the "[[" method for Ranges objects.
setMethod("[[", "GappedRanges",
    function(x, i, j, ..., exact=TRUE)
    {
        i <- checkAndTranslateDbleBracketSubscript(x, i)
        newNormalIRangesFromIRanges(x@cnirl[[i]], check=FALSE)
    }
)

### WARNING: We override the *semantic* of the "elementLengths" method for
### Ranges objects.
setMethod("elementLengths", "GappedRanges",
    function(x) elementLengths(x@cnirl)
)

setMethod("[", "GappedRanges",
    function(x, i, j, ... , drop=TRUE)
    {
        x@cnirl <- x@cnirl[i]
        x
    }
)

setMethod("seqselect", "GappedRanges",
    function(x, start=NULL, end=NULL, width=NULL)
    {
        x@cnirl <- seqselect(x@cnirl, start=start, end=end, width=width)
        x
    }
)

setMethod("window", "GappedRanges",
    function(x, start=NA, end=NA, width=NA, frequency=NULL, delta=NULL, ...)
    {
        x@cnirl <- window(x@cnirl, start=start, end=end, width=width,
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
#setMethod("narrow", "GappedRanges",
#    function(x, start=NA, end=NA, width=NA, use.names=TRUE)
#    {
#        solved_SEW <- solveUserSEW(width(x), start=start, end=end, width=width)
#        start2 <- start(x) + start(solved_SEW) - 1L
#        end2 <- start2 + width(solved_SEW) - 1L

#        for (i in seq_len(length(x))) {
#            x@cnirl[[i]] <- restrict(x[[i]], start=start2[i], end=end2[i])
#        }
#        if (!normargUseNames(use.names))
#            names(x) <- NULL
#        x
#    }
#)


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
        x@cnirl <- do.call(c, lapply(args, function(xx) xx@cnirl))
        x
    }
)

