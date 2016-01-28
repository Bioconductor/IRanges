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

setMethod("length", "GappedRanges",
    function(x)
    {
        .Deprecated(msg="GappedRanges objects are deprecated")
        length(x@cnirl)
    }
)

setMethod("start", "GappedRanges",
    function(x, ...)
    {
        .Deprecated(msg="GappedRanges objects are deprecated")
        CompressedNormalIRangesList.min(x@cnirl, FALSE)
    }
)

setMethod("end", "GappedRanges",
    function(x, ...)
    {
        .Deprecated(msg="GappedRanges objects are deprecated")
        CompressedNormalIRangesList.max(x@cnirl, FALSE)
    }
)

setGeneric("ngap", function(x) standardGeneric("ngap"))
setMethod("ngap", "GappedRanges",
    function(x)
    {
        .Deprecated(msg="GappedRanges objects are deprecated")
        elementNROWS(x) - 1L
    }
)

setMethod("names", "GappedRanges",
    function(x)
    {
        .Deprecated(msg="GappedRanges objects are deprecated")
        names(x@cnirl)
    }
)

setReplaceMethod("names", "GappedRanges",
    function(x, value)
    {
        .Deprecated(msg="GappedRanges objects are deprecated")
        names(x@cnirl) <- value
        x
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Validity.
###

.valid.GappedRanges <- function(x)
{
    .Deprecated(msg="GappedRanges objects are deprecated")
    .Call2("valid_GappedRanges", x, 0L, PACKAGE="IRanges")
}

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

.as.data.frame.GappedRanges <- function(x, row.names=NULL, optional=FALSE, ...)
{
    ans <- callNextMethod(unname(x), row.names, optional, ...)
    ans$ngap <- ngap(x)
    ans$names <- names(x)
    ans
}
setMethod("as.data.frame", "GappedRanges", .as.data.frame.GappedRanges)

setMethod("show", "GappedRanges",
    function(object)
    {
        .Deprecated(msg="GappedRanges objects are deprecated")
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
              c(head(x, n=9L), "...", tail(x, n=9L))
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
setMethod("getListElement", "GappedRanges",
    function(x, i, exact=TRUE)
    {
        .Deprecated(msg="GappedRanges objects are deprecated")
        i <- normalizeDoubleBracketSubscript(i, x, exact=exact,
                                             error.if.nomatch=TRUE)
        newNormalIRangesFromIRanges(x@cnirl[[i]], check=FALSE)
    }
)

### WARNING: We override the *semantic* of the "elementNROWS" method for
### Ranges objects.
setMethod("elementNROWS", "GappedRanges",
    function(x)
    {
        .Deprecated(msg="GappedRanges objects are deprecated")
        elementNROWS(x@cnirl)
    }
)

setMethod("extractROWS", "GappedRanges",
    function(x, i)
    {
        .Deprecated(msg="GappedRanges objects are deprecated")
        i <- normalizeSingleBracketSubscript(i, x, as.NSBS=TRUE)
        x@cnirl <- extractROWS(x@cnirl, i)
        x@elementMetadata <- extractROWS(x@elementMetadata, i)
        x
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Combining.
###

setMethod("c", "GappedRanges",
    function(x, ..., recursive=FALSE)
    {
        .Deprecated(msg="GappedRanges objects are deprecated")
        if (!identical(recursive, FALSE))
            stop("\"c\" method for GappedRanges objects ",
                 "does not support the 'recursive' argument")
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

