### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "reverse" generic and methods.
###

setGeneric("reverse", signature="x",
    function(x, ...) standardGeneric("reverse")
)

### This method does NOT preserve normality.
.IRanges.reverse <- function(x, ...)
{
    args <- extraArgsAsList(NULL, ...)
    argnames <- names(args)
    n2p <- match(c("start", "end", "use.names"), argnames)
    if (is.na(n2p[1]))
        stop("'start' must be supplied for \"reverse\" method for IRanges objects")
    start <- normargSingleStart(args[[n2p[1]]])
    if (is.na(n2p[2]))
        stop("'end' must be supplied for \"reverse\" method for IRanges objects")
    end <- normargSingleEnd(args[[n2p[2]]])
    if (!is.na(n2p[3]) && !normargUseNames(args[[n2p[3]]])) {
        unsafe.names(x) <- NULL
    }
    x@start <- start + end - end(x)
    x
}

setMethod("reverse", "IRanges", .IRanges.reverse)

setMethod("reverse", "NormalIRanges",
    function(x, ...)
    {
        ## callNextMethod() temporarily breaks 'x' as a NormalIRanges object
        ## because the returned ranges are ordered from right to left.
        x <- callNextMethod()
        unsafe.update(x, start=rev(start(x)), width=rev(width(x)), names=rev(names(x)))
    }
)

setMethod("reverse", "XRle",
    function(x, ...)
    {
        x@lengths <- reverse(x@lengths)
        x@values <- reverse(x@values)
        x
    }
)

setMethod("reverse", "MaskCollection",
    function(x, ...)
    {
        start <- 1L
        end <- width(x)
        x@nir_list <- lapply(nir_list(x),
            function(nir) reverse(nir, start=start, end=end)
        )
        x
    }
)

