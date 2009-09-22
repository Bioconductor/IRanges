### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "reverse" generic and methods.
###

setGeneric("reverse", function(x, ...) standardGeneric("reverse"))

### This method does NOT preserve normality.
.IRanges.reverse <- function(x, ...)
{
    args <- extraArgsAsList(NULL, ...)
    argnames <- names(args)
    n2p <- match(c("start", "end", "use.names"), argnames)
    if (is.na(n2p[1]))
        stop("'start' must be supplied for \"reverse\" method for IRanges objects")
    start <- args[[n2p[1]]]
    if (!is.numeric(start))
        stop("'start' must be a vector of integers")
    if (!is.integer(start))
        start <- as.integer(start)
    if (any(is.na(start)))
        stop("'start' contains NAs")
    if (is.na(n2p[2]))
        stop("'end' must be supplied for \"reverse\" method for IRanges objects")
    end <- args[[n2p[2]]]
    if (!is.numeric(end))
        stop("'end' must be a vector of integers")
    if (!is.integer(end))
        end <- as.integer(end)
    if (any(is.na(end)))
        stop("'end' contains NAs")
    if (!is.na(n2p[3]) && !normargUseNames(args[[n2p[3]]])) {
        unsafe.names(x) <- NULL
    }
    ## WARNING: -end(x) *must* appear first in this expression if we want
    ## the supplied 'start' and 'end' to be recycled properly.
    ## Remember that in R, because of the recycling, addition of numeric
    ## vectors of different lengths is not associative i.e. in general
    ## '(x + y) + z' is not the same as 'x + (y + z)'. For example:
    ##     (integer(6) + 1:2) + 1:3  and  integer(6) + (1:2 + 1:3)
    ## are not the same.
    x@start[] <- -end(x) + start + end
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

