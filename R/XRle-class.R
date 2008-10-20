### =========================================================================
### XRle objects
### -------------------------------------------------------------------------
###
### The XRle virtual class is a general container for storing an
### "external run length encoding".
###

setClass("XRle",
    contains="Sequence",
    representation(
        "VIRTUAL",
        vectorLength="integer",
        lengths="XInteger",
        values="Sequence"
    )
)

setMethod("length", "XRle", function(x) x@vectorLength)

setMethod("[", "XRle",
    function(x, i, j, ..., drop=TRUE)
    {
        breaks <- c(0, cumsum(as.integer(x@lengths)))
        group <- findInterval(i - 1e-6, breaks)
        output <- x@values[group, drop = TRUE]
        if (!drop)
            output <- as(output, class(x))
        output
    }
)

### TODO: Maybe consider defaulting to the "rep" method for Sequence objects.
setMethod("rep", "XRle",
    function(x, times)
    {
        if (!isSingleNumber(times))
            stop("'times' must be a single integer when 'x' is a ",
                 class(x), " object")
        if (!is.integer(times))
            times <- as.integer(times)
        x@vectorLength <- x@vectorLength * times
        x@lengths <- rep(x@lengths, times)
        x@values <- rep(x@values, times)
        x
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Comparison.
###

### FIXME (maybe): Is an XRle object guaranteed to contain the most compact
### representation of a sequence? Given the implementation of the "rep"
### method above it seems that there is no such guarantee, that is, a valid
### XRle object can contain 2 consecutive runs of the same value (i.e. 2
### consecutive repeated values in its 'values' slot). If this is the case,
### then the "==" method below would need to be fixed because it will return
### FALSE even when 'e1' and 'e2' represent the same sequence but have
### different internal RLE representations. For example it will return FALSE
### if 'e1' contains a single run of length 10 with value 3 and 'e2' contains
### 2 runs of length 5 with value 3.
setMethod("==", signature(e1="XRle", e2="XRle"),
    function(e1, e2)
    {
        (length(e1) == length(e2)) &&
          all(e1@lengths == e2@lengths) && all(e1@values == e2@values)
    }
)

