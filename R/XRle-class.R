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
        lengths="XInteger",
        vectorLength="integer"
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

setMethod("subseq", "XRle",
    function(x, start=NA, end=NA, width=NA)
    {
        limits <- new2("IRanges", start=1L, width=length(x), check=FALSE)
        limits <- narrow(limits, start=start, end=end, width=width)
        x[start(limits):end(limits), drop = FALSE]
    }
)

setMethod("rep", "XRle",
    function(x, times) {
        x@values <- rep(x@values, times)
        x@lengths <- rep(x@lengths, times)
        x
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Comparison.
###

### FIXME: Compare the contents, not the addresses!
setMethod("==", signature(e1="XRle", e2="XRle"),
        function(e1, e2) {
            e1@values == e2@values && e1@lenghts == e2@lengths && length(e1) == length(e2)
        }
)

setMethod("!=", signature(e1="XInteger", e2="XInteger"),
        function(e1, e2) {
            e1@values != e2@values ||  e1@lengths != e2@lengths || length(e1) != length(e2)
        }
)
