### =========================================================================
### Binning objects
### -------------------------------------------------------------------------
###

setClass("Binning",
    contains="Grouping",
    representation(
        bins="CompressedIntegerList"
    )
)

setMethod("length", "Binning", function(x) length(x@bins))

setMethod("nobj", "Binning", function(x) length(unlist(x@bins)))

setMethod("names", "Binning", function(x) names(x@bins))

setReplaceMethod("names", "Binning",
    function(x, value)
    {
        if (!is.null(value))
            value <- as.character(value)
        names(x@bins) <- value
        x
    }
)

setMethod("[[", "Binning",
    function(x, i, j, ...)
        x@bins[[i]]
)

setMethod("grouplength", "Binning",
    function(x, i=NULL)
    {
        binSizes <- elementLengths(x@bins)
        if (is.null(i))
            binSizes
        else
            binSizes[i]
    }
)

setMethod("togroup", "Binning",
    function(x, j=NULL)
    {
        whichBin <-
          rep.int(seq_len(length(x)), grouplength(x))[order(unlist(x@bins))]
        if (is.null(j))
            whichBin
        else
            whichBin[j]
    }
)

.valid.Binning.names <- function(x)
{
    if (is.null(names(x)))
        return(NULL)
    if (!is.character(names(x)))
        return("the names must be a character vector or NULL")
    if (length(names(x)) != length(x))
        return("number of names and number of elements differ")
    NULL
}

.valid.Binning.values <- function(x)
{
    message <- NULL
    if (any(sapply(x@bins, is.unsorted, strictly=FALSE)))
        message <- c(message, "observations not sorted within bins")
    unlistedBins <- unlist(x@bins)
    names(unlistedBins) <- NULL
    if (!identical(sort(unlistedBins), seq_len(nobj(x))))
        message <- c(message, "observations not properly binned")
    message
}

setValidity2("Binning",
    function(x)
        c(.valid.Binning.names(x), .valid.Binning.values(x))
)

Binning <- function(group=integer(), names=NULL)
{
    if (length(group) == 0) {
        ans <- new("Binning")
    } else {
        if (!is.factor(group)) {
            if (!is.numeric(group))
                stop("'group' must be a factor or a vector of positive integers")
            if (!is.integer(group))
                group <- as.integer(group)
            if (any(group < 1L))
                stop("'group' values must be positive")
            group <- factor(group, levels=as.character(seq_len(max(group))))
        } else if (missing(names))
            names <- levels(group)
        bins <- do.call(IntegerList, split(seq_len(length(group)), group))
        names(bins) <- names
        ans <- new2("Binning", bins=bins, check=FALSE)
    }
    ans
}
