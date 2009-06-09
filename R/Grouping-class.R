### =========================================================================
### Grouping objects
### -------------------------------------------------------------------------
###
### We call "grouping" the action of dividing a collection of NO objects into
### NG groups (some of them eventually empty). The Grouping class and
### subclasses are containers for representing groupings.
###

setClass("Grouping", contains=c("IntegerList", "VIRTUAL"))

setGeneric("nobj", function(x) standardGeneric("nobj"))

setGeneric("grouplength", signature="x",
    function(x, i=NULL) standardGeneric("grouplength")
)

setMethod("grouplength", "Grouping",
    function(x, i=NULL)
    {
        if (is.null(i))
            i <- seq_len(length(x))
        sapply(i, function(ii) length(x[[i]]))
    }
)

setGeneric("members", signature="x",
    function(x, i) standardGeneric("members")
)

setMethod("members", "Grouping",
    function(x, i)
    {
        if (!is.numeric(i))
            stop("subscript 'i' must be a vector of integers")
        if (!is.integer(i))
            i <- as.integer(i)
        sort(unlist(sapply(i, function(ii) x[[ii]])))
    }
)

setGeneric("vmembers", signature="x",
    function(x, L) standardGeneric("vmembers")
)

setMethod("vmembers", "Grouping",
    function(x, L)
    {
        if (!is.list(L))
            stop("'L' must be a list of integer vectors") 
        lapply(L, function(i) members(x, i))
    }
)

setGeneric("togroup", signature="x",
    function(x, j=NULL) standardGeneric("togroup")
)

setGeneric("togrouplength", signature="x",
    function(x, j=NULL) standardGeneric("togrouplength")
)

setMethod("togrouplength", "Grouping",
    function(x, j=NULL)
        grouplength(x, togroup(x, j))
)

### Not needed anymore since R now displays an error message when attempting
### to use "[[<-" on an object for which it's not defined.
setReplaceMethod("[[", "Grouping",
    function(x, i, j,..., value)
    {
        stop("attempt to modify the value of a ", class(x), " instance")
    }
)

setMethod("show", "Grouping",
    function(object)
    {
        NG <- length(object)
        NO <- nobj(object)
        cat("  A ", class(object), " instance with ", NG,
            ifelse(NG == 1, " group ", " groups "),
            "and ", NO,
            ifelse(NO == 1, " object\n", " objects\n"), sep="")
        if (NG == 0L)
            return(invisible(NULL))
        empty_groups <- which(grouplength(object) == 0L)
        cat("Nb of empty groups: ", length(empty_groups),
            " (", 100.00 * length(empty_groups) / NG, "%)\n", sep="")
    }
)



### -------------------------------------------------------------------------
### BiIndexGrouping objects
### -----------------------

#setClass("BiIndexGrouping",
#    contains="Grouping",
#    representation(
#        group2object="list",
#        object2group="integer"
#    )
#)

#setMethod("length", "BiIndexGrouping", function(x) length(x@group2object))

#setMethod("nobj", "BiIndexGrouping", function(x) length(x@object2group))



### -------------------------------------------------------------------------
### H2LGrouping and Dups objects
### ----------------------------
###
### High-to-Low Index Grouping objects.
###

setClass("H2LGrouping",
    contains="Grouping",
    representation(
        high2low="integer",
        low2high="list"
    )
)

### For storing the grouping implicitly defined by the "duplicated"
### relationship between elements of an arbitrary vector.
setClass("Dups", contains="H2LGrouping")

### Two additional accessors for H2LGrouping objects.
setGeneric("high2low", function(x) standardGeneric("high2low"))
setMethod("high2low", "H2LGrouping", function(x) x@high2low)
setGeneric("low2high", function(x) standardGeneric("low2high"))
setMethod("low2high", "H2LGrouping", function(x) x@low2high)

### 'length(x)' and 'nobj(x)' are the same.
setMethod("length", "H2LGrouping", function(x) length(x@low2high))
setMethod("nobj", "H2LGrouping", function(x) length(x@high2low))

setMethod("[[", "H2LGrouping",
    function(x, i, j, ...)
    {
        i <- checkAndTranslateDbleBracketSubscript(x, i)
        if (is.na(x@high2low[i]))
            c(i, x@low2high[[i]])
        else
            integer()
    }
)

### Should be more efficient than the default method for Grouping objects.
setMethod("grouplength", "H2LGrouping",
    function(x, i=NULL)
    {
        group_length <- elementLengths(x@low2high) + 1L
        group_length[!is.na(x@high2low)] <- 0L
        if (is.null(i))
            return(group_length)
        if (!is.numeric(i))
            stop("subscript 'i' must be a vector of integers or NULL")
        if (!is.integer(i))
            i <- as.integer(i)
        if (any(is.na(i)))
            stop("subscript 'i' contains NAs")
        if (any(i < -length(group_length)) || any(i > length(group_length)))
            stop("subscript out of bounds")
        group_length[i]
    }
)

setMethod("members", "H2LGrouping",
    function(x, i)
    {
        if (!is.numeric(i))
            stop("subscript 'i' must be a vector of integers")
        if (!is.integer(i))
            i <- as.integer(i)
        ## NAs and "subscript out of bounds" are checked at the C level
        .Call("H2LGrouping_members", x, i, PACKAGE="IRanges")
    }
)

setMethod("vmembers", "H2LGrouping",
    function(x, L)
    {
        if (!is.list(L))
            stop("'L' must be a list of integer vectors") 
        .Call("H2LGrouping_vmembers", x, L, PACKAGE="IRanges")
    }
)

setMethod("togroup", "H2LGrouping",
    function(x, j=NULL)
    {
        to_group <- x@high2low
        to_group[is.na(to_group)] <- which(is.na(to_group))
        if (is.null(j))
            return(to_group)
        if (!is.numeric(j))
            stop("subscript 'j' must be a vector of integers or NULL")
        if (!is.integer(j))
            j <- as.integer(j)
        if (any(is.na(j)))
            stop("subscript 'j' contains NAs")
        if (any(j < -length(to_group)) || any(j > length(to_group)))
            stop("subscript out of bounds")
        to_group[j]
    }
)

### The default method should be as good (if not better) as this.
#setMethod("togrouplength", "H2LGrouping",
#    function(x)
#    {
#        ans <- rep.int(1L, length(x))
#        mapped_lows <- setdiff(unique(x@high2low), NA)
#        for (low in mapped_lows) {
#            ii <- as.integer(c(low, x@low2high[[low]]))
#            ans[ii] <- length(ii)
#        }
#        ans
#    }
#)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Validity.
###

.makeLow2highFromHigh2low <- function(high2low)
{
    ans <- vector(mode="list", length=length(high2low))
    sparse_ans <- split(seq_along(high2low), high2low)
    ans[as.integer(names(sparse_ans))] <- sparse_ans
    ans
}

.valid.H2LGrouping <- function(x)
{
    if (!is.integer(x@high2low))
        return("the 'high2low' slot must contain an integer vector")
    if (!all(x@high2low >= 1L, na.rm=TRUE))
        return("the 'high2low' slot must contain integer values >= 1")
    if (!all(x@high2low < seq_along(x@high2low), na.rm=TRUE)) {
        problem <- c("when mapped, elements in the 'high2low' slot must be mapped ",
                     "to elements at a lower position")
        return(paste(problem, collapse=""))
    }
    if (!all(is.na(x@high2low[x@high2low]))) {
        problem <- c("when mapped, elements in the 'high2low' slot must be mapped ",
                     "to unmapped elements")
        return(paste(problem, collapse=""))
    }
    if (!is.list(x@low2high))
        return("the 'low2high' slot must contain a list")
    if (length(x@high2low) != length(x@low2high))
        return("the 'high2low' and 'low2high' slots must have the same length")
    if (!identical(.makeLow2highFromHigh2low(x@high2low), x@low2high)) {
        problem <- c("the 'low2high' slot must contain the reverse mapping ",
                     "of the 'high2low' slot")
        return(paste(problem, collapse=""))
    }
    NULL
}

setValidity("H2LGrouping",
    function(object)
    {
        problems <- .valid.H2LGrouping(object)
        if (is.null(problems)) TRUE else problems
    }
)

### For Dups objects only.
setMethod("duplicated", "Dups",
    function(x, incomparables=FALSE, ...) !is.na(high2low(x))
)

### For Dups objects only.
setMethod("show", "Dups",
    function(object)
    {
        percentage <- 100.00 * sum(duplicated(object)) / length(object)
        cat("  A ", class(object), " instance of length ", length(object),
            " (", percentage, "% of duplicates)\n", sep="")
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Constructors.
###

.newH2LGrouping <- function(Class, high2low)
{
    if (!is.numeric(high2low))
        stop("'high2low' must be a vector of integers")
    if (!is.integer(high2low))
        high2low <- as.integer(high2low)
    new(Class, high2low=high2low,
        low2high=.makeLow2highFromHigh2low(high2low))
}

H2LGrouping <- function(high2low=integer())
    .newH2LGrouping("H2LGrouping", high2low)

Dups <- function(high2low=integer())
    .newH2LGrouping("Dups", high2low)



### -------------------------------------------------------------------------
### Partitioning objects
### --------------------
###
### A Partitioning container represents a block-grouping i.e. a grouping
### where each group contains objects that are neighbors in the original
### collection of objects. More formally, a grouping 'x' is a block-grouping
### iff 'togroup(x)' is sorted in increasing order (not necessarily strictly
### increasing). In addition, a Partitioning object can be seen (and
### manipulated) as a Ranges object where all the ranges are adjacent
### starting at 1 (i.e. it covers an integer interval starting at 1
### and with no overlap between the ranges). Therefore the "start/end/width"
### API is implemented on Partitioning objects (in addition to the Grouping
### API).
###
### The Partitioning class is virtual with 2 concrete subclasses:
### PartitioningByEnd and PartitioningByWidth.
### Note that we put Ranges before Grouping in order to have Partitioning
### objects inherit the "show" method for Ranges objects.

setClass("Partitioning",
    contains=c("Ranges", "Grouping"),
    representation(
        "VIRTUAL",
        NAMES="characterORNULL"  # R doesn't like @names !!
    ),
    prototype(
        NAMES=NULL
    )
)

### The default methods below assume that the "length + start/end/width" API
### is already implemented.

setMethod("[[", "Partitioning",
    function(x, i, j, ...)
    {
        i <- checkAndTranslateDbleBracketSubscript(x, i)
        ## The purpose of the code below is to extract 'start(x)[i] - 1'
        ## (stored in 'ans_shift') and 'width(x)[i]' (stored in 'ans_length')
        ## in the fastest possible way. Looks like a convoluted way to
        ## extract those 2 values but it is actually 1000x faster than the
        ## naive way.
        ans_shift <- 0L
        ans_length <- end(x)[i]
        if (i >= 2L) {
            ans_shift <- end(x)[i - 1L]
            ans_length <- ans_length - ans_shift
        }
        seq_len(ans_length) + ans_shift
    }
)

### Pretty inefficient.
setMethod("togroup", "Partitioning",
    function(x, j=NULL)
    {
        to_group <- rep(seq_len(length(x)), times=width(x))
        if (is.null(j))
            return(to_group)
        if (!is.numeric(j))
            stop("subscript 'j' must be a vector of integers or NULL")
        if (!is.integer(j))
            j <- as.integer(j)
        if (any(is.na(j)))
            stop("subscript 'j' contains NAs")
        if (any(j < -length(to_group)) || any(j > length(to_group)))
            stop("subscript out of bounds")
        to_group[j]
    }
)

### Should be more efficient than the default method for Grouping objects.
setMethod("grouplength", "Partitioning",
    function(x, i=NULL)
    {
        x_width <- width(x)
        if (is.null(i))
            return(x_width)
        if (!is.numeric(i))
            stop("subscript 'i' must be a vector of integers or NULL")
        if (!is.integer(i))
            i <- as.integer(i)
        if (any(is.na(i)))
            stop("subscript 'i' contains NAs")
        if (any(i < -length(x_width)) || any(i > length(x_width)))
            stop("subscript out of bounds")
        x_width[i]
    }
)

setMethod("names", "Partitioning", function(x) x@NAMES)

setReplaceMethod("names", "Partitioning",
    function(x, value)
    {
        if (!is.null(value))
            value <- as.character(value)
        unsafe.names(x) <- value
        x
    }
)

.valid.Partitioning <- function(x)
{
    if (is.null(names(x)))
        return(NULL)
    if (!is.character(names(x)))
        return("the names must be a character vector or NULL")
    if (length(names(x)) != length(x))
        return("number of names and number of elements differ")
    NULL
}

setValidity2("Partitioning", .valid.Partitioning)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### PartitioningByEnd uses a compact internal representation that allows
### fast mapping from groups to objects. However, it is not efficient for
### mapping from objects to groups.
###

setClass("PartitioningByEnd",
    contains="Partitioning",
    representation(
        end="integer"
    ),
    prototype(
        end=integer()
    )
)

setMethod("end", "PartitioningByEnd", function(x) x@end)

setMethod("length", "PartitioningByEnd", function(x) length(end(x)))

setMethod("nobj", "PartitioningByEnd",
    function(x)
    {
        x_end <- end(x)
        if (length(x_end) == 0L)
            return(0L)
        x_end[length(x_end)]
    }
)

setMethod("start", "PartitioningByEnd",
    function(x)
    {
        x_end <- end(x)
        if (length(x_end) == 0L)
            return(integer())
        c(1L, x_end[-length(x_end)] + 1L)
    }
)

setMethod("width", "PartitioningByEnd", function(x) diff(c(0L, end(x))))

.valid.PartitioningByEnd <- function(x)
{
    if (!is.integer(end(x)))
        return("the ends must be integers")
    if (length(x) == 0L)
        return(NULL)
    if (any(is.na(end(x))))
        return("the ends cannot be NAs")
    if (.Internal(is.unsorted(end(x), FALSE)))
        return("the ends must be sorted")
    if (end(x)[1L] < 0L)
        return("the ends cannot be negative")
    NULL
}

setValidity2("PartitioningByEnd", .valid.PartitioningByEnd)

PartitioningByEnd <- function(end=integer(), names=NULL)
{
    if (!is.numeric(end))
        stop("'end' must contain integer values")
    if (!is.integer(end))
        end <- as.integer(end)
    new("PartitioningByEnd", end=end, NAMES=names)
}

setAs("Ranges", "PartitioningByEnd",
    function(from)
    {
        ans <- PartitioningByEnd(end(from), names(from))
        if (!identical(start(ans), start(from)))
            stop("the Ranges object to coerce does not represent a partitioning")
        ans
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### PartitioningByWidth uses a compact internal representation too. Storing
### the widths instead of the ends would allow the total number of objects
### (nobj(x)) to be greater than 2^31-1 but note that some methods will break
### when this happens, e.g. nobj, end, etc...
###

setClass("PartitioningByWidth",
    contains="Partitioning",
    representation(
        width="integer"
    ),
    prototype(
        width=integer()
    )
)

setMethod("width", "PartitioningByWidth", function(x) x@width)

setMethod("length", "PartitioningByWidth", function(x) length(width(x)))

setMethod("end", "PartitioningByWidth", function(x) cumsum(width(x)))

setMethod("nobj", "PartitioningByWidth",
    function(x)
    {
        x_end <- end(x)
        if (length(x_end) == 0L)
            return(0L)
        x_end[length(x_end)]
    }
)

setMethod("start", "PartitioningByWidth",
    function(x)
    {
        x_width <- width(x)
        if (length(x_width) == 0L)
            return(integer())
        c(1L, cumsum(x_width[-length(x_width)]) + 1L)
    }
)

.valid.PartitioningByWidth <- function(x)
{
    if (!is.integer(width(x)))
        return("the widths must be integers")
    if (length(x) == 0L)
        return(NULL)
    if (any(is.na(width(x))))
        return("the widths cannot be NAs")
    if (any(width(x) < 0L))
        return("the widths cannot be negative")
    NULL
}

setValidity2("PartitioningByWidth", .valid.PartitioningByWidth)

PartitioningByWidth <- function(width=integer(), names=NULL)
{
    if (!is.numeric(width))
        stop("'width' must contain integer values")
    if (!is.integer(width))
        width <- as.integer(width)
    new("PartitioningByWidth", width=width, NAMES=names)
}

setAs("Ranges", "PartitioningByWidth",
    function(from)
    {
        ans <- PartitioningByWidth(width(from), names(from))
        if (!identical(start(ans), start(from)))
            stop("the Ranges object to coerce does not represent a partitioning")
        ans
    }
)



### -------------------------------------------------------------------------
### Binning objects
### -----------------------

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
          rep(seq_len(length(x)), times=grouplength(x))[order(unlist(x@bins))]
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
        ans <- new("Binning", bins=bins)
    }
    ans
}
