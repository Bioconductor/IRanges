### =========================================================================
### Grouping objects
### -------------------------------------------------------------------------
###
### We call "grouping" the action of dividing a collection of NO objects into
### NG groups (some of them eventually empty). The Grouping class and
### subclasses are containers for representing groupings.
###

setClass("Grouping", contains="IntegerList", representation("VIRTUAL"))

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

### The default method works on any object 'x' for which 'elementLengths(x)'
### works (e.g. Partitioning, List, list). Not very efficient.
setMethod("togroup", "ANY",
    function(x, j=NULL)
    {
        elt_len <- elementLengths(x)
        to_group <- rep.int(seq_len(length(elt_len)), elt_len)
        if (is.null(j))
            return(to_group)
        if (!is.numeric(j))
            stop("subscript 'j' must be a vector of integers or NULL")
        if (!is.integer(j))
            j <- as.integer(j)
        bound <- length(to_group)
        if (anyMissingOrOutside(j, -bound, bound))
            stop("subscript 'j' contains NAs or out of bounds indices")
        to_group[j]
    }
)

tofactor <- function(x)
{
    .Deprecated("PartitioningByEnd")
    factor(togroup(x), seq_len(length(x)))
}

setGeneric("togrouplength", signature="x",
    function(x, j=NULL) standardGeneric("togrouplength")
)

setMethod("togrouplength", "Grouping",
    function(x, j=NULL)
        grouplength(x, togroup(x, j))
)

setMethod("show", "Grouping",
    function(object)
    {
        NG <- length(object)
        NO <- nobj(object)
        cat(class(object), " with ",
            NG, ifelse(NG == 1, " group ", " groups "),
            "and ", NO, ifelse(NO == 1, " object\n", " objects\n"),
            sep="")
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
        bound <- length(group_length)
        if (anyMissingOrOutside(i, -bound, bound))
            stop("subscript 'i' contains NAs or out of bounds indices")
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
        .Call2("H2LGrouping_members", x, i, PACKAGE="IRanges")
    }
)

setMethod("vmembers", "H2LGrouping",
    function(x, L)
    {
        if (!is.list(L))
            stop("'L' must be a list of integer vectors") 
        .Call2("H2LGrouping_vmembers", x, L, PACKAGE="IRanges")
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
        bound <- length(to_group)
        if (anyMissingOrOutside(j, -bound, bound))
            stop("subscript 'j' contains NAs or out of bounds indices")
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
### More operations on H2LGrouping objects. These operations are NOT part of
### the Grouping core API.
###

### The rank of group G_i is the number of non-empty groups that are before
### G_i plus one. Or, equivalently, it's the number of non-empty groups with
### an index <= i.
setGeneric("grouprank", signature="x",
    function(x, i=NULL) standardGeneric("grouprank")
)
setMethod("grouprank", "H2LGrouping",
    function(x, i=NULL)
    {
        ans <- cumsum(is.na(high2low(x)))
        if (!is.null(i))
            ans <- ans[i]
        return(ans)
    }
)

### togrouprank() returns the mapping from objects to group ranks.
### An important property of togrouprank() is that:
###   togrouprank(x, neg_idx)
### and
###   seq_len(length(neg_idx))
### are identical, where 'neg_idx' is the vector of the indices of
### the non-empty groups i.e.
###   neg_idx <- which(grouplength(x) != 0L)
setGeneric("togrouprank", signature="x",
     function(x, j=NULL) standardGeneric("togrouprank")
)
setMethod("togrouprank", "H2LGrouping",
    function(x, j=NULL)
    {
        to_group <- togroup(x)
        group_rank <- grouprank(x)
        ans <- group_rank[to_group]
        if (!is.null(j))
            ans <- ans[j]
        return(ans)
    }
)

.makeLow2highFromHigh2low <- function(high2low)
{
    ans <- vector(mode="list", length=length(high2low))
    sparse_ans <- split(seq_along(high2low), high2low)
    ans[as.integer(names(sparse_ans))] <- sparse_ans
    ans
}

setReplaceMethod("length", "H2LGrouping",
    function(x, value)
    {
        if (!isSingleNumber(value))
            stop("length must be a single integer")
        if (!is.integer(value))
            value <- as.integer(value)
        if (value < 0L)
            stop("length cannot be negative")
        if (value > length(x))
            stop("cannot make a ", class(x), " instance longer")
        length(x@high2low) <- value
        x@low2high <- .makeLow2highFromHigh2low(x@high2low)
        x
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Validity.
###

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
        cat(class(object), " of length ", length(object),
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
    new2(Class, high2low=high2low,
         low2high=.makeLow2highFromHigh2low(high2low),
         check=FALSE)
}

H2LGrouping <- function(high2low=integer())
    .newH2LGrouping("H2LGrouping", high2low)

Dups <- function(high2low=integer())
    .newH2LGrouping("Dups", high2low)

### If 'x' is a vector-like object for which "[" and "==" are defined, then
### 'high2low(x)' can be *conceptually* defined with:
###
###   high2low <- function(x)
###       sapply(seq_len(length(x)),
###              function(i) match(x[i], x[seq_len(i-1L)]))
###
### Of course this is *very* inefficient (quadratic in time), its only value
### being to describe the semantic:
###
###   > x <- as.integer(c(2,77,4,4,7,2,8,8,4,99))
###   > high2low(x)
###    [1] NA NA NA  3 NA  1 NA  7  3 NA
###   > bigx <- rep.int(x, 10000)
###   > system.time(high2low(bigx))
###      user  system elapsed 
###   284.805   9.792 294.888
###
setMethod("high2low", "vector",
    function(x)
    {
        ## Author: Harris A. Jaffee
        ans <- match(x, x)
        ans[ans == seq_len(length(x))] <- NA_integer_
        return(ans)
    }
)

### The "high2low" method for Vector objects uses an implementation that
### is O(n*log(n)) in time but it requires that "order" be defined for 'x'
### (in addition to "[" and "==").
setMethod("high2low", "Vector",
    function(x)
    {
        ## The 2 lines below are equivalent but much faster than
        ## ans <- rep.int(NA_integer_, length(x))
        ans <- integer(length(x))
        ans[] <- NA_integer_
        if (length(x) <= 1L)
            return(ans)
        x_order <- order(x)
        low <- x_order[1L]
        for (i in 2:length(x)) {
            high <- x_order[i]
            if (x[high] == x[low])
                ans[high] <- low
            else
                low <- high
        }
        return(ans)
    }
)


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
        bound <- length(x_width)
        if (anyMissingOrOutside(i, -bound, bound))
            stop("subscript 'i' contains NAs or out of bounds indices")
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

setMethod("width", "PartitioningByEnd", function(x) diffWithInitialZero(end(x)))

.valid.PartitioningByEnd <- function(x)
{
    if (!is.integer(end(x)))
        return("the ends must be integers")
    if (length(x) == 0L)
        return(NULL)
    if (anyMissing(end(x)))
        return("the ends cannot be NAs")
    if (isNotSorted(end(x)))
        return("the ends must be sorted")
    if (end(x)[1L] < 0L)
        return("the ends cannot be negative")
    if (!is.null(names(end(x))))
        return("the ends should not be named")
    NULL
}

setValidity2("PartitioningByEnd", .valid.PartitioningByEnd)

PartitioningByEnd <- function(x=integer(), NG=NULL, names=NULL)
{
    if (is(x, "CompressedList")) {
        ## Behaves like a getter for the 'partitioning' slot.
        if (!is.null(NG))
            warning("when 'x' is a CompressedList object, ",
                    "the 'NG' argument is ignored")
        if (!is.null(names))
            warning("when 'x' is a CompressedList object, ",
                    "the 'names' argument is ignored")
        return(x@partitioning)
    }
    if (is.list(x) || is(x, "List")) {
        if (!is.null(NG))
            warning("'NG' argument is ignored when 'x' is ",
                    "a CompressedList object")
        x <- cumsum(elementLengths(x))
    } else {
        if (!is.numeric(x))
            stop("'x' must be either a list-like object ",
                 "or a sorted integer vector")
        if (!is.integer(x))
            x <- as.integer(x)
        if (isNotSorted(x))
            stop("when 'x' is an integer vector, it must be sorted")
        if (!is.null(NG)) {
            ## When 'NG' (number of groups) is supplied, then 'x' is considered
            ## to represent the group assignment of a collection of 'length(x)'
            ## objects. Therefore the values in 'x' must be >= 1 and <= 'NG'.
            ## ADDITIONALLY, 'x' must be *sorted* (not strictly) so it can be
            ## reconstructed from the object returned by PartitioningByEnd()
            ## by doing togroup() on that object.
            if (!isSingleNumber(NG))
                stop("'NG' must be either NULL or a single integer")
            if (!is.integer(NG))
                NG <- as.integer(NG)
            NO <- length(x)  # nb of objects
            if (NG == 0L) {
                if (NO != 0L)
                    stop("when 'NG' is 0, 'x' must be of length 0")
            } else {
                ## 'x' is expected to be non-decreasing and with values >= 1
                ## and <= 'NG'.
                x <- cumsum(tabulate(x, nbins=NG))
                ## 'x[NG]' is guaranteed to be <= 'NO'.
                if (x[NG] != NO)
                    stop("when 'NG' is supplied, values in 'x' must ",
                         "be >= 1 and <= 'NG'")
            }
        }
    }
    if (is.null(names)) {
        ans_names <- names(x)
    } else {
        if (!is.character(names) || length(names) != length(x))
            stop("'names' must be either NULL or a character vector of length ",
                 "'NG' (if supplied) or 'length(x)' (if 'NG' is not supplied)")
        ans_names <- names
    }
    new2("PartitioningByEnd", end=unname(x), NAMES=ans_names, check=FALSE)
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
    if (anyMissing(width(x)))
        return("the widths cannot be NAs")
    if (any(width(x) < 0L))
        return("the widths cannot be negative")
    if (!is.null(names(width(x))))
        return("the widths should not be named")
    NULL
}

setValidity2("PartitioningByWidth", .valid.PartitioningByWidth)

PartitioningByWidth <- function(x=integer(), NG=NULL, names=NULL)
{
    if (is.list(x) || is(x, "List")) {
        if (!is.null(NG))
            warning("'NG' argument is ignored when 'x' is ",
                    "a CompressedList object")
        x <- elementLengths(x)
    } else {
        if (!is.numeric(x))
            stop("'x' must be either a list-like object or an integer vector")
        if (!is.integer(x))
            x <- as.integer(x)
        if (!is.null(NG)) {
            ## When 'NG' (number of groups) is supplied, then 'x' is considered
            ## to represent the group assignment of a collection of 'length(x)'
            ## objects. Therefore the values in 'x' must be >= 1 and <= 'NG'.
            ## ADDITIONALLY, 'x' must be *sorted* (not strictly) so it can be
            ## reconstructed from the object returned by PartitioningByWidth()
            ## by doing togroup() on that object.
            if (isNotSorted(x))
                stop("when 'x' is an integer vector, it must be sorted")
            if (!isSingleNumber(NG))
                stop("'NG' must be either NULL or a single integer")
            if (!is.integer(NG))
                NG <- as.integer(NG)
            NO <- length(x)  # nb of objects
            if (NG == 0L) {
                if (NO != 0L)
                    stop("when 'NG' is 0, 'x' must be of length 0")
            } else {
                ## 'x' is expected to be non-decreasing and with values >= 1
                ## and <= 'NG'.
                x <- tabulate(x, nbins=NG)
                ## 'sum(x)' is guaranteed to be <= 'NO'.
                if (sum(x) != NO)
                    stop("when 'NG' is supplied, values in 'x' must ",
                         "be >= 1 and <= 'NG'")
            }
        }
    }
    if (is.null(names)) {
        ans_names <- names(x)
    } else {
        if (!is.character(names) || length(names) != length(x))
            stop("'names' must be either NULL or a character vector of length ",
                 "'NG' (if supplied) or 'length(x)' (if 'NG' is not supplied)")
        ans_names <- names
    }
    new2("PartitioningByWidth", width=unname(x), NAMES=ans_names, check=FALSE)
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


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### findOverlaps()
###

### A simple findOverlaps method that doesn't use IntervalTree but works only
### on a subject with *adjacent* ranges sorted non-decreasingly.
### Can be 30% faster or more than the real findOverlaps() (IntervalTree-based)
### when 'query' is such that 'start(query)' and 'end(query)' are also sorted
### non-decreasingly (which is the case if for example 'query' is a
### Partitioning object).
### TODO: Add a "findOverlaps" method for Partitioning,Partitioning in the
### findOverlaps-methods.R file that calls this.
findOverlaps_Ranges_Partitioning <- function(query, subject,
                                             hit.empty.query.ranges=FALSE,
                                             hit.empty.subject.ranges=FALSE)
{
    if (!is(query, "Ranges"))
        stop("'query' must be a Ranges object")
    if (!is(subject, "Partitioning"))
        stop("'subject' must be a Partitioning object")
    if (!isTRUEorFALSE(hit.empty.query.ranges) ||
        !isTRUEorFALSE(hit.empty.subject.ranges))
        stop("'hit.empty.query.ranges' and 'hit.empty.subject.ranges' ",
             "must be TRUE or FALSE")
    q_len <- length(query)
    q_start <- start(query)
    q_end <- end(query)
    s_len <- length(subject)
    s_end <- end(subject)
    if (!hit.empty.query.ranges) {
        q_idx <- which(width(query) != 0L)
        q_start <- q_start[q_idx]
        q_end <- q_end[q_idx]
    }
    if (!hit.empty.subject.ranges) {
        s_idx <- which(width(subject) != 0L)
        s_end <- s_end[s_idx]
    }
    vec <- c(0L, s_end) + 0.5
    q_start2subject <- findInterval(q_start, vec)
    q_end2subject <- findInterval(q_end, vec)
    q_hits <- rep.int(seq_len(length(q_start)),
                      q_end2subject - q_start2subject + 1L)
    s_hits <- mseq(q_start2subject, q_end2subject)
    ## If 'query' is a Partitioning object, all hits are guaranteed to be
    ## valid.
    if (!is(query, "Partitioning")) {
        ## Remove invalid hits.
        is_valid <- 1L <= s_hits & s_hits <= length(s_end)
        q_hits <- q_hits[is_valid]
        s_hits <- s_hits[is_valid]
    }
    ## Remap hits to original query/subject.
    if (!hit.empty.query.ranges)
        q_hits <- q_idx[q_hits]
    if (!hit.empty.subject.ranges)
        s_hits <- s_idx[s_hits]

    ## Make and return Hits object.
    new2("Hits", queryHits=q_hits, subjectHits=s_hits,
                 queryLength=q_len, subjectLength=s_len)
}   

