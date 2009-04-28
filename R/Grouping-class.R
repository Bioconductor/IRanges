### =========================================================================
### Grouping objects
### -------------------------------------------------------------------------
###
### We call "grouping" the action of dividing a collection of NO objects into
### NG groups (some of them eventually empty). The Grouping class and
### subclasses are containers for representing groupings.
###
### Formal description of the Grouping core API
### -------------------------------------------
###
### Groups G_i are indexed from 1 to NG (1 <= i <= NG).
### Object O_j are indexed from 1 to NO (1 <= i <= NO).
### Every object must belong to one group and only one.
### Given that empty groups are allowed, NG can be greater than NO.
### Grouping an empty collection of objects (NO = 0) is supported. In that
### case, all the groups are empty. And only in that case, NG can be zero
### too (no group).
### If 'x' is a Grouping object:
###
### Primitives (no default method):
###
### - length(x): returns the number of groups (NG).
###
### - names(x): returns the names of the groups.
###
### - nobj(x): returns the number of objects (NO). Equivalent to
###   'length(togroup(x))'.
###
### - x[[i]]: returns the indices of the objects (the j's) that belong to
###   G_i. The j's must be returned in ascending order. This provides the
###   mapping from groups to objects (one-to-many mapping).
###
### - togroup(x, j=NULL): returns the index i of the group that O_j belongs
###   to. This provides the mapping from objects to groups (many-to-one
###   mapping). Expected to work in a vectorized fashion. 'togroup(x)' is
###   equivalent to 'togroup(x, seq_len(nobj(x)))': both return the entire
###   mapping in an integer vector of length NO. In fact 'togroup(x, j)' is
###   equivalent to 'y <- togroup(x); y[j]'.
###
### Not primitives (default method provided, based on primitives):
###
### - grouplength(x, i=NULL): returns the number of objects in G_i. Expected
###   to work in a vectorized fashion (unlike 'x[[i]]'). 'grouplength(x)' is
###   equivalent to 'grouplength(x, seq_len(length(x)))'. If 'i' is not NULL,
###   'grouplength(x, i)' is equivalent to:
###       sapply(i, function(ii) length(x[[i]]))
###
### - togrouplength(x, j=NULL): returns the nb of objects that belong to the
###   same group as O_j (including O_j itself). Equivalent to:
###       grouplength(x, togroup(x, j))
###
### Given that "length", "names" and "[[" are defined for Grouping objects,
### they can be considered ListLike objects. Therefore 'as.list(x)' works
### out-of-the-box on them. One important property of Grouping objects is
### that 'sort(unlist(as.list(x)))' and 'seq_len(nobj(x))' should always
### be identical (a consequence of the fact that every object belongs to one
### group and only one).

setClass("Grouping", contains=c("ListLike", "VIRTUAL"))

setGeneric("nobj", function(x) standardGeneric("nobj"))

setGeneric("togroup", signature="x",
    function(x, j=NULL) standardGeneric("togroup")
)

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
        cat(class(object), " instance with ", NG, " groups ",
            "and ", NO, " objects\n", sep="")
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
### H2LGrouping objects
### ------------------
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

### Two additional accessors for H2LGrouping objects.
setGeneric("high2low", function(x) standardGeneric("high2low"))
setMethod("high2low", "H2LGrouping", function(x) x@high2low)
setGeneric("low2high", function(x) standardGeneric("low2high"))
setMethod("low2high", "H2LGrouping", function(x) x@low2high)

### 'length(x)' and 'nobj(x)' are the same.
setMethod("length", "H2LGrouping", function(x) length(x@group2object))
setMethod("nobj", "H2LGrouping", function(x) length(x@object2group))

setMethod("[[", "H2LGrouping",
    function(x, i, j, ...)
    {
        i <- callNextMethod()  # calls "[[" method for ListLike objects
        c(i, x@low2high[[i]])
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

setMethod("grouplength", "H2LGrouping",
    function(x, i=NULL)
    {
        group_length <- sapplyLength(x@low2high) + 1L
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
    if (!all(x@high2low < seq_along(x@high2low), na.rm=TRUE))
        return("when mapped, values in the 'high2low' slot must be mapped ",
               "to lower values")
    if (!all(is.na(x@high2low[x@high2low])))
        return("when mapped, values in the 'high2low' slot must be mapped ",
               "to unmapped values")
    if (!is.list(x@low2high))
        return("the 'low2high' slot must contain a list")
    if (length(x@high2low) != length(x@low2high))
        return("the 'high2low' and 'low2high' slots must have the same length")
    if (!identical(.makeLow2highFromHigh2low(x@high2low), x@low2high))
        return("the 'low2high' slot must contain the reverse mapping ",
               "of the 'high2low' slot")
    NULL
}

setValidity("H2LGrouping",
    function(object)
    {
        problems <- .valid.H2LGrouping(object)
        if (is.null(problems)) TRUE else problems
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Constructor.
###

H2LGrouping <- function(high2low)
    new("H2LGrouping", high2low=high2low,
        low2high=.makeLow2highFromHigh2low(high2low))


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Dups objects.
###
### For storing the grouping implicitly defined by the "duplicated"
### relationship between elements of an arbitrary vector.
###

setClass("Dups", contains="H2LGrouping")

setMethod("duplicated", "Dups",
    function(x, incomparables=FALSE, ...) !is.na(high2low(x))
)

setMethod("show", "Dups",
    function(object)
    {
        percentage <- 100.00 * sum(duplicated(object)) / length(object)
        cat(class(object), " instance of length ", length(object),
            " (", percentage, "% of duplicates)\n", sep="")
    }
)



### -------------------------------------------------------------------------
### Partitioning objects
### --------------------
###
### The Partitioning container represents block-groupings i.e. groupings
### where each group contains objects that are contiguous in the original
### collection of objects. In other words, 'x' is a Partitioning object iff
### 'togroup(x)' is sorted. In addition, a Partitioning object can be seen
### (and manipulated) as a Ranges object where all the ranges are adjacent
### starting at 1 (i.e. it covers an integer interval starting at 1 and with
### no overlap between the ranges). Therefore the "start/end/width" API is
### implemented on Partitioning objects (in addition to the Grouping API).
###

### This implementation of the Partitioning uses a compact internal
### representation:
###   - strength: compactness, fast mapping from groups to objects;
###   - weakness: inefficient mapping from objects to groups.
### Note that we put Ranges before Grouping in order to have Partitioning
### objects inherit the "show" method for Ranges objects.
setClass("Partitioning",
    contains=c("Ranges", "Grouping"),
    representation(
        end="integer",
        NAMES="characterORNULL"  # R doesn't like @names !!
    ),
    prototype(
        end=integer(),
        NAMES=NULL
    )
)

setMethod("end", "Partitioning", function(x) x@end)

setMethod("length", "Partitioning", function(x) length(end(x)))

setMethod("nobj", "Partitioning",
    function(x)
    {
        x_end <- end(x)
        if (length(x_end) == 0L)
            return(0L)
        x_end[length(x_end)]
    }
)

setMethod("start", "Partitioning",
    function(x)
    {
        x_end <- end(x)
        if (length(x_end) == 0L)
            return(integer())
        c(1L, x_end[-length(x_end)] + 1L)
    }
)

setMethod("width", "Partitioning", function(x) diff(c(0L, end(x))))

setMethod("[[", "Partitioning",
    function(x, i, j, ...)
    {
        i <- callNextMethod()  # calls "[[" method for ListLike objects
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
        to_group <- rep(seq_len(length(x)), each=width(x))
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

### Should be more efficient than the default method (hopefully).
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
        if (!is(value, "characterORNULL"))
            stop("'value' must a character vector or NULL")
        ## This works only "by chance" i.e. just because, like IRanges, the
        ## names of a Partitioning object are stored in a slot called "NAMES",
        ## which is what `unsafe.names<-` expects.
        unsafe.names(x) <- value
        x
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Validity.
###

.valid.Partitioning.end <- function(x)
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

.valid.Partitioning.names <- function(x)
{
    if (is.null(names(x)))
        return(NULL)
    if (!is.character(names(x)))
        return("the names must be a character vector or NULL")
    if (length(names(x)) != length(x))
        return("number of names and number of elements differ")
    NULL
}

.valid.Partitioning <- function(x)
{
    c(.valid.Partitioning.end(x),
      .valid.Partitioning.names(x))
}

setValidity2("Partitioning", .valid.Partitioning)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Constructor.
###

Partitioning <- function(end=integer(), names=NULL)
{
    if (!is.numeric(end))
        stop("'end' must contain integer values")
    if (!is.integer(end))
        end <- as.integer(end)
    new("Partitioning", end=end, NAMES=names)
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion.
###

setAs("Ranges", "Partitioning",
    function(from)
    {
        ans <- Partitioning(end(from), names(from))
        if (!identical(start(ans), start(from)))
            stop("'from' does not represent a partitioning")
        ans
    }
)

