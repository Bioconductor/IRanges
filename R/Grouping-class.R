### =========================================================================
### Grouping objects
### -------------------------------------------------------------------------
###
### We call "grouping" an arbitrary mapping from a collection of NO objects
### to a collection of NG groups, or, more formally, a bipartite graph
### between integer sets [1, NO] and [1, NG]. Objects mapped to a given group
### are said to belong to, or to be assigned to, or to be in that group.
### Additionally, the objects in each group are ordered. So for example the
### 2 following groupings are considered different:
###
###   Grouping 1: NG = 3, NO = 5
###               group   objects
###                   1 : 4, 2
###                   2 :
###                   3 : 4
###
###   Grouping 2: NG = 3, NO = 5
###               group   objects
###                   1 : 2, 4
###                   2 :
###                   3 : 4
###
### There are no restriction on the mapping e.g. any object can be mapped
### to 0, 1, or more groups, and can be mapped twice to the same group. Also
### some or all the groups can be empty.
###

### The Grouping class is a virtual class that formalizes the most general
### kind of grouping. More specific groupings (e.g. many-to-one mappings)
### are formalized via specific Grouping subclasses.
setClass("Grouping", contains="IntegerList", representation("VIRTUAL"))

setGeneric("nobj", function(x) standardGeneric("nobj"))

setGeneric("grouplengths", signature="x",
    function(x, i=NULL) standardGeneric("grouplengths")
)

.subset_by_integer <- function(x, i=NULL)
{
    if (is.null(i))
        return(x)
    if (!is.numeric(i))
        stop(wmsg("subscript must be NULL or an integer vector"))
    if (!is.integer(i))
        i <- as.integer(i)
    x_len <- length(x)
    if (S4Vectors:::anyMissingOrOutside(i, -x_len, x_len))
        stop(wmsg("subscript contains NAs or out of bounds indices"))
    x[i]
}

setMethod("grouplengths", "Grouping",
    function(x, i=NULL)
    {
        x_grouplens <- elementNROWS(x)
        .subset_by_integer(x_grouplens, i)
    }
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
        empty_groups <- which(grouplengths(object) == 0L)
        cat("Nb of empty groups: ", length(empty_groups),
            " (", 100.00 * length(empty_groups) / NG, "%)\n", sep="")
    }
)


### -------------------------------------------------------------------------
### ManyToOneGrouping objects
### -------------------------

### A ManyToOneGrouping object represents a grouping where every object in
### the collection belongs to exactly one group.
setClass("ManyToOneGrouping", contains="Grouping", representation("VIRTUAL"))

setMethod("nobj", "ManyToOneGrouping", function(x) sum(grouplengths(x)))

setGeneric("members", signature="x",
    function(x, i) standardGeneric("members")
)

setMethod("members", "ManyToOneGrouping",
    function(x, i)
    {
        if (!is.numeric(i))
            stop(wmsg("subscript 'i' must be a vector of integers"))
        if (!is.integer(i))
            i <- as.integer(i)
        sort(unlist(sapply(i, function(ii) x[[ii]])))
    }
)

setGeneric("vmembers", signature="x",
    function(x, L) standardGeneric("vmembers")
)

setMethod("vmembers", "ManyToOneGrouping",
    function(x, L)
    {
        if (!is.list(L))
            stop(wmsg("'L' must be a list of integer vectors"))
        lapply(L, function(i) members(x, i))
    }
)

setGeneric("togroup", signature="x",
    function(x, j=NULL) standardGeneric("togroup")
)

### Works on any ManyToOneGrouping object 'x' for which unlist() and
### elementNROWS() work.
setMethod("togroup", "ManyToOneGrouping",
    function(x, j=NULL)
    {
        x_togroup <- unlist(x, use.names=FALSE)
        x_eltNROWS <- elementNROWS(x)
        x_togroup[x_togroup] <- rep.int(seq_along(x_eltNROWS), x_eltNROWS)
        .subset_by_integer(x_togroup, j)
    }
)

setGeneric("togrouplength", signature="x",
    function(x, j=NULL) standardGeneric("togrouplength")
)

setMethod("togrouplength", "ManyToOneGrouping",
    function(x, j=NULL) grouplengths(x, togroup(x, j))
)


### -------------------------------------------------------------------------
### ManyToManyGrouping objects
### -------------------------

### A ManyToManyGrouping object represents a grouping where objects
### can map to any number of groups.
setClass("ManyToManyGrouping", contains="Grouping", representation("VIRTUAL"))

### -------------------------------------------------------------------------
### BiIndexGrouping objects
### -----------------------

#setClass("BiIndexGrouping",
#    contains="ManyToOneGrouping",
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
### High-to-Low Index ManyToOneGrouping objects.
###

setClass("H2LGrouping",
    contains="ManyToOneGrouping",
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

setMethod("getListElement", "H2LGrouping",
    function(x, i, exact=TRUE)
    {
        i <- normalizeDoubleBracketSubscript(i, x, exact=exact,
                                             error.if.nomatch=TRUE)
        if (is.na(x@high2low[i]))
            c(i, x@low2high[[i]])
        else
            integer()
    }
)

### Should be more efficient than the default method for ManyToOneGrouping
### objects.
setMethod("grouplengths", "H2LGrouping",
    function(x, i=NULL)
    {
        x_grouplens <- elementNROWS(x@low2high) + 1L
        x_grouplens[!is.na(x@high2low)] <- 0L
        .subset_by_integer(x_grouplens, i)
    }
)

setMethod("members", "H2LGrouping",
    function(x, i)
    {
        if (!is.numeric(i))
            stop(wmsg("subscript 'i' must be a vector of integers"))
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
            stop(wmsg("'L' must be a list of integer vectors"))
        .Call2("H2LGrouping_vmembers", x, L, PACKAGE="IRanges")
    }
)

setMethod("togroup", "H2LGrouping",
    function(x, j=NULL)
    {
        x_togroup <- x@high2low
        x_togroup[is.na(x_togroup)] <- which(is.na(x_togroup))
        .subset_by_integer(x_togroup, j)
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
### the core Grouping API.
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
###   seq_along(neg_idx)
### are identical, where 'neg_idx' is the vector of the indices of
### the non-empty groups i.e.
###   neg_idx <- which(grouplengths(x) != 0L)
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

setReplaceMethod("length", "H2LGrouping",
    function(x, value)
    {
        if (!isSingleNumber(value))
            stop(wmsg("length must be a single integer"))
        if (!is.integer(value))
            value <- as.integer(value)
        if (value < 0L)
            stop(wmsg("length cannot be negative"))
        if (value > length(x))
            stop(wmsg("cannot make a ", class(x), " instance longer"))
        length(x@high2low) <- value
        x@low2high <- S4Vectors:::reverseSelfmatchMapping(x@high2low)
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
    if (!identical(S4Vectors:::reverseSelfmatchMapping(x@high2low),
                   x@low2high))
    {
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
.duplicated.Dups <- function(x, incomparables=FALSE)
{
    if (!identical(incomparables, FALSE))
        stop(wmsg("\"duplicated\" method for Dups objects ",
                  "only accepts 'incomparables=FALSE'"))
    !is.na(high2low(x))
}
setMethod("duplicated", "Dups", .duplicated.Dups)

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
        stop(wmsg("'high2low' must be a vector of integers"))
    if (!is.integer(high2low))
        high2low <- as.integer(high2low)
    new2(Class, high2low=high2low,
         low2high=S4Vectors:::reverseSelfmatchMapping(high2low),
         check=FALSE)
}

H2LGrouping <- function(high2low=integer())
    .newH2LGrouping("H2LGrouping", high2low)

Dups <- function(high2low=integer())
    .newH2LGrouping("Dups", high2low)

setMethod("high2low", "ANY",
    function(x)
    {
        ans <- selfmatch(x)
        ans[ans == seq_along(x)] <- NA_integer_
        ans
    }
)


### -------------------------------------------------------------------------
### GroupingRanges objects
### ----------------------
###
### A GroupingRanges object represents a "block-grouping", that is, a
### grouping where each group is a block of adjacent elements in the original
### collection of objects. GroupingRanges objects support the Ranges API (e.g.
### start/end/width) in addition to the Grouping API.
###

setClass("GroupingRanges",
    ## We put Ranges before Grouping so GroupingRanges objects inherit the
    ## "show" method for Ranges objects instead of the method for Grouping
    ## objects.
    contains=c("Ranges", "Grouping"),
    representation("VIRTUAL")
)

### Overwrite default method with optimized method for GroupingRanges objects.
setMethod("grouplengths", "GroupingRanges",
    function(x, i=NULL)
    {
        x_width <- width(x)
        .subset_by_integer(x_width, i)
    }
)

setClass("GroupingIRanges", contains=c("IRanges", "GroupingRanges"))


### -------------------------------------------------------------------------
### Partitioning objects
### --------------------
###
### A Partitioning object is a GroupingRanges object where the blocks fully
### cover the original collection of objects and don't overlap. This makes
### them many-to-one groupings. Furthermore, the blocks must be ordered by
### ascending position on the original collection of objects.
### Note that for a Partitioning object 'x', 'togroup(x)' is sorted in
### increasing order (not necessarily strictly increasing).
###
### The Partitioning class is virtual with 2 concrete direct subclasses:
### PartitioningByEnd and PartitioningByWidth.
###

setClass("Partitioning",
    contains=c("GroupingRanges", "ManyToOneGrouping"),
    representation(
        "VIRTUAL",
        NAMES="character_OR_NULL"  # R doesn't like @names !!
    ),
    prototype(
        NAMES=NULL
    )
)

### The default methods below assume that the "length + start/end/width" API
### is already implemented.

setMethod("getListElement", "Partitioning",
    function(x, i, exact=TRUE)
    {
        i <- normalizeDoubleBracketSubscript(i, x, exact=exact,
                                             error.if.nomatch=TRUE)
        ## The purpose of the code below is to extract 'start(x)[i] - 1'
        ## (stored in 'ans_shift') and 'width(x)[i]' (stored in 'ans_len')
        ## in the fastest possible way. Looks like a convoluted way to
        ## extract those 2 values but it is actually 1000x faster than the
        ## naive way.
        ans_shift <- 0L
        ans_len <- end(x)[i]
        if (i >= 2L) {
            ans_shift <- end(x)[i - 1L]
            ans_len <- ans_len - ans_shift
        }
        seq_len(ans_len) + ans_shift
    }
)

### Overwrite method for ManyToOneGrouping objects with optimized method for
### Partitioning objects.
setMethod("togroup", "Partitioning",
    function(x, j=NULL)
    {
        x_width <- width(x)
        x_togroup <- rep.int(seq_along(x_width), x_width)
        .subset_by_integer(x_togroup, j)
    }
)

setMethod("names", "Partitioning", function(x) x@NAMES)

setReplaceMethod("names", "Partitioning", set_IRanges_names)

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

setMethod("NSBS", "Partitioning",
          function(i, x, exact=TRUE, strict.upper.bound=TRUE, allow.NAs=FALSE)
          {
              i <- range(i)
              callNextMethod()
          })

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

### Overwrite method for Ranges objects with optimized method for
### PartitioningByEnd objects.
setMethod("length", "PartitioningByEnd", function(x) length(end(x)))

### Overwrite method for ManyToOneGrouping objects with optimized method for
### PartitioningByEnd objects.
setMethod("nobj", "PartitioningByEnd",
    function(x) S4Vectors:::last_or(end(x), 0L)
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

setMethod("width", "PartitioningByEnd",
    function(x) S4Vectors:::diffWithInitialZero(end(x))
)

.valid.PartitioningByEnd <- function(x)
{
    if (!is.integer(end(x)))
        return("the ends must be integers")
    if (length(x) == 0L)
        return(NULL)
    if (S4Vectors:::anyMissing(end(x)))
        return("the ends cannot be NAs")
    if (S4Vectors:::isNotSorted(end(x)))
        return("the ends must be sorted")
    if (end(x)[1L] < 0L)
        return("the ends cannot be negative")
    if (!is.null(names(end(x))))
        return("the ends should not be named")
    NULL
}

setValidity2("PartitioningByEnd", .valid.PartitioningByEnd)

.numeric2end <- function(x=integer(0), NG=NULL)
{
    if (!is.integer(x))
        x <- as.integer(x)
    if (S4Vectors:::anyMissingOrOutside(x, 0L))
        stop(wmsg("when 'x' is an integer vector, ",
                  "it cannot contain NAs or negative values"))
    if (S4Vectors:::isNotSorted(x))
        stop(wmsg("when 'x' is an integer vector, ",
                  "it must be sorted"))
    if (is.null(NG))
        return(x)
    ## When 'NG' (number of groups) is supplied, then 'x' is considered
    ## to represent the group assignment of a collection of 'length(x)'
    ## objects. Therefore the values in 'x' must be >= 1 and <= 'NG'.
    ## ADDITIONALLY, 'x' must be *sorted* (not strictly) so it can be
    ## reconstructed from the object returned by PartitioningByEnd()
    ## by doing togroup() on that object.
    if (!isSingleNumber(NG))
        stop(wmsg("'NG' must be either NULL or a single integer"))
    if (!is.integer(NG))
        NG <- as.integer(NG)
    NO <- length(x)  # nb of objects
    if (NG == 0L) {
        if (NO != 0L)
            stop(wmsg("when 'NG' is 0, 'x' must be of length 0"))
    } else {
        ## 'x' is expected to be non-decreasing and with values >= 1
        ## and <= 'NG'.
        x <- cumsum(tabulate(x, nbins=NG))
        ## 'x[NG]' is guaranteed to be <= 'NO'.
        if (x[NG] != NO)
            stop(wmsg("when 'NG' is supplied, values in 'x' must ",
                      "be >= 1 and <= 'NG'"))
    }
    x
}

.prepare_Partitioning_names <- function(names, ans_len, NG, x_names)
{
    if (!is.null(names)) {
        if (!is.character(names) || length(names) != ans_len)
            stop(wmsg("'names' must be either NULL or a character vector ",
                      "of length 'NG' (if supplied) or 'length(x)' ",
                      "(if 'NG' is not supplied)"))
        return(names)
    }
    if (is.null(NG))
        return(x_names)  # should be of length 'ans_len'
    NULL
}

PartitioningByEnd <- function(x=integer(0), NG=NULL, names=NULL)
{
    if (is(x, "List") || is.list(x)) {
        if (!is.null(NG))
            warning(wmsg("'NG' argument is ignored when 'x' ",
                         "is a list-like object"))
        if (is(x, "CompressedList")) {
            ## Behaves like a getter for the 'partitioning' slot.
            ans <- x@partitioning
            if (!is.null(names))
                names(ans) <- names
            return(ans)
        }
        if (is(x, "PartitioningByEnd")) {
            if (!is.null(names))
                names(x) <- names
            return(x)
        }
        x_names <- names(x)
        ans_end <- cumsum(elementNROWS(x))
    } else {
        if (!is.numeric(x))
            stop(wmsg("'x' must be either a list-like object or ",
                      "a sorted vector of non-NA non-negative integers"))
        x_names <- names(x)
        ans_end <- .numeric2end(x, NG)
    }
    ans_names <- .prepare_Partitioning_names(names, length(ans_end),
                                             NG, x_names)
    new2("PartitioningByEnd", end=unname(ans_end), NAMES=ans_names,
                              check=FALSE)
}

setAs("Ranges", "PartitioningByEnd",
    function(from)
    {
        ans <- PartitioningByEnd(end(from), names=names(from))
        if (!identical(start(ans), start(from)))
            stop(wmsg("the Ranges object to coerce does not represent ",
                      "a partitioning"))
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

setMethod("end", "PartitioningByWidth", function(x) cumsum(width(x)))

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
    if (S4Vectors:::anyMissingOrOutside(width(x), 0L))
        return("the widths cannot be NAs or negative")
    if (!is.null(names(width(x))))
        return("the widths should not be named")
    NULL
}

setValidity2("PartitioningByWidth", .valid.PartitioningByWidth)

.numeric2width <- function(x=integer(0), NG=NULL)
{
    if (!is.integer(x))
        x <- as.integer(x)
    if (S4Vectors:::anyMissingOrOutside(x, 0L))
        stop(wmsg("when 'x' is an integer vector, ",
                  "it cannot contain NAs or negative values"))
    if (is.null(NG))
        return(x)
    ## When 'NG' (number of groups) is supplied, then 'x' is considered
    ## to represent the group assignment of a collection of 'length(x)'
    ## objects. Therefore the values in 'x' must be >= 1 and <= 'NG'.
    ## ADDITIONALLY, 'x' must be *sorted* (not strictly) so it can be
    ## reconstructed from the object returned by PartitioningByWidth()
    ## by doing togroup() on that object.
    if (S4Vectors:::isNotSorted(x))
        stop(wmsg("when 'x' is an integer vector, it must be sorted"))
    if (!isSingleNumber(NG))
        stop(wmsg("'NG' must be either NULL or a single integer"))
    if (!is.integer(NG))
        NG <- as.integer(NG)
    NO <- length(x)  # nb of objects
    if (NG == 0L) {
        if (NO != 0L)
            stop(wmsg("when 'NG' is 0, 'x' must be of length 0"))
    } else {
        ## 'x' is expected to be non-decreasing and with values >= 1
        ## and <= 'NG'.
        x <- tabulate(x, nbins=NG)
        ## 'sum(x)' is guaranteed to be <= 'NO'.
        if (sum(x) != NO)
            stop(wmsg("when 'NG' is supplied, values in 'x' must ",
                      "be >= 1 and <= 'NG'"))
    }
    x
}

PartitioningByWidth <- function(x=integer(0), NG=NULL, names=NULL)
{
    if (is(x, "List") || is.list(x)) {
        if (!is.null(NG))
            warning(wmsg("'NG' argument is ignored when 'x' ",
                         "is a list-like object"))
        if (is(x, "PartitioningByWidth")) {
            if (!is.null(names))
                names(x) <- names
            return(x)
        }
        x_names <- names(x)
        ans_width <- elementNROWS(x)
    } else {
        if (!is.numeric(x))
            stop(wmsg("'x' must be either a list-like object or ",
                      "a vector of non-NA non-negative integers"))
        x_names <- names(x)
        ans_width <- .numeric2width(x, NG)
    }
    ans_names <- .prepare_Partitioning_names(names, length(ans_width),
                                             NG, x_names)
    new2("PartitioningByWidth", width=unname(ans_width), NAMES=ans_names,
                                check=FALSE)
}

setAs("Ranges", "PartitioningByWidth",
    function(from)
    {
        ans <- PartitioningByWidth(width(from), names(from))
        if (!identical(start(ans), start(from)))
            stop(wmsg("the Ranges object to coerce does not represent ",
                      "a partitioning"))
        ans
    }
)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### PartitioningMap contains PartitioningByEnd and one additional slot,
### 'mapOrder', to specify a different order. This object is used by the 
### pack() function in GenomicFiles and is put in @partitioning of a 
### GRangesList of pack()ed ranges. 'mapOrder' records the order of the
### unpacked() ranges. 
### 

setClass("PartitioningMap",
    contains="PartitioningByEnd",
    representation(
        mapOrder="integer"
    ),
    prototype(
        mapOrder=integer()
    )
)

setGeneric("mapOrder", function(x) standardGeneric("mapOrder"))
setMethod("mapOrder", "PartitioningMap", function(x) x@mapOrder)

.valid.PartitioningMap <- function(x)
{
    if (length(x) == 0L)
        return(NULL)
    if (S4Vectors:::anyMissing(mapOrder(x)))
        return("mapOrder cannot contain NA values")
    if (any(mapOrder(x) < 0L))
        return("mapOrder values cannot be negative")
    if (!is.null(names(mapOrder(x))))
        return("the mapOrder should not be named")

    if (length(maporder <- mapOrder(x))) {
        maxorder <- max(maporder)
        if (max(maporder) > max(end(x)))
            return("max mapOrder value must be == max(end(object))")
    }
    NULL
}

setValidity2("PartitioningMap", .valid.PartitioningMap)

PartitioningMap <- function(x=integer(), mapOrder=integer(), ...)
    new("PartitioningMap", PartitioningByEnd(x=x), mapOrder=mapOrder, ...)

setAs("PartitioningByEnd", "PartitioningMap",
    function(from)
        new("PartitioningMap", from, mapOrder=numeric())
)

setMethod("show", "PartitioningMap", 
    function(object)
    {
        cat(class(object), " of length ", length(object), "\n")
        cat("mapOrder: ", mapOrder(object), "\n")
        print(PartitioningByEnd(object))
    }
) 

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### findOverlaps()
###

### A simple findOverlaps method that doesn't use NCList but works only
### on a subject with *adjacent* ranges sorted non-decreasingly.
### Can be 30% faster or more than the real findOverlaps() (NCList-based)
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
        stop(wmsg("'query' must be a Ranges object"))
    if (!is(subject, "Partitioning"))
        stop(wmsg("'subject' must be a Partitioning object"))
    if (!isTRUEorFALSE(hit.empty.query.ranges) ||
        !isTRUEorFALSE(hit.empty.subject.ranges))
        stop(wmsg("'hit.empty.query.ranges' and 'hit.empty.subject.ranges' ",
                  "must be TRUE or FALSE"))
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
    q_hits <- rep.int(seq_along(q_start),
                      q_end2subject - q_start2subject + 1L)
    s_hits <- S4Vectors:::mseq(q_start2subject, q_end2subject)
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
    Hits(q_hits, s_hits, q_len, s_len, sort.by.query=TRUE)
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Old stuff (deprecated & defunct)
###

setMethod("togroup", "ANY",
    function(x, j=NULL)
    {
        msg <- wmsg(
            "Using togroup() on a ", class(x), " object is defunct. ",
            "Please use togroup(PartitioningByWidth(...)) instead."
        )
        .Defunct(msg=msg)
    }
)

