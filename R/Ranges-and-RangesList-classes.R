### =========================================================================
### Ranges and RangesList objects
### -------------------------------------------------------------------------
###
### The Ranges class is a virtual class that serves as the parent class for
### any container that represents a vector of ranges. The core Ranges API
### consists of the start(), end() and width() getters. All 3 getters must
### return an integer vector parallel to the object and with no NAs. In
### addition the 3 vectors must satisfy the 2 following properties:
###
###   (1) all(width(x) >= 0) is TRUE
###   (2) all(start(x) + width(x) - 1L == end(x)) is TRUE
###
### The direct Ranges subclasses defined in the IRanges and GenomicRanges
### packages are: Pos, FWRanges, IntegerRanges, and GenomicRanges.
###
### The Ranges hierarchy:
### - showing the top-level classes only;
### - showing only classes defined in the IRanges package;
### - all classes showed in the diagram are virtual except IRanges, NCList,
###   IPos and IFWRanges (marked with an asterisk).
###
###                              Ranges
###                              ^  ^  ^
###                              |  |  |
###                              |  |   \-------------------|
###                             /    \------------|         |
###                 IntegerRanges                Pos     FWRanges
###                 ^           ^                 ^         ^
###                 |           |                 |         |
###                 |           |                 |         |
###               Views     IPosRanges            |         |
###                ^       ^ ^     ^^ ^           |         |
###               /       /  |     | \ \          |         |
###              .       /   |     |  \ \---------|         |
###                     /    |     |   \        IPos*       |
###                    /     |     |    \-------------------|
###                   /      |     |                    IFWRanges*
###                  /       |     |
###                 /        |     |
###             IRanges*     |     |
###                ^      NCList*  |
###                |              GroupingRanges
###                |                    ^
###                .                    |
###                                     .
###

### General ranges.
setClass("Ranges", contains="List", representation("VIRTUAL"))

### Positions (i.e. ranges of with 1).
setClass("Pos", contains="Ranges", representation("VIRTUAL"))

### Fixed-width ranges.
setClass("FWRanges", contains="Ranges", representation("VIRTUAL"))

### All ranges are on a single space.
### Direct IntegerRanges subclasses: Views, IPosRanges.
setClass("IntegerRanges", contains="Ranges", representation("VIRTUAL"))


setClass("RangesList",
    contains="List",
    representation("VIRTUAL"),
    prototype(elementType="Ranges")
)

setClass("SimpleRangesList",
    contains=c("RangesList", "SimpleList"),
    representation("VIRTUAL")
)

setClass("PosList",
    contains="RangesList",
    representation("VIRTUAL"),
    prototype(elementType="Pos")
)

setClass("SimplePosList",
    contains=c("PosList", "SimpleRangesList"),
    representation("VIRTUAL")
)

setClass("FWRangesList",
    contains="RangesList",
    representation("VIRTUAL"),
    prototype(elementType="FWRanges")
)

setClass("SimpleFWRangesList",
    contains=c("FWRangesList", "SimpleRangesList"),
    representation("VIRTUAL")
)

setClass("IntegerRangesList",
    contains="RangesList",
    representation("VIRTUAL"),
    prototype(elementType="IntegerRanges")
)

setClass("SimpleIntegerRangesList",
    contains=c("IntegerRangesList", "SimpleRangesList"),
    representation("VIRTUAL")
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Methods for Ranges derivatives
###

### Note that the 3 default methods below implement a circular relationship.
### So Ranges subclasses must overwrite at least 2 of them!
setMethod("start", "Ranges", function(x, ...) {1L - width(x) + end(x)})
setMethod("end", "Ranges", function(x, ...) {width(x) - 1L + start(x)})
setMethod("width", "Ranges", function(x) {end(x) - start(x) + 1L})

setMethod("length", "Ranges", function(x) length(start(x)))

setGeneric("mid", function(x, ...) standardGeneric("mid"))

setMethod("mid", "Ranges",
    function(x) start(x) + as.integer((width(x) - 1) / 2)
)

### A Ranges object is considered empty iff all its ranges are empty.
setMethod("isEmpty", "Ranges", function(x) all(width(x) == 0L))


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Ranges extends List
###

setMethod("elementNROWS", "Ranges",
    function(x) setNames(width(x), names(x))
)

unlist_as_integer <- function(x)
{
    stopifnot(is(x, "Ranges"))
    if (is(x, "Pos"))
        return(pos(x))
    S4Vectors:::fancy_mseq(width(x), offset=start(x)-1L)
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Methods for Pos derivatives
###

### Pos subclasses only need to implement a "pos" method.
setMethod("start", "Pos", function(x) pos(x))
setMethod("end", "Pos", function(x) pos(x))
setMethod("width", "Pos", function(x) rep.int(1L, length(x)))

### Pos derivatives don't accept names.
setMethod("names", "Pos", function(x) NULL)

setReplaceMethod("names", "Pos",
    function(x, value)
    {
        if (!is.null(value))
            stop(class(x), " objects don't accept names")
        x
    }
)

setMethod("as.integer", "Pos", function(x) pos(x))


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Methods for FWRanges derivatives
###

### Overwrite default method with optimized method for FWRanges objects
### storing width 1 ranges.
setMethod("end", "FWRanges",
    function(x, ...)
    {
        if (x@width == 1L)
            return(start(x))
        callNextMethod()
    }
)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Methods for IntegerRanges derivatives
###

#setGeneric("isDisjointAndSorted",
#    function(x, ...) standardGeneric("isDisjointAndSorted")
#)

#setMethod("isDisjointAndSorted", "IntegerRanges",
#    function(x)
#    {
#        x_start <- start(x)
#        x_len <- length(x_start)
#        if (x_len <= 1L)
#            return(TRUE)
#        x_end <- end(x)
#        all(x_start[-1L] > x_end[-x_len])
#    }
#)

### Being "normal" is stronger that being "disjoint and sorted".
setGeneric("isNormal", function(x, ...) standardGeneric("isNormal"))

setMethod("isNormal", "IntegerRanges",
    function(x)
    {
        x_start <- start(x)
        x_end <- end(x)
        if (any(x_end < x_start))
            return(FALSE)
        x_len <- length(x_start)
        if (x_len <= 1L)
            return(TRUE)
        all(x_start[-1L] > x_end[-x_len] + 1L)
    }
)

setGeneric("whichFirstNotNormal",
    function(x) standardGeneric("whichFirstNotNormal")
)

setMethod("whichFirstNotNormal", "IntegerRanges",
    function(x)
    {
        is_ok <- width(x) >= 1L
        if (length(x) >= 2)
            is_ok <- is_ok & c(TRUE, start(x)[-1L] - end(x)[-length(x)] >= 2L)
        which(!is_ok)[1L]
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Methods for RangesList derivatives
###

setMethod("start", "RangesList",
    function(x) as(lapply(x, start), "SimpleIntegerList")
)

setMethod("end", "RangesList",
    function(x) as(lapply(x, end), "SimpleIntegerList")
)

setMethod("width", "RangesList",
    function(x) as(lapply(x, width), "SimpleIntegerList")
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Methods for PosList derivatives
###

setMethod("pos", "PosList",
    function(x) as(lapply(x, pos), "SimpleIntegerList")
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Validity
###

validate_Ranges <- function(x)
{
    x_start <- start(x)
    x_end <- end(x)
    x_width <- width(x)
    validity_failures <- .Call2("Ranges_validate",
                                x_start, x_end, x_width,
                                PACKAGE="IRanges")
    if (!is.null(validity_failures))
        return(validity_failures)
    if (!(is.null(names(x_start)) &&
          is.null(names(x_end)) &&
          is.null(names(x_width))))
        return(wmsg("'start(x)', 'end(x)', and 'width(x)' ",
                    "cannot have names on them"))
    NULL
}

validate_Pos <- function(x)
{
    x_width <- width(x)
    if (!all(x_width == 1L))
        return(wmsg())
    x_pos <- pos(x)
    x_start <- start(x)
    if (!all(x_pos == x_start))
        return(wmsg())
    NULL
}

validate_FWRanges <- function(x)
{
    x_width <- width(x)
    # TODO: validate_FWRanges(IFWRanges()) must pass this validity check. At
    #       present, it will error because integer(0)[[1L]] is an error.
    if (!all(x_width == x_width[[1L]]))
        return(wmsg())
    NULL
}

