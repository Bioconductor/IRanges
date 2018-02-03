### =========================================================================
### Ranges objects
### -------------------------------------------------------------------------
###
### Ranges is a virtual class that serves as the parent class for any class
### that represents a vector of ranges. The core Ranges API consists of the
### start(), end() and width() getters. All 3 getters must return an integer
### vector parallel to the object and with no NAs. In addition the 3 vectors
### must satisfy the 2 following properties:
###
###   (1) all(width(x) >= 0) is TRUE
###   (2) all(start(x) + width(x) - 1L == end(x)) is TRUE
###
### Direct Ranges subclasses are: IntegerRanges, Views, GenomicRanges, and
### GenomicAlignments.
###

setClass("Ranges", contains="Vector", representation("VIRTUAL"))

setClass("Pos", contains="Ranges", representation("VIRTUAL"))


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Some default methods
###

### Note that the 3 default methods below implement a circular relationship.
### So Ranges subclasses must overwrite at least 2 of them!
setMethod("start", "Ranges", function(x, ...) {1L - width(x) + end(x)})
setMethod("end", "Ranges", function(x, ...) {width(x) - 1L + start(x)})
setMethod("width", "Ranges", function(x) {end(x) - start(x) + 1L})

setMethod("length", "Ranges", function(x) length(start(x)))

### Pos subclasses only need to implement a "pos" method.
setMethod("start", "Pos", function(x) pos(x))
setMethod("end", "Pos", function(x) pos(x))
setMethod("width", "Pos", function(x) rep.int(1L, length(x)))

setMethod("elementNROWS", "Ranges",
    function(x) setNames(width(x), names(x))
)

setGeneric("mid", function(x, ...) standardGeneric("mid"))

setMethod("mid", "Ranges",
    function(x) start(x) + as.integer((width(x) - 1) / 2)
)

### A Ranges object is considered empty iff all its ranges are empty.
setMethod("isEmpty", "Ranges", function(x) all(width(x) == 0L))


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

