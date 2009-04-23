### =========================================================================
### Partitioning and IPartitioning objects
### -------------------------------------------------------------------------
###
### A Partitioning object is a Ranges object where the ranges are adjacent
### starting at 1. Hence it covers an interval of the form 1:N with no
### overlap.
### The IPartitioning container is a concrete implementation of the
### Partitioning VIRTUAL class that uses a compact internal representation.
###

setClass("Partitioning", contains=c("Ranges", "VIRTUAL"))

setClass("IPartitioning",
    contains="Partitioning",
    representation(
        end="integer",
        NAMES="characterORNULL"  # R doesn't like @names !!
    ),
    prototype(
        end=integer(),
        NAMES=NULL
    )
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Accessor-like methods.
###

setMethod("end", "IPartitioning", function(x) x@end)

setMethod("length", "IPartitioning", function(x) length(end(x)))

setMethod("start", "IPartitioning",
    function(x)
    {
        if (length(x) == 0L)
            return(integer())
        c(1L, end(x)[-length(x)] + 1L)
    }
)

setMethod("width", "IPartitioning", function(x) diff(c(0L, end(x))))

setMethod("names", "IPartitioning", function(x) x@NAMES)

setReplaceMethod("names", "IPartitioning",
    function(x, value)
    {
        if (!is(value, "characterORNULL"))
            stop("'value' must a character vector or NULL")
        ## This works only "by chance" i.e. just because, like IRanges, the
        ## names of a IPartitioning object are stored in a slot called "NAMES",
        ## which is what `unsafe.names<-` expects.
        unsafe.names(x) <- value
        x
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Validity.
###

.valid.IPartitioning.end <- function(x)
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

.valid.IPartitioning.names <- function(x)
{
    if (is.null(names(x)))
        return(NULL)
    if (!is.character(names(x)))
        return("the names must be a character vector or NULL")
    if (length(names(x)) != length(x))
        return("number of names and number of elements differ")
    NULL
}

.valid.IPartitioning <- function(x)
{
    c(.valid.IPartitioning.end(x),
      .valid.IPartitioning.names(x))
}

setValidity2("IPartitioning", .valid.IPartitioning)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Constructor.
###

IPartitioning <- function(end=integer(), names=NULL)
{
    if (!is.numeric(end))
        stop("'end' must contain integer values")
    if (!is.integer(end))
        end <- as.integer(end)
    new("IPartitioning", end=end, NAMES=names)
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion.
###

setAs("Ranges", "IPartitioning",
    function(from) IPartitioning(end(from), names(from))
)

