### Ranges is a virtual class that serves as the base for all range containers
### Conceptually Ranges are closed, one-dimensional intervals with integer end
### points and on the domain of integers.

setClass("Ranges", contains = "VIRTUAL")


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The Ranges API (work in progress):
###
###   Various:
###     length
###     start, width, end, names
###     start<-, width<-, end<-, names<-
###     isEmpty
###     as.matrix, as.data.frame
###     duplicated
###     show
###     split, c
###
###   Endomorphisms:
###     update
###     [, [<-, rep
###     shift, restrict, narrow, reduce, gaps
###
###   Binary set operations:
###     union, intersect, setdiff
###     overlap
###
### Note that, except for some default methods provided below (and implemented
### as formal algorithms), Ranges subclasses need to implement their own
### methods.
###

### The "start" and "end" generics are defined in the stats package.
setGeneric("width", function(x) standardGeneric("width"))

### Note that when width(x)[i] is 0, then end(x)[i] is start(x)[i] - 1
setMethod("end", "Ranges", function(x, ...) {start(x) + width(x) - 1L})

setGeneric("start<-", signature="x",
    function(x, check=TRUE, value) standardGeneric("start<-")
)

setGeneric("width<-", signature="x",
    function(x, check=TRUE, value) standardGeneric("width<-")
)

setGeneric("end<-", signature="x",
    function(x, check=TRUE, value) standardGeneric("end<-")
)

### TODO: default implementation using width()
setGeneric("isEmpty", function(x) standardGeneric("isEmpty"))

setMethod("as.matrix", "Ranges",
    function(x, ...)
        matrix(data=c(start(x), width(x)), ncol=2, dimnames=list(names(x), NULL))
)

setMethod("as.data.frame", "Ranges",
    function(x, row.names=NULL, optional=FALSE, ...)
    {
        if (!(is.null(row.names) || is.character(row.names)))
            stop("'row.names'  must be NULL or a character vector")
        ans <- data.frame(start=start(x),
                          end=end(x),
                          width=width(x),
                          row.names=row.names,
                          check.rows=TRUE,
                          check.names=FALSE,
                          stringsAsFactors=FALSE)
        ans$names <- names(x)
        ans
    }
)

### Note that this default method is very inefficient so efficient methods for
### the Ranges subclasses need to be implemented.
setMethod("duplicated", "Ranges",
    function(x, incomparables=FALSE, ...)
    {
        duplicated(data.frame(start=start(x),
                              width=width(x),
                              check.names=FALSE,
                              stringsAsFactors=FALSE))
    }
)

setReplaceMethod("[", "Ranges",
    function(x, i, j,..., value)
        stop("attempt to modify the value of a ", class(x), " instance")
)

setMethod("rep", "Ranges",
    function(x, times)
        x[rep.int(seq_len(length(x)), times)]
)

setGeneric("shift", signature="x",
    function(x, shift, use.names=TRUE) standardGeneric("shift")
)

setGeneric("restrict", signature="x",
    function(x, start=NA, end=NA, keep.all.ranges=FALSE, use.names=TRUE)
        standardGeneric("restrict")
)

setGeneric("narrow", signature="x",
    function(x, start=NA, end=NA, width=NA, use.names=TRUE)
        standardGeneric("narrow")
)

setGeneric("reduce", signature="x",
    function(x, with.inframe.attrib=FALSE) standardGeneric("reduce")
)

setGeneric("gaps", signature="x",
    function(x, start=NA, end=NA) standardGeneric("gaps")
)

setMethod("union", c("Ranges", "Ranges"),
    function(x, y)
    {
        reduce(c(x, y))
    }
)

setMethod("intersect", c("Ranges", "Ranges"),
    function(x, y)
    {
        start <- min(c(start(x), start(y)))
        end <- max(c(end(x), end(y)))
        setdiff(x, gaps(y, start, end))
    }
)

setMethod("setdiff", c("Ranges", "Ranges"),
    function(x, y)
    {
        start <- min(c(start(x), start(y)))
        end <- max(c(end(x), end(y)))
        gaps(union(gaps(x, start, end), y), start, end)
    }
)

## Find objects in the index that overlap those in a query set
setGeneric("overlap", signature=c("object", "query"),
    function(object, query, multiple = TRUE, ...) standardGeneric("overlap")
)

setMethod("overlap", c("Ranges", "missing"),
    function(object, query, multiple = TRUE) overlap(object, object, multiple)
)

