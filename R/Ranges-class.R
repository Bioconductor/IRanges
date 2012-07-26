### =========================================================================
### Ranges objects
### -------------------------------------------------------------------------
###
### Ranges is a virtual class that serves as the base for all range containers
### Conceptually Ranges are closed, one-dimensional intervals with integer end
### points and on the domain of integers.
###

setClass("Ranges", contains="IntegerList", representation("VIRTUAL"))


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The Ranges API (work still very much in progress):
###
###   Basic get/set methods:
###     length
###     start, width, end, names
###     start<-, width<-, end<-, names<-
###
###   More basic stuff:
###     as.matrix, as.data.frame
###     as.integer, unlist
###     show
###
###   Testing a Ranges object:
###     isEmpty
###     isNormal, whichFirstNotNormal
###
###   Core endomorphisms:
###     update
###     [, [<-, rep
###

setMethod("length", "Ranges", function(x) length(start(x)))

### Without this definition, we inherit the method for Vector objects
### which is very inefficient on Ranges objects!
setMethod("elementLengths", "Ranges", function(x) width(x))

### The "start" and "end" generics are defined in the stats package.
setGeneric("width", function(x) standardGeneric("width"))

### The 3 default methods below provide a formalization of the relationship
### between the starts/widths/ends of a Ranges object. Of course Ranges
### subclasses need to implement at least 2 of them!
### Note that when width(x)[i] is 0, then end(x)[i] is start(x)[i] - 1
setMethod("start", "Ranges", function(x, ...) {end(x) - width(x) + 1L})
setMethod("width", "Ranges", function(x) {end(x) - start(x) + 1L})
setMethod("end", "Ranges", function(x, ...) {start(x) + width(x) - 1L})

setGeneric("mid", function(x, ...) standardGeneric("mid"))
setMethod("mid", "Ranges", function(x) start(x) + as.integer((width(x)-1) / 2))

setGeneric("start<-", signature="x",
    function(x, check=TRUE, value) standardGeneric("start<-")
)

setGeneric("width<-", signature="x",
    function(x, check=TRUE, value) standardGeneric("width<-")
)

setGeneric("end<-", signature="x",
    function(x, check=TRUE, value) standardGeneric("end<-")
)

setMethod("update", "Ranges",
    function(object, ...)
        as(update(as(object, "IRanges"), ...), class(object))
)

setMethod("as.matrix", "Ranges",
    function(x, ...)
        matrix(data=c(start(x), width(x)), ncol=2,
               dimnames=list(names(x), NULL))
)

setMethod("as.data.frame", "Ranges",
    function(x, row.names=NULL, optional=FALSE, ...)
    {
        if (!(is.null(row.names) || is.character(row.names)))
            stop("'row.names' must be NULL or a character vector")
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

setMethod("as.integer", "Ranges",
    function(x, ...)
    {
        x <- x[width(x) > 0L]
        mseq(start(x), end(x))
    }
)

setMethod("unlist", "Ranges",
    function(x, recursive = TRUE, use.names = TRUE)
    {
        if (!missing(recursive))
            warning("'recursive' argument currently ignored")
        ans <- as.integer(x)
        if (use.names) {
            if (!is.null(names(x))) {
                nms <- rep.int(names(x), elementLengths(x))
                if (!is.null(names(ans)))
                    nms <- paste(nms, names(ans), sep = ".")
                names(ans) <- nms
            }
        }
        ans
    }
)

setMethod("[[", "Ranges",
    function(x, i, j, ...)
    {
        i <- checkAndTranslateDbleBracketSubscript(x, i)
        ans_shift <- start(x)[i] - 1L
        ans_length <- width(x)[i]
        seq_len(ans_length) + ans_shift
    }
)

setMethod("show", "Ranges",
    function(object)
    {
        lo <- length(object)
        cat(class(object), " of length ", lo, "\n", sep="")
        if (lo == 0L)
            return(NULL)
        if (lo < 20L) {
            showme <-
              as.data.frame(object,
                            row.names=paste("[", seq_len(lo), "]", sep=""))
        } else {
            sketch <- function(x)
              c(as.character(window(x, 1L, 9L)),
                "...",
                as.character(window(x, length(x)-8L, length(x))))
            showme <-
              data.frame(start=sketch(start(object)),
                         end=sketch(end(object)),
                         width=sketch(width(object)),
                         row.names=c(paste("[", 1:9, "]", sep=""), "...",
                                     paste("[", (lo-8L):lo, "]", sep="")),
                         check.rows=TRUE,
                         check.names=FALSE,
                         stringsAsFactors=FALSE)
            NAMES <- names(object)
            if (!is.null(NAMES))
                showme$names <- sketch(NAMES)
        }
        show(showme)
    }
)

setMethod("showAsCell", "Ranges",
    function(object)
    {
        if (length(object) == 0L)
            return(character(0))
        paste("[", format(start(object)), ", ", format(end(object)), "]",
              sep = "")
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Testing a Ranges object.
###

### A Ranges object is considered empty iff all its ranges are empty.
setMethod("isEmpty", "Ranges", function(x) all(width(x) == 0L))

setGeneric("isNormal", function(x) standardGeneric("isNormal"))

setMethod("isNormal", "Ranges",
    function(x)
    {
        all_ok <- all(width(x) >= 1L)
        if (length(x) >= 2)
            all_ok <- all_ok && all(start(x)[-1L] - end(x)[-length(x)] >= 2L)
        all_ok
    }
)

setGeneric("whichFirstNotNormal",
    function(x) standardGeneric("whichFirstNotNormal")
)

setMethod("whichFirstNotNormal", "Ranges",
    function(x)
    {
        is_ok <- width(x) >= 1L
        if (length(x) >= 2)
            is_ok <- is_ok & c(TRUE, start(x)[-1L] - end(x)[-length(x)] >= 2L)
        which(!is_ok)[1L]
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Core endomorphisms.
###
### TODO: "[" and most of the Ranges endomorphisms below are only defined for
### IRanges objects. Need to fix up the update mechanism, so that they can be
### defined on 'Ranges'. "[" and other endomorphisms below are currently
### implemented as wrappers that coerce to IRanges, which is not a general,
### long-term solution.

setMethod("[", "Ranges",
    function(x, i, j, ..., drop)
    {
        if (!missing(j) || length(list(...)) > 0L)
            stop("invalid subsetting")
        as(callGeneric(as(x, "IRanges"), i=i, ...), class(x))
    }
)
