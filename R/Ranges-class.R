### =========================================================================
### Ranges objects
### -------------------------------------------------------------------------
###
### Ranges is a virtual class that serves as the base for all range containers
### Conceptually Ranges are closed, one-dimensional intervals with integer end
### points and on the domain of integers.
###

setClass("Ranges",
    contains=c("IntegerList", "SmartSubscript"),
    representation("VIRTUAL")
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Getters/setters.
###

setMethod("length", "Ranges", function(x) length(start(x)))

### Without this definition, we inherit the method for Vector objects
### which is very inefficient on Ranges objects!
setMethod("elementLengths", "Ranges", function(x) setNames(width(x), names(x)))

### The "start" and "end" generics are defined in the stats package.
setGeneric("width", function(x) standardGeneric("width"))

### The 3 default methods below provide a formalization of the relationship
### between the starts/widths/ends of a Ranges object. Of course Ranges
### subclasses need to implement at least 2 of them!
### Note that when width(x)[i] is 0, then end(x)[i] is start(x)[i] - 1
setMethod("start", "Ranges", function(x, ...) {1L - width(x) + end(x)})
setMethod("width", "Ranges", function(x) {end(x) - start(x) + 1L})
setMethod("end", "Ranges", function(x, ...) {width(x) - 1L + start(x)})

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


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Validity.
###

### The checking of the names(x) is taken care of by the validity method for
### Vector objects.
.valid.Ranges <- function(x)
{
    x_start <- start(x)
    x_end <- end(x)
    x_width <- width(x)
    validity_failures <- .Call2("valid_Ranges",
                                x_start, x_end, x_width,
                                PACKAGE="IRanges");
    if (!is.null(validity_failures))
        return(validity_failures)
    if (!(is.null(names(x_start)) &&
          is.null(names(x_end)) &&
          is.null(names(x_width))))
        return(paste0("'start(x)', 'end(x)', and 'width(x)' ",
                      "cannot have names on them"))
    NULL
}

setValidity2("Ranges", .valid.Ranges)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion.
###

setMethod("as.matrix", "Ranges",
    function(x, ...)
        matrix(data=c(start(x), width(x)), ncol=2,
               dimnames=list(names(x), NULL))
)

### S3/S4 combo for as.data.frame.Ranges
as.data.frame.Ranges <- function(x, row.names=NULL, optional=FALSE, ...)
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
setMethod("as.data.frame", "Ranges", as.data.frame.Ranges)

setMethod("as.integer", "Ranges",
    function(x, ...) S4Vectors:::fancy_mseq(width(x), offset=start(x)-1L)
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### More stuff.
###
### TODO: Reorganize this
###

setMethod("unlist", "Ranges",
    function(x, recursive=TRUE, use.names=TRUE)
    {
        if (!identical(recursive, TRUE))
            stop("\"unlist\" method for Ranges objects ",
                 "does not support the 'recursive' argument")
        if (!isTRUEorFALSE(use.names))
            stop("'use.names' must be TRUE or FALSE")
        ans <- as.integer(x)  # 'ans' should have no names
        stopifnot(is.null(names(ans)))  # sanity check
        if (use.names && !is.null(names(x)))
            names(ans) <- rep.int(names(x), elementLengths(x))
        ans
    }
)

setMethod("getListElement", "Ranges",
    function(x, i, exact=TRUE)
    {
        i <- normalizeDoubleBracketSubscript(i, x, exact=exact,
                                             error.if.nomatch=TRUE)
        ans_shift <- start(x)[i] - 1L
        ans_length <- width(x)[i]
        seq_len(ans_length) + ans_shift
    }
)

setMethod("show", "Ranges",
    function(object)
    {
        nhead <- get_showHeadLines()
        ntail <- get_showTailLines()
        lo <- length(object)
        cat(class(object), " of length ", lo, "\n", sep="")
        if (lo == 0L)
            return(NULL)
        if (lo < (nhead + ntail + 1L)) {
            showme <-
              as.data.frame(object,
                            row.names=paste0("[", seq_len(lo), "]"))
        } else {
            showme <-
              data.frame(start=.sketch(start(object), nhead, ntail),
                         end=.sketch(end(object), nhead, ntail),
                         width=.sketch(width(object), nhead, ntail),
                         row.names=.sketch(start(object), nhead, ntail, TRUE),
                         check.rows=TRUE,
                         check.names=FALSE,
                         stringsAsFactors=FALSE)
            NAMES <- names(object)
            if (!is.null(NAMES))
                showme$names <- .sketch(NAMES, nhead, ntail)
        }
        show(showme)
    }
)

.sketch <- function(x, nhead, ntail, rownames=FALSE)
{
    len <- length(x)
    p1 <- ifelse (nhead == 0, 0L, 1L)
    p2 <- ifelse (ntail == 0, 0L, ntail-1L)
    s1 <- s2 <- character(0) 
 
    if (rownames) {
        if (nhead > 0) 
            s1 <- paste0("[", p1:nhead, "]")
        if (ntail > 0) 
            s2 <- paste0("[", (len-p2):len, "]")
    } else { 
        if (nhead > 0) 
            s1 <- paste0(as.character(x[p1:nhead])) 
        if (ntail > 0) 
            s2 <- paste0(as.character(x[(len-p2):len])) 
    }
    c(s1, "...", s2)
}

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
### TODO: "extractROWS" and most of the Ranges endomorphisms are only
### defined for IRanges objects. Need to fix up the update mechanism, so that
### they can be defined on Ranges. "extractROWS" and other endomorphisms
### are currently implemented as wrappers that coerce to IRanges, which is not
### efficient so not a general, long-term solution.

setMethod("extractROWS", "Ranges",
    function(x, i)
    {
        as(callGeneric(as(x, "IRanges"), i), class(x))
    }
)

