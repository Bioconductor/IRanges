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
### Getters/setters.
###

setMethod("length", "Ranges", function(x) length(width(x)))

### Without this definition, we inherit the method for Vector objects
### which is very inefficient on Ranges objects!
setMethod("elementNROWS", "Ranges", function(x) setNames(width(x), names(x)))

### The 3 default methods below provide a formalization of the relationship
### between the starts/widths/ends of a Ranges object. Of course Ranges
### subclasses need to implement at least 2 of them!
### Note that when width(x)[i] is 0, then end(x)[i] is start(x)[i] - 1
setMethod("start", "Ranges", function(x, ...) {1L - width(x) + end(x)})
setMethod("width", "Ranges", function(x) {end(x) - start(x) + 1L})
setMethod("end", "Ranges", function(x, ...) {width(x) - 1L + start(x)})

setGeneric("mid", function(x, ...) standardGeneric("mid"))
setMethod("mid", "Ranges", function(x) start(x) + as.integer((width(x)-1) / 2))

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
                                PACKAGE="IRanges")
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

.as.data.frame.Ranges <- function(x, row.names=NULL, optional=FALSE, ...)
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
setMethod("as.data.frame", "Ranges", .as.data.frame.Ranges)

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
            names(ans) <- rep.int(names(x), elementNROWS(x))
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

.make_naked_matrix_from_Ranges <- function(x)
{
    x_len <- length(x)
    x_mcols <- mcols(x)
    x_nmc <- if (is.null(x_mcols)) 0L else ncol(x_mcols)
    ans <- cbind(start=as.character(start(x)),
                 end=as.character(end(x)),
                 width=as.character(width(x)))
    if (x_nmc > 0L) {
        tmp <- do.call(data.frame, c(lapply(x_mcols, showAsCell),
                                     list(check.names=FALSE)))
        ans <- cbind(ans, `|`=rep.int("|", x_len), as.matrix(tmp))
    }
    ans
}

showRanges <- function(x, margin="", print.classinfo=FALSE)
{
    x_class <- class(x)
    x_len <- length(x)
    x_mcols <- mcols(x)
    x_nmc <- if (is.null(x_mcols)) 0L else ncol(x_mcols)
    cat(x_class, " object with ",
        x_len, " ", ifelse(x_len == 1L, "range", "ranges"),
        " and ",
        x_nmc, " metadata ", ifelse(x_nmc == 1L, "column", "columns"),
        ":\n", sep="")
    ## S4Vectors:::makePrettyMatrixForCompactPrinting() assumes that 'x' is
    ## subsettable but not all Ranges objects are (and if they are,
    ## subsetting them could be costly). However IRanges objects are assumed
    ## to be subsettable so if 'x' is not one then we turn it into one (this
    ## coercion is expected to work on any Ranges object).
    if (!is(x, "IRanges"))
        x <- as(x, "IRanges", strict=FALSE)
    out <- S4Vectors:::makePrettyMatrixForCompactPrinting(x,
               .make_naked_matrix_from_Ranges)
    if (print.classinfo) {
        .COL2CLASS <- c(
            start="integer",
            end="integer",
            width="integer"
        )
        classinfo <-
            S4Vectors:::makeClassinfoRowForCompactPrinting(x, .COL2CLASS)
        ## A sanity check, but this should never happen!
        stopifnot(identical(colnames(classinfo), colnames(out)))
        out <- rbind(classinfo, out)
    }
    if (nrow(out) != 0L)
        rownames(out) <- paste0(margin, rownames(out))
    ## We set 'max' to 'length(out)' to avoid the getOption("max.print")
    ## limit that would typically be reached when 'showHeadLines' global
    ## option is set to Inf.
    print(out, quote=FALSE, right=TRUE, max=length(out))
}

setMethod("show", "Ranges",
    function(object)
        showRanges(object, margin="  ", print.classinfo=TRUE)
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
### isEmpty() and isNormal()
###
### All of them test a Ranges object as a whole and return a single TRUE or
### FALSE.
###

### A Ranges object is considered empty iff all its ranges are empty.
setMethod("isEmpty", "Ranges", function(x) all(width(x) == 0L))

setGeneric("isNormal", function(x, ...) standardGeneric("isNormal"))

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
### Subsetting
###
### TODO: "extractROWS" and most of the Ranges endomorphisms are only
### defined for IRanges objects. Need to fix up the update mechanism, so that
### they can be defined on Ranges. "extractROWS" and other endomorphisms
### are currently implemented as wrappers that coerce to IRanges, which is not
### efficient so not a general, long-term solution.

setMethod("extractROWS", "Ranges",
    function(x, i)
        as(callNextMethod(as(x, "IRanges", strict=FALSE), i), class(x))
)

