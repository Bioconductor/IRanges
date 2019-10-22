### =========================================================================
### IPosRanges objects
### -------------------------------------------------------------------------
###
### The ranges in an IPosRanges derivative are closed, one-dimensional
### intervals with integer end points and on the domain of integers.
###
### The direct IPosRanges subclasses defined in the IRanges package are:
### IRanges, IPos, NCList, and GroupingRanges.


setClass("IPosRanges",
    contains="IntegerRanges",
    representation("VIRTUAL")
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Validity.
###

### The checking of the names(x) is taken care of by the validity method for
### Vector objects.
setValidity2("IPosRanges", validate_Ranges)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### updateObject()
###
### Value of elementType slot has changed from "integer" to "ANY" for
### IPosRanges objects in IRanges 2.13.22 (Bioc 3.7). It will soon change
### again to "StitchedIPos".
###

setMethod("updateObject", "IPosRanges",
    function(object, ..., verbose=FALSE)
    {
        target <- new(class(object))@elementType
        current <- object@elementType
        if (identical(target, current)) {
            if (verbose)
                message("[updateObject] Internal representation of ",
                        class(object), " object is current.\n",
                        "[updateObject] Nothing to update.")
        } else {
            if (verbose)
                message("[updateObject] elementType slot of ", class(object),
                        " object should be set to \"", target, "\",\n",
                        "[updateObject] not to \"", current, "\".\n",
                        "[updateObject] Updating it ... ", appendLF=FALSE)
            object@elementType <- target
            if (verbose)
                message("OK")
        }

        callNextMethod()
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion.
###

### Propagate the names.
setMethod("as.character", "IPosRanges",
    function(x)
    {
        if (length(x) == 0L)
            return(setNames(character(0), names(x)))
        x_start <- start(x)
        x_end <- end(x)
        ans <- paste0(x_start, "-", x_end)
        idx <- which(x_start == x_end)
        ans[idx] <- as.character(x_start)[idx]
        names(ans) <- names(x)
        ans
    }
)

### The as.factor() generic doesn't have the ... argument so this method
### cannot support the 'ignore.strand' argument.
setMethod("as.factor", "IPosRanges",
    function(x)
        factor(as.character(x), levels=as.character(sort(unique(x))))
)

setMethod("as.matrix", "IPosRanges",
    function(x, ...)
        matrix(data=c(start(x), width(x)), ncol=2,
               dimnames=list(names(x), NULL))
)

### S3/S4 combo for as.data.frame.IPosRanges
.as.data.frame.IPosRanges <- function(x, row.names=NULL, optional=FALSE)
{
    if (!identical(optional, FALSE))
        warning(wmsg("'optional' argument was ignored"))
    ans <- data.frame(start=start(x),
                      end=end(x),
                      width=width(x),
                      row.names=row.names,
                      check.names=FALSE,
                      stringsAsFactors=FALSE)
    ans$names <- names(x)
    x_mcols <- mcols(x, use.names=FALSE)  # can be NULL!
    if (!is.null(x_mcols))
        ans <- cbind(ans, as.data.frame(x_mcols, optional=TRUE))
    ans
}
as.data.frame.IPosRanges <- function(x, row.names=NULL, optional=FALSE, ...)
    .as.data.frame.IPosRanges(x, row.names=NULL, optional=FALSE, ...)
setMethod("as.data.frame", "IPosRanges", .as.data.frame.IPosRanges)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### show()
###

.from_IPosRanges_to_naked_character_matrix_for_display <- function(x)
{
    x_len <- length(x)
    x_mcols <- mcols(x, use.names=FALSE)
    x_nmc <- if (is.null(x_mcols)) 0L else ncol(x_mcols)
    ans <- cbind(start=as.character(start(x)),
                 end=as.character(end(x)),
                 width=as.character(width(x)))
    if (x_nmc > 0L) {
        tmp <- as.data.frame(lapply(x_mcols, showAsCell), optional=TRUE)
        ans <- cbind(ans, `|`=rep.int("|", x_len), as.matrix(tmp))
    }
    ans
}

show_IPosRanges <- function(x, margin="", print.classinfo=FALSE)
{
    x_class <- class(x)
    x_len <- length(x)
    x_mcols <- mcols(x, use.names=FALSE)
    x_nmc <- if (is.null(x_mcols)) 0L else ncol(x_mcols)
    cat(x_class, " object with ",
        x_len, " ", ifelse(x_len == 1L, "range", "ranges"),
        " and ",
        x_nmc, " metadata ", ifelse(x_nmc == 1L, "column", "columns"),
        ":\n", sep="")
    ## S4Vectors:::makePrettyMatrixForCompactPrinting() assumes that 'x' is
    ## subsettable but not all IPosRanges objects necesseraly are (and if they
    ## are, subsetting them could be costly). However IRanges objects are
    ## assumed to be subsettable so if 'x' is not one then we turn it into
    ## one (this coercion is expected to work on any IPosRanges object).
    if (!is(x, "IRanges"))
        x <- as(x, "IRanges", strict=FALSE)
    out <- S4Vectors:::makePrettyMatrixForCompactPrinting(x,
               .from_IPosRanges_to_naked_character_matrix_for_display)
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

setMethod("show", "IPosRanges",
    function(object)
        show_IPosRanges(object, margin="  ", print.classinfo=TRUE)
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Subsetting
###

### Avoid infinite recursion that we would otherwise get:
###   IRanges(1:4, 8)[[1]]
###   # Error: C stack usage  7969636 is too close to the limit
setMethod("getListElement", "IPosRanges",
    function(x, i, exact=TRUE)
    {
        ## A temporary situation
        stop(wmsg(class(x), " objects don't support [[, as.list(), ",
                  "lapply(), or unlist() at the moment"))
    }
)

