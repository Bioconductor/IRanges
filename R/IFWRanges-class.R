### =========================================================================
### IFWRanges objects
### -------------------------------------------------------------------------
###
### The IFWRanges class is a simple container for storing a vector of integer
### fixed-width ranges.
###

setClass("IFWRanges",
    contains=c("FWRanges", "IPosRanges"),
    representation(
        start="integer",
        width="integer",
        NAMES="character_OR_NULL"  # R doesn't like @names !!
    )
)

# TODO: Comment copied from IPos-class.R; is this really that expensive for
#       IFWRanges objects?
### Expensive! (and not needed)
#setValidity2("IFWRanges", validate_FWRanges)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### parallelSlotNames()
###

### Combine the new parallel slots with those of the parent class. Make sure
### to put the new parallel slots *first*.
setMethod("parallelSlotNames", "IFWRanges",
          function(x) c("start", "NAMES", callNextMethod())
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Getters
###

setMethod("start", "IFWRanges", function(x, ...) x@start)

setMethod("width", "IFWRanges", function(x) rep.int(x@width, length(x)))

setMethod("names", "IFWRanges", function(x) x@NAMES)

setMethod("ranges", "IntegerRanges",
    function(x, use.names=TRUE, use.mcols=FALSE)
    {
        if (!isTRUEorFALSE(use.names))
            stop("'use.names' must be TRUE or FALSE")
        if (!isTRUEorFALSE(use.mcols))
            stop("'use.mcols' must be TRUE or FALSE")
        ans_start <- start(x)
        ans_width <- width(x)
        ans_names <- if (use.names) names(x) else NULL
        ans_mcols <- if (use.mcols) mcols(x, use.names=FALSE) else NULL
        new2("IRanges", start=ans_start,
             width=ans_width,
             NAMES=ans_names,
             elementMetadata=ans_mcols,
             check=FALSE)
    }
)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Constructor
###

.normarg_fixed_width <- function(width)
{
    width <- .normargSEW0(width, "width")
    if (identical(width, integer()))
        return(width)
    width0 <- unique(width)
    if (length(width0) > 1)
        stop("'width' must not have more than 1 unique value")
    if (width0 < 0)
        stop("negative widths are not allowed")
    width0
}

IFWRanges <- function(start=NULL, end=NULL, width=NULL, names=NULL)
{
    if (is(start, "IntegerRanges")) {
        if (!is.null(end) || !is.null(width))
            stop("'end' and 'width' must be NULLs ",
                 "when 'start' is an IntegerRanges object")
        width <- .normarg_fixed_width(width(start))
        ans <- new2("IFWRanges", start=start(start), width=width,
                    NAMES=names, check=FALSE)
        return(ans)
    }
    start <- .normargSEW0(start, "start")
    end <- .normargSEW0(end, "end")
    width <- .normarg_fixed_width(width)
    ## Shortcut if user supplied 'width' and one of 'start' or 'end'.
    if (length(width) && xor(length(start), length(end))) {
        if (length(start) == 0L)
            start <- end - width + 1L
        return(new2("IFWRanges", start=start, width=width, NAMES=names,
                    check=FALSE))
    }
    ## Otherwise construct an IRanges object and coerce to a IFWRanges
    ## instance.
    ans <- solveUserSEW0(start, end, width)
    names(ans) <- names
    as(ans, "IFWRanges")
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion
###

.from_IntegerRanges_to_IFWRanges <- function(from)
{
    from_width <- width(from)
    if (!length(from_width))
        return(new("IFWRanges"))
    if (!all(from_width == from_width[[1L]]))
        stop(wmsg("all the ranges in the ", class(from), " object to ",
                  "coerce to IFWRanges must have the same width"))
    ans <- IFWRanges(start=start(from), width=width(from), names=names(from))
    mcols(ans) <- mcols(from, use.names=FALSE)
    ans
}
setAs("IntegerRanges", "IFWRanges", .from_IntegerRanges_to_IFWRanges)

setAs("ANY", "IFWRanges",
    function(from) .from_IntegerRanges_to_IFWRanges(as(from, "IntegerRanges"))
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Concatenation
###

.concatenate_IFWRanges_objects <-
    function(x, objects=list(), use.names=TRUE, ignore.mcols=FALSE, check=TRUE)
{
    objects <- S4Vectors:::prepare_objects_to_bind(x, objects)
    all_objects <- c(list(x), objects)

    # TODO: Is this necessary? Why doesn't bindROWS,IRanges-method have
    #       something similar?
    ans_len <- suppressWarnings(sum(lengths(all_objects)))
    if (is.na(ans_len))
        stop("too many integer positions to concatenate")

    ## 1. Take care of the non-parallel slots

    ## Concatenate the "width" slots.
    ## Note that we use direct slot access to avoid materializing the width
    ## vector (which could be large).
    width_list <- lapply(all_objects, slot, "width")
    ans_width <- .normarg_fixed_width(unlist(width_list, use.names=FALSE))

    ## 2. Take care of the parallel slots

    ## Call method for Vector objects to concatenate all the parallel
    ## slots (only "start", "NAMES", and "elementMetadata" in the case of
    ## IFWRanges) and stick them into ans.
    ## Note that there's no need to do anything for the non-parallel slot
    ## "width" because we've already checked that all objects have the
    ## same width, so we can just use the width of the first object.
    ans <- callNextMethod(x, objects, use.names=use.names,
                          ignore.mcols=ignore.mcols, check=check)

    # TODO: Replacing the "width" slot is redundant because by this point
    #       it will have inherited the correct width from x@width or
    #       errored due to .normarg_fixed_width() detecting incompatible
    #       widths.
    BiocGenerics:::replaceSlots(ans, width=ans_width,
                                check=check)
}

setMethod("bindROWS", "IFWRanges", .concatenate_IFWRanges_objects)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Low-level setters for IRanges objects.
###
### All these low-level setters preserve the length of the object.
### The choice was made to implement a "resizing" semantic:
###   (1) changing the start preserves the end (so it changes the width)
###   (2) changing the end preserves the start (so it changes the width)
###   (3) changing the width preserves the start (so it changes the end)
###

.set_IFWRanges_width <- function(x, value, check=TRUE)
{
    if (!isTRUEorFALSE(check))
        stop("'check' must be TRUE or FALSE")
    value <- .normarg_fixed_width(value)
    x@width <- value
    if (check)
        validObject(x)
    x
}

setReplaceMethod("width", "IFWRanges",
    function(x, ..., value) .set_IFWRanges_width(x, value)
)


set_IFWRanges_names <- function(x, value)
{
    x@NAMES <- S4Vectors:::normalize_names_replacement_value(value, x)
    ## No need to validate an IFWRanges object after setting its names so
    ## this should be safe.
    x
}

setReplaceMethod("names", "IFWRanges", set_IFWRanges_names)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Subsetting
###

setMethod("replaceROWS", "IFWRanges",
    function(x, i, value)
    {
        i <- normalizeSingleBracketSubscript(i, x, as.NSBS=TRUE)
        ans_start <- replaceROWS(start(x), i, start(value))
        ans_width <- .normarg_fixed_width(c(x@width, value@width))
        ans_mcols <- replaceROWS(mcols(x, use.names=FALSE), i,
                                 mcols(value, use.names=FALSE))
        BiocGenerics:::replaceSlots(x, start=ans_start,
                                    width=ans_width,
                                    mcols=ans_mcols,
                                    check=FALSE)
    }
)

