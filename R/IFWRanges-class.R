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
        fixed_width="integer",
        NAMES="character_OR_NULL"  # R doesn't like @names !!
    )
)

# TODO: Comment copied from IPos-class.R; is this really that expensive for
#       IFWRanges objects?
### Expensive! (and not needed)
#setValidity2("IFWRanges", validate_FWRanges)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Getters
###

setMethod("fixed_width", "IFWRanges", function(x) x@fixed_width)

setMethod("start", "IFWRanges", function(x, ...) x@start)

setMethod("names", "IFWRanges", function(x) x@NAMES)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### parallelSlotNames()
###

### Combine the new parallel slots with those of the parent class. Make sure
### to put the new parallel slots *first*.
setMethod("parallelSlotNames", "IFWRanges",
          function(x) c("start", "NAMES", callNextMethod())
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Constructor
###


.normarg_fixed_width <- function(width, funname = NULL)
{
    width <- .normargSEW0(width, "width")
    if (identical(width, integer()))
        return(width)
    fixed_width <- unique(width)
    if (length(fixed_width) > 1)
        if (is.null(funname)) {
            stop("'width' must not have more than 1 unique value")
        } else if (funname == "bindROWS") {
            stop("all the objects to concatenate must have the same width")
        } else if (funname == "replaceROWS") {
            stop("replacement value must have the same width as x")
        } else {
            stop("")
        }
    if (fixed_width < 0)
        stop("negative widths are not allowed")
    fixed_width
}

IFWRanges <- function(start=NULL, end=NULL, width=NULL, names=NULL)
{
    if (is(start, "IntegerRanges")) {
        if (!is.null(end) || !is.null(width))
            stop("'end' and 'width' must be NULLs ",
                 "when 'start' is an IntegerRanges object")
        fixed_width <- .normarg_fixed_width(width(start))
        ans_names <- S4Vectors:::normalize_names_replacement_value(names, start)
        ans <- new2("IFWRanges", start=start(start), fixed_width=fixed_width,
                    NAMES=ans_names, check=FALSE)
        return(ans)
    }
    start <- .normargSEW0(start, "start")
    end <- .normargSEW0(end, "end")
    fixed_width <- .normarg_fixed_width(width)
    ## Shortcut if user supplied 'width' and one of 'start' or 'end'.
    if (length(fixed_width) && xor(length(start), length(end))) {
        if (length(start) == 0L)
            start <- end - fixed_width + 1L
        ans_names <- S4Vectors:::normalize_names_replacement_value(names, start)
        ans <- new2("IFWRanges", start=start, fixed_width=fixed_width,
                    NAMES=ans_names, check=FALSE)
        return(ans)
    }
    ## Otherwise construct an IRanges object and coerce to a IFWRanges
    ## instance.
    ans <- solveUserSEW0(start, end, fixed_width)
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
    ans <- IFWRanges(start=start(from), width=from_width, names=names(from))
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

    # TODO: Is this necessary (comes from IPos)?
    #       Why doesn't bindROWS,IRanges-method have something similar?
    ans_len <- suppressWarnings(sum(lengths(all_objects)))
    if (is.na(ans_len))
        stop("too many integer positions to concatenate")

    ## 1. Take care of the non-parallel slots

    ## Concatenate the "width" slots.
    ## Note that we use direct slot access to avoid materializing the width
    ## vector (which could be large).
    fixed_width_list <- lapply(all_objects, slot, "fixed_width")
    ans_fixed_width <- .normarg_fixed_width(unlist(fixed_width_list,
                                                   use.names=FALSE),
                                            "bindROWS")

    ## 2. Take care of the parallel slots

    ## Call method for Vector objects to concatenate all the parallel
    ## slots (only "start", "NAMES", and "elementMetadata" in the case of
    ## IFWRanges) and stick them into ans.
    ## Note that there's no need to do anything for the non-parallel slot
    ## "width" because we've already checked that all objects have the
    ## same width, so we can just use the width of the first object.
    ans <- callNextMethod(x, objects, use.names=use.names,
                          ignore.mcols=ignore.mcols, check=check)

    # TODO: Replacing the "fixed_width" slot is redundant because by this point
    #       it will have inherited the correct width from x@fixed_width or
    #       errored due to .normarg_fixed_width() detecting incompatible
    #       widths.
    BiocGenerics:::replaceSlots(ans, fixed_width=ans_fixed_width,
                                check=check)
}

setMethod("bindROWS", "IFWRanges", .concatenate_IFWRanges_objects)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Low-level setters for IFWRanges objects.
###
### All these low-level setters preserve the length of the object.
### The choice was made to implement a "resizing" semantic:
###   (1) changing the start preserves the end (so it changes the width)
###   (2) changing the end preserves the start (so it changes the width)
###   (3) changing the width preserves the start (so it changes the end)
###

.set_IFWRanges_fixed_width <- function(x, value, check=TRUE)
{
    if (!isTRUEorFALSE(check))
        stop("'check' must be TRUE or FALSE")
    value <- .normarg_fixed_width(value)
    x@fixed_width <- value
    if (check)
        validObject(x)
    x
}

setReplaceMethod("width", "IFWRanges",
    function(x, ..., value) .set_IFWRanges_fixed_width(x, value)
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
        ans_fixed_width <- .normarg_fixed_width(c(x@fixed_width,
                                                  value@fixed_width),
                                                "replaceROWS")
        ans_mcols <- replaceROWS(mcols(x, use.names=FALSE), i,
                                 mcols(value, use.names=FALSE))
        BiocGenerics:::replaceSlots(x, start=ans_start,
                                    fixed_width=ans_fixed_width,
                                    mcols=ans_mcols,
                                    check=FALSE)
    }
)

