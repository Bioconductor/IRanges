### =========================================================================
### IRanges objects
### -------------------------------------------------------------------------
###
### The IRanges class is a simple container for storing a vector of integer
### ranges.
###

setClass("IRanges",
    contains="Ranges",
    representation(
        start="integer",
        width="integer",
        NAMES="character_OR_NULL"  # R doesn't like @names !!
    ),
    prototype(
        start=integer(),
        width=integer(),
        NAMES=NULL
    )
)

### A NormalIRanges object is an IRanges object where the ranges are:
###   (a) not empty (i.e. they have a non-null width);
###   (b) not overlapping;
###   (c) ordered from left to right;
###   (d) not even adjacent (i.e. there must be a non empty gap between 2
###       consecutive ranges).
### If 'x' is an IRanges object of length >= 2, then 'x' is normal iff:
###   start(x)[i] <= end(x)[i] < start(x)[i+1] <= end(x)[i+1]
### for every 1 <= i < length(x).
### If length(x) == 1, then 'x' is normal iff width(x)[1] >= 1.
### If length(x) == 0, then 'x' is normal.
setClass("NormalIRanges", contains="IRanges")


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### parallelSlotNames()
###

### Combine the new parallel slots with those of the parent class. Make sure
### to put the new parallel slots *first*.
setMethod("parallelSlotNames", "IRanges",
    function(x) c("start", "width", "NAMES", callNextMethod())
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Getters
###

setMethod("start", "IRanges", function(x, ...) x@start)

setMethod("width", "IRanges", function(x) x@width)

setMethod("names", "IRanges", function(x) x@NAMES)

setMethod("ranges", "Ranges",
    function(x, use.names=TRUE, use.mcols=FALSE)
    {
        if (!isTRUEorFALSE(use.names))
            stop("'use.names' must be TRUE or FALSE")
        if (!isTRUEorFALSE(use.mcols))
            stop("'use.mcols' must be TRUE or FALSE")
        ans_start <- start(x)
        ans_width <- width(x)
        ans_names <- if (use.names) names(x) else NULL
        ans_mcols <- if (use.mcols) mcols(x) else NULL
        new2("IRanges", start=ans_start,
                        width=ans_width,
                        NAMES=ans_names,
                        elementMetadata=ans_mcols,
                        check=FALSE)
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### isEmpty() and isNormal()
###

.isNormal_IRanges <- function(x)
    .Call2("IRanges_isNormal", x, PACKAGE="IRanges")

setMethod("isNormal", "IRanges", .isNormal_IRanges)

### Fast methods for NormalIRanges objects.
setMethod("isEmpty", "NormalIRanges", function(x) length(x) == 0L)
setMethod("isNormal", "NormalIRanges", function(x) TRUE)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "max" and "min" methods.
###
### Note: defined for NormalIRanges objects only.
### For an ordinary IRanges object 'x', it's not clear what the semantic
### should be. In particular, should empty ranges be ignored or not? If not
### then we could end up with 'min(x)' > 'max(x)' (e.g. when 'x' is made of 1
### empty range) which is not nice. Another (and more pragmatic) reason for
### not defining these methods for IRanges objects is that I don't need them
### at the moment.
###

setMethod("max", "NormalIRanges",
    function(x, ..., na.rm)
    {
        if (isEmpty(x)) {
            warning("empty ", class(x), " object; returning -Inf")
            -Inf
        } else {
            end(x)[length(x)]
        }
    }
)

setMethod("min", "NormalIRanges",
    function(x, ..., na.rm)
    {
        if (isEmpty(x)) {
            warning("empty ", class(x), " object; returning Inf")
            Inf
        } else {
            start(x)[1L]
        }
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Validity.
###
### Validity of IRanges objects is taken care of by the validity method for
### Ranges objects.
###

### NormalIRanges objects
.valid.NormalIRanges <- function(x)
{
    if (!.isNormal_IRanges(x))
        return("object is not normal")
    NULL
}

setValidity2("NormalIRanges", .valid.NormalIRanges)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion.
###

setAs("Ranges", "IRanges",
    function(from) ranges(from, use.mcols=TRUE)
)

### Helper function (not exported) used by the "coerce" methods defined in
### IRanges-utils.R. Believe it or not but the implicit "coerce" methods do
### NOT check that they return a valid object!
newNormalIRangesFromIRanges <- function(x, check=TRUE)
{
    if (!is(x, "IRanges"))
        stop("'x' must be an IRanges object")
    if (!isTRUEorFALSE(check))
        stop("'check' must be TRUE or FALSE")
    ## Check only what needs to be checked.
    if (check)
        S4Vectors:::stopIfProblems(.valid.NormalIRanges(x))
    class(x) <- "NormalIRanges"
    x
}

### The returned IRanges instance is guaranteed to be normal.
setAs("logical", "IRanges",
    function(from) as(as(from, "NormalIRanges"), "IRanges")
)

setAs("logical", "NormalIRanges",
    function(from) .Call2("NormalIRanges_from_logical", from, PACKAGE="IRanges")
)

### coercion from integer
setAs("integer", "IRanges",
    function(from) .Call2("IRanges_from_integer", from, PACKAGE="IRanges")
)

setAs("integer", "NormalIRanges",
    function(from) newNormalIRangesFromIRanges(as(from, "IRanges"))
)

setAs("numeric", "IRanges", function(from) as(as.integer(from), "IRanges"))

setAs("numeric", "NormalIRanges", 
    function(from) newNormalIRangesFromIRanges(as(as.integer(from), "IRanges")))

### coercion from character
.from_character_to_IRanges <- function(from)
{
    stopifnot(is.character(from))
    if (anyNA(from))
        stop(wmsg("converting a character vector to an IRanges object ",
                  "does not support NAs"))
    error_msg <- wmsg(
        "The character vector to convert to an IRanges object must ",
        "contain strings of the form \"start-end\" or \"start..end\", ",
        "with end >= start - 1, or just \"pos\". For example: \"2501-2900\", ",
        "\"2501..2900\", or \"740\"."
    )
    ## We want to split on the first occurence of  "-" that is preceeded by
    ## a digit (ignoring and removing the spaces in between if any).
    from <- sub("([[:digit:]])[[:space:]]*-", "\\1..", from)
    split2 <- CharacterList(strsplit(from, "..", fixed=TRUE))
    split2_eltNROWS <- elementNROWS(split2)
    if (!all(split2_eltNROWS <= 2L))
        stop(error_msg)
    ans_start <- suppressWarnings(as.integer(phead(split2, n=1L)))
    ans_end <- suppressWarnings(as.integer(ptail(split2, n=1L)))
    if (anyNA(ans_start) || anyNA(ans_end))
        stop(error_msg)
    IRanges(ans_start, ans_end, names=names(from))
}
setAs("character", "IRanges", .from_character_to_IRanges)

.from_factor_to_IRanges <- function(from)
{
    from <- setNames(as.character(from), names(from))
    .from_character_to_IRanges(from)
}
setAs("factor", "IRanges", .from_factor_to_IRanges)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Low-level setters for IRanges objects.
###
### All these low-level setters preserve the length of the object.
### The choice was made to implement a "resizing" semantic:
###   (1) changing the start preserves the end (so it changes the width)
###   (2) changing the end preserves the start (so it changes the width)
###   (3) changing the width preserves the start (so it changes the end)
###

.set_IRanges_start <- function(x, value, check=TRUE)
{
    if (!isTRUEorFALSE(check))
        stop("'check' must be TRUE or FALSE")
    old_start <- start(x)
    ## Use 'x@start[]' instead of 'x@start' so the right value is recycled.
    x@start[] <- S4Vectors:::numeric2integer(value)
    x@width <- width(x) - start(x) + old_start
    if (check)
        validObject(x)
    x
}

setReplaceMethod("start", "IRanges",
    function(x, ..., value) .set_IRanges_start(x, value)
)

.set_IRanges_end <- function(x, value, check=TRUE)
{
    if (!isTRUEorFALSE(check))
        stop("'check' must be TRUE or FALSE")
    ## Use 'x@width[]' instead of 'x@width' so the right value is recycled.
    x@width[] <- width(x) + S4Vectors:::numeric2integer(value) - end(x)
    if (check)
        validObject(x)
    x
}

setReplaceMethod("end", "IRanges",
    function(x, ..., value) .set_IRanges_end(x, value)
)

.set_IRanges_width <- function(x, value, check=TRUE)
{
    if (!isTRUEorFALSE(check))
        stop("'check' must be TRUE or FALSE")
    ## Use 'x@width[]' instead of 'x@width' so the right value is recycled.
    x@width[] <- S4Vectors:::numeric2integer(value)
    if (check)
        validObject(x)
    x
}

setReplaceMethod("width", "IRanges",
    function(x, ..., value) .set_IRanges_width(x, value)
)

set_IRanges_names <- function(x, value)
{
    x@NAMES <- S4Vectors:::normalize_names_replacement_value(value, x)
    ## No need to validate an IRanges object after setting its names so this
    ## should be safe.
    x
}

setReplaceMethod("names", "IRanges", set_IRanges_names)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "update" method.
###
### This is a convenience method for combining multiple modifications in one
### single call.
###
### It must verify 2 important properties:
###   (1) update(x) must be identical to x (doesn't touch x at all)
###   (2) update(x, start=start(x), width=width(x), names=names(x))
###       must be identical to x too (but this time it updates x with its own
###       content)
###

.update_IRanges <- function(object, ...)
{
    valid_argnames <- c("start", "width", "end", "names", "mcols")
    args <- S4Vectors:::extraArgsAsList(valid_argnames, ...)
    argnames <- names(args)
    sew <- c("start", "end", "width")
    narg_in_sew <- sum(sew %in% argnames)
    if (narg_in_sew == 3L)
        stop("at most 2 out of the ",
             paste("'", sew, "'", sep="", collapse=", "),
             " arguments can be supplied")
    if (narg_in_sew == 2L &&
        ("names" %in% argnames || is.null(names(object))) &&
        ("mcols" %in% argnames || is.null(mcols(object))))
    {
        ## The update can change the length of the object.
        if ("end" %in% argnames) {
            if ("width" %in% argnames) {
                width <- args$width
                start <- args$end - width + 1L
            } else {
                start <- args$start
                width <- args$end - start + 1L
            }
        } else {
            start <- args$start
            width <- args$width
        }
        object <- BiocGenerics:::replaceSlots(object,
                      start=S4Vectors:::numeric2integer(start),
                      width=S4Vectors:::numeric2integer(width),
                      NAMES=args$names,
                      elementMetadata=args$mcols,
                      check=FALSE)
        return(object)
    }
    ## The update preserves the length of the object.
    if ("start" %in% argnames)
        object <- .set_IRanges_start(object, args$start, check=FALSE)
    if ("end" %in% argnames)
        object <- .set_IRanges_end(object, args$end, check=FALSE)
    if ("width" %in% argnames)
        object <- .set_IRanges_width(object, args$width, check=FALSE)
    if ("names" %in% argnames)
        names(object) <- args$names
    if ("mcols" %in% argnames)
        mcols(object) <- args$mcols
    object
}

### FIXME: need some way of specifying the extent of validity
### checking, like giving the class up to which the object is
### assumed valid.
setMethod("update", "IRanges",
    function(object, ..., check = TRUE)
    {
        if (!isTRUEorFALSE(check))
            stop("'check' must be TRUE or FALSE")
        object <- .update_IRanges(object, ...)
        if (check)
            validObject(object)
        object
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Subsetting.
###

setMethod("extractROWS", "NormalIRanges",
    function(x, i)
    {
        i <- normalizeSingleBracketSubscript(i, x, as.NSBS=TRUE)
        if (is(x, "NormalIRanges")) {
            if (!isStrictlySorted(i))
                stop("subscript must extract elements at strictly sorted ",
                     "positions when\n  subsetting a ", class(x), " object")
        }
        callNextMethod()
    }
)

setMethod("replaceROWS", "IRanges",
    function(x, i, value)
    {
        i <- normalizeSingleBracketSubscript(i, x, as.NSBS=TRUE)
        ans_start <- replaceROWS(start(x), i, start(value))
        ans_width <- replaceROWS(width(x), i, width(value))
        ans_mcols <- replaceROWS(mcols(x), i, mcols(value))
        update(x, start=ans_start,
                  width=ans_width,
                  mcols=ans_mcols,
                  check=FALSE)
    }
)

setMethod("replaceROWS", "NormalIRanges",
    function(x, i, value)
    {
        ans <- callNextMethod()
        validObject(ans)
        ans
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Combining.
###
### The "c" method for IRanges objects is implemented to behave like an
### endomorphism i.e. to return an object of the same class as 'x'. In
### particular 'c(x)' now returns 'x' and not 'as(x, "IRanges")'.
### It's easy to implement specific "c" methods for IRanges subclasses.
### Typically they just need to do something like:
###
###     old_val <- S4Vectors:::disableValidity()
###     on.exit(S4Vectors:::disableValidity(old_val))
###     S4Vectors:::disableValidity(TRUE)
###     ans <- callNextMethod(x, ..., recursive=FALSE)
###     ...
###
### and to take care of the additional slots (aka the subclass-specific
### slots). If there aren't any additional slots (e.g. NormalIRanges), or
### if the additional slots don't need to be modified (e.g. the "subject"
### slot of the Views subclass), then no need to implement a specific method
### at all.
###
### In the case of NormalIRanges objects, 'c(x1, x2)' will fail if the result
### is not normal, but 'c(as(x1, "IRanges"), x2)' or 'c(IRanges(), x1, x2)'
### would work. Note that, in general, 'c(IRanges(), x)' is not the same as
### 'c(x, IRanges())' (the former is equivalent to 'as(x, IRanges)' and the
### latter to 'c(x)' or 'x').
### Also note that the user needs to be carefull when passing named arguments
### to c() (there is no good reason to do this in the first place) because of
### the following pitfalls:
###  (1) If all the arguments are named (e.g. 'c(a=x1, b=x2)') then the first
###      argument must be an IRanges *instance* otherwise dispatch will fail.
###      It's not clear why dispatch works when 'x1' is an IRanges instance
###      because, in that case, formal argument 'x' is missing. It's even
###      less clear why it fails when 'x1' is an IRanges object without being
###      an IRanges instance. For example:
###        x1 <- IRanges(1, 11)
###        x2 <- IRanges(22, 33)
###        ## works as expected:
###        c(a=x1, b=x2)
###        ## works as expected:
###        c(a=x1, asNormalIRanges(x2))
###        ## dispatch fails (the default "c" method is selected)
###        c(a=asNormalIRanges(x1), b=x2))
###  (2) When named and unnamed arguments are mixed and no named argument has
###      name 'x' (e.g. 'c(a=x1, x2)'), then, following the standard rules of
###      argument matching in R, one would expect that the first unnamed
###      argument will match formal argument 'x'. This is more or less what
###      happens:
###        > c(a=x1, x2)
###        IRanges object:
###            start end width
###        [1]     2  22    21
###        [2]     1  11    11
###      but there are some surprises:
###        > c(a=x1, TRUE)
###        Error in c(a = x1, TRUE) : 
###          all arguments in '...' must be logical objects (or NULLs)
###        > c(a=asNormalIRanges(x1), TRUE)
###        $a
###        NormalIRanges object:
###            start end width
###        [1]     1  11    11
###
###        [[2]]
###        [1] TRUE
###

setMethod("c", "IRanges",
    function(x, ..., ignore.mcols=FALSE, recursive=FALSE)
    {
        if (!identical(recursive, FALSE))
            stop("\"c\" method for IRanges objects ",
                 "does not support the 'recursive' argument")
        if (!isTRUEorFALSE(ignore.mcols))
            stop("'ignore.mcols' must be TRUE or FALSE")
        if (missing(x)) {
            args <- unname(list(...))
            x <- args[[1L]]
        } else {
            args <- unname(list(x, ...))
        }
        if (length(args) == 1L)
            return(x)
        arg_is_null <- sapply(args, is.null)
        if (any(arg_is_null))
            args[arg_is_null] <- NULL  # remove NULL elements by setting them to NULL!
        if (!all(sapply(args, is, class(x))))
            stop("all arguments in '...' must be ", class(x), " objects (or NULLs)")

        ans_start <- unlist(lapply(args, start), use.names=FALSE)
        ans_width <- unlist(lapply(args, width), use.names=FALSE)
        names_list <- lapply(args, names)
        arg_has_no_names <- sapply(names_list, is.null)
        if (all(arg_has_no_names)) {
            ans_names <- NULL
        } else {
            names_list[arg_has_no_names] <- lapply(args[arg_has_no_names],
                                                   function(arg) character(length(arg)))
            ans_names <- unlist(names_list, use.names=FALSE)
        }
        if (ignore.mcols) {
            ans_mcols <- NULL
        } else  {
            ans_mcols <- do.call(S4Vectors:::rbind_mcols, args)
        }
        update(x, start=ans_start,
                  width=ans_width,
                  names=ans_names,
                  mcols=ans_mcols)
    }
)

