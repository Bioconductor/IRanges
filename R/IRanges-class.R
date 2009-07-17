### =========================================================================
### IRanges objects
### -------------------------------------------------------------------------
###
### The IRanges class is a simple container for storing a set of integer
### ranges.
###

setClass("IRanges",
    contains="Ranges",
    representation(
        start="integer",
        width="integer",
        NAMES="characterORNULL"  # R doesn't like @names !!
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
### Accessor methods.
###

setMethod("start", "IRanges", function(x, ...) x@start)

setMethod("width", "IRanges", function(x) x@width)

setMethod("names", "IRanges", function(x) x@NAMES)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "isEmpty" default method for Ranges objects would work just fine on a
### NormalIRanges object but we can take advantage of the normality to make
### it slightly more efficient.
###

setMethod("isEmpty", "NormalIRanges", function(x) length(x) == 0L)


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
### Both 'start' and 'width' must be integer vectors of equal length
### (eventually 0) with no NAs and such that all(width >= 0) is TRUE.
### 'names' must be NULL or a character vector of the same length as 'start'
### (or 'width').
###
### We use 'min(width(x)) < 0L' in .valid.IRanges.width().
### Note that the 'min(x) <= y' construct is faster and more memory efficent
### than 'any(x <= y)', especially when 'x' is a big vector (the speedup is
### around 10x or more when length(x) >= 100000).
###

### IRanges objects

.valid.IRanges.start <- function(x)
{
    if (!is.integer(start(x)) || any(is.na(start(x))))
        return("the starts must be non-NA integers")
    if (length(start(x)) != length(width(x)))
        return("number of starts and number of widths differ")
    NULL
}

.valid.IRanges.width <- function(x)
{
    if (!is.integer(width(x)) || any(is.na(width(x))))
        return("the widths must be non-NA integers")
    if (length(start(x)) != length(width(x)))
        return("number of starts and number of widths differ")
    if (length(width(x)) != 0L && min(width(x)) < 0L)
        return("negative widths are not allowed")
    NULL
}

.valid.IRanges.names <- function(x)
{
    if (is.null(names(x)))
        return(NULL)
    if (!is.character(names(x)))
        return("the names must be a character vector or NULL")
    if (length(names(x)) != length(x))
        return("number of names and number of elements differ")
    NULL
}

.valid.IRanges <- function(x)
{
    c(.valid.IRanges.start(x),
      .valid.IRanges.width(x),
      .valid.IRanges.names(x))
}

setValidity2("IRanges", .valid.IRanges)

### NormalIRanges objects

.valid.NormalIRanges <- function(x)
{
    if (!isNormal(x))
        return("object is not normal")
    NULL
}

setValidity2("NormalIRanges", .valid.NormalIRanges)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion.
###

### Any Ranges object can be turned into an IRanges instance.
setAs("Ranges", "IRanges",
    function(from)
        new2("IRanges", start=start(from), width=width(from),
             NAMES=names(from), check=FALSE)
)

### Helper function (not exported) used by the "coerce" methods defined in
### IRanges-utils.R. Believe it or not but the implicit "coerce" methods do
### NOT check that they return a valid object!
newNormalIRangesFromIRanges <- function(x, check=TRUE)
{
    if (!is(x, "IRanges"))
        stop("'x' must be an IRanges object")
    ## Check only what needs to be checked.
    if (check)
        stopIfProblems(.valid.NormalIRanges(x))
    ## Make a "hard copy" of the slots. No need to check anything!
    new2("NormalIRanges", start=x@start, width=x@width, NAMES=x@NAMES, check=FALSE)
}

### The returned IRanges instance is guaranteed to be normal.
setAs("logical", "Ranges",
    function(from) as(Rle(from), "Ranges")
)
setAs("logical", "IRanges",
      function(from) as(Rle(from), "IRanges")
)

setAs("logical", "NormalIRanges",
    function(from) as(Rle(from), "NormalIRanges")
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Low-level (i.e. non-exported and unsafe) replacement functions for
### IRanges objects.
###
### The choice was made to implement a "resizing" semantic:
###   (1) changing the start preserves the end (so it changes the width)
###   (2) changing the end preserves the start (so it changes the width)
###   (3) changing the width preserves the start (so it changes the end)
###
### IMPORTANT: They do NOT check their arguments ('x' and 'value'). In
### particular they do not check that 'value' is of the expected type (integer
### for "unsafe.start<-", "unsafe.width<-", "unsafe.end<-", and character for
### "unsafe.names<-"). Also they don't check that the resulting IRanges object
### is valid!
###

### 'value' is recycled.
`unsafe.start<-` <- function(x, value)
{
    old_start <- start(x)
    ## Use 'x@start[]' instead of just 'x@start' so the right value is recycled
    x@start[] <- numeric2integer(value)
    x@width <- width(x) - start(x) + old_start
    x
}

### 'value' is recycled.
`unsafe.end<-` <- function(x, value)
{
    ## Use 'x@width[]' instead of just 'x@width' so the right value is recycled
    x@width[] <- width(x) + numeric2integer(value) - end(x)
    x
}

### 'value' is recycled.
`unsafe.width<-` <- function(x, value)
{
    ## Use 'x@width[]' instead of just 'x@width' so the right value is recycled
    x@width[] <- numeric2integer(value)
    x
}

### 'value' is NOT recycled so we stay close to what the standard R "names<-"
### methods generally do
`unsafe.names<-` <- function(x, value)
{
    if (is.null(value))
        x@NAMES <- NULL
    else {
        if (length(value) > length(x))
            stop("too many names")
        if (length(value) < length(x))
            value <- c(value, rep(NA, length(x) - length(value)))
        x@NAMES <- value
    }
    x
}

unsafe.update <- function(object, ...)
{
    valid_argnames <- c("start", "end", "width", "names")
    args <- extraArgsAsList(valid_argnames, ...)
    argnames <- names(args)
    sew <- c("start", "end", "width")
    narg_in_sew <- sum(sew %in% argnames)
    if (narg_in_sew == 3)
        stop("only two out of the ",
             paste("'", sew, "'", sep="", collapse=", "),
             " arguments can be supplied")
    do_atomic_update <- narg_in_sew == 2 && (is.null(names(object))
                                             || ("names" %in% argnames))
    if (do_atomic_update) {
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
        object@start <- numeric2integer(start)
        object@width <- numeric2integer(width)
        object@NAMES <- args$names
        return(object)
    }
    if ("start" %in% argnames)
        unsafe.start(object) <- args$start
    if ("end" %in% argnames)
        unsafe.end(object) <- args$end
    if ("width" %in% argnames)
        unsafe.width(object) <- args$width
    if ("names" %in% argnames)
        unsafe.names(object) <- args$names
    object
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Exported (and safe) replacement methods.
###
### See the unsafe replacement functions for IRanges objects above for the
### "sliding rules".
###
### Note that we don't call validObject(x) after 'x' has been modified because
### we don't need to revalidate the entire object: validating the bits that
### have been touched is enough (and faster). However, because of this, when
### defining a new class that contains the IRanges class, if objects of
### the new class must satisfy additional constraints, then some of the
### replacement methods below need to be overridden for this new class.
###

setReplaceMethod("start", "IRanges",
    function(x, check=TRUE, value)
    {
        unsafe.start(x) <- value
        if (check)
            validObject(x)
        x
    }
)

setReplaceMethod("width", "IRanges",
    function(x, check=TRUE, value)
    {
        unsafe.width(x) <- value
        if (check)
            validObject(x)
        x
    }
)

setReplaceMethod("end", "IRanges",
    function(x, check=TRUE, value)
    {
        unsafe.end(x) <- value
        if (check)
            validObject(x)
        x
    }
)

setReplaceMethod("names", "IRanges",
    function(x, value)
    {
        if (!is.null(value))
            value <- as.character(value)
        unsafe.names(x) <- value
        x
    }
)


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

### FIXME: need some way of specifying the extent of validity
### checking, like giving the class up to which the object is
### assumed valid.
setMethod("update", "IRanges",
    function(object, ..., check = TRUE)
    {
        object <- unsafe.update(object, ...)
        if (check)
            validObject(object)
        object
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Subsetting.
###

### Supported 'i' types: numeric vector, logical vector, NULL and missing.
setMethod("[", "IRanges",
    function(x, i, j, ..., drop)
    {
        if (!missing(j) || length(list(...)) > 0L)
            stop("invalid subsetting")
        if (missing(i))
            return(x)
        if (!is(i, "Ranges") && !is.atomic(i))
            stop("invalid subscript type")
        lx <- length(x)
        if (is.numeric(i)) {
            if (any(is.na(i)))
                stop("subscript contains NAs")
            if (any(i < -lx) || any(i > lx))
                stop("subscript out of bounds")
            if (is(x, "NormalIRanges") && all(i >= 0)) {
                if (!is.integer(i))
                    i <- as.integer(i)
                i <- i[i != 0L]
                if (isNotStrictlySorted(i))
                    stop("positive numeric subscript must be strictly increasing ",
                         "for NormalIRanges objects")
            }
        } else if (is.logical(i)) {
            if (any(is.na(i)))
                stop("subscript contains NAs")
            if (length(i) > lx)
                stop("subscript out of bounds")
        } else if (is.character(i) || is.factor(i)) {
          if (is.null(names(x)))
            stop("cannot subset by character when names are NULL")
          i <- match(i, names(x))
          if (any(is.na(i)))
            stop("subsetting by character would result in NA's")
        } else if (is(i, "Ranges")) {
            i <- x %in% i
        } else if (!is.null(i)) {
            stop("invalid subscript type")
        }
        slot(x, "start", check=FALSE) <- start(x)[i]
        slot(x, "width", check=FALSE) <- width(x)[i]
        if (!is.null(names(x)))
            slot(x, "NAMES", check=FALSE) <- names(x)[i]
        .bracket.Sequence(x, i)
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Combining.
###
### The "c" method for IRanges objects is implemented to behave like an
### endomorphism i.e. to return an object of the same class as 'x'. In
### particular 'c(x)' now returns 'x' and not 'as(x, IRanges)'.
### It's easy to implement specific "c" methods for IRanges subclasses:
### typically they just need to call 'x <- callNextMethod()' (the 'x' they
### get is already of the same class as the original 'x') and to take care
### of the additional slots (aka the class-specific slots). If there aren't
### any additional slots (e.g. NormalIRanges) or if they don't need to be
### modified (e.g. Views), then no need to implement a specific method at all.
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
    function(x, ..., recursive=FALSE)
    {
        if (!identical(recursive, FALSE))
            stop("'recursive' mode not supported")
        if (missing(x)) {
            args <- list(...)
            x <- args[[1L]]
        } else {
            args <- list(x, ...)
        }
        if (length(args) == 1L)
            return(x)
        arg_is_null <- sapply(args, is.null)
        if (any(arg_is_null))
            args[arg_is_null] <- NULL  # remove NULL elements by setting them to NULL!
        if (!all(sapply(args, is, class(x))))
            stop("all arguments in '...' must be ", class(x), " objects (or NULLs)")
        new_start <- unlist(lapply(args, start), use.names=FALSE)
        new_width <- unlist(lapply(args, width), use.names=FALSE)
        new_elementMetadata <- do.call(rbind, lapply(args, elementMetadata))
        names_list <- lapply(args, names)
        arg_has_no_names <- sapply(names_list, is.null)
        if (all(arg_has_no_names)) {
            new_names <- NULL
        } else {
            names_list[arg_has_no_names] <- lapply(args[arg_has_no_names],
                                                   function(arg) character(length(arg)))
            new_names <- unlist(names_list, use.names=FALSE)
        }
        ans <-
          update(x, start=new_start, width=new_width, names=new_names, check=FALSE)
        elementMetadata(ans) <- new_elementMetadata
        validObject(ans)
        ans
    }
)

