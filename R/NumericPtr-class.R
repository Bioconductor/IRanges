### =========================================================================
### External pointer to a numeric vector: the "NumericPtr" class
### -------------------------------------------------------------------------
###
### The "NumericPtr" class implements the concept of "RawPtr" objects but
### for numerics instead of bytes.
### Some differences between "numeric" and "NumericPtr":
###
###   1. NumericPtr(10) does not initialize its values (numeric(10) does).
###
###   2. NumericPtr(10)[i] produces an error if i is out of bounds.
###
###   3. NumericPtr objects are faster.
###

setClass("NumericPtr", contains="SequencePtr")


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Initialization.
###
### Note that unlike numeric vectors, NumericPtr objects are not initialized
### with 0's.
###

### This:
###   xn <- NumericPtr(30)
### will call this "initialize" method.
setMethod("initialize", "NumericPtr",
    function(.Object, length=0, verbose=FALSE)
    {
        if (isSingleNumber(length)) {
            values <- NULL
        } else {
            values <- as.numeric(length)
            length <- length(values)
        }
        length <- as.integer(length)
        if (length < 0)
            stop("'length' must be a non-negative integer")
        xp <- .Call("ExternalPtr_new", PACKAGE="IRanges")
        if (verbose)
            cat("Allocating memory for new", class(.Object), "object...")
        .Object@xp <- .Call("NumericPtr_alloc", xp, length, PACKAGE="IRanges")
        if (verbose) {
            cat(" OK\n")
            show_string <- .Call("NumericPtr_get_show_string", .Object, PACKAGE="IRanges")
            cat("New", show_string, "successfully created\n")
        }
        if (!is.null(values) && (length == length(values)))
            .Object[] <- values
        .Object
    }
)

NumericPtr <- function(...)
{
    new("NumericPtr", ...)
}

setMethod("show", "NumericPtr",
    function(object)
    {
        show_string <- .Call("NumericPtr_get_show_string", object, PACKAGE="IRanges")
        cat(show_string, "\n", sep="")
        ## What is correct here? The documentation (?show) says that 'show'
        ## should return an invisible 'NULL' but, on the other hand, the 'show'
        ## method for numerics returns its 'object' argument...
        invisible(object)
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Read/write functions.
### These are almost safe wrappers to unsafe C functions ("almost" because
### they don't check for NAs in their arguments).
### If length(i) == 0 then the read functions return an empty vector
### and the write functions don't do anything.

NumericPtr.read <- function(x, i, imax=integer(0))
{
    if (!is.integer(i))
        i <- as.integer(i)
    if (length(i) == 1) {
        if (length(imax) == 0)
            imax <- i
        else
            imax <- as.integer(imax)
        .Call("NumericPtr_read_nums_from_i1i2", x, i, imax, PACKAGE="IRanges")
    } else {
        .Call("NumericPtr_read_nums_from_subset", x, i, PACKAGE="IRanges")
    }
}

NumericPtr.write <- function(x, i, imax=integer(0), value)
{
    if (!is.numeric(value))
        stop("'value' must be a numeric vector")
    if (!is.integer(i))
        i <- as.integer(i)
    if (length(i) == 1) {
        if (length(imax) == 0)
            imax <- i
        else
            imax <- as.integer(imax)
        .Call("NumericPtr_write_nums_to_i1i2", x, i, imax, value, PACKAGE="IRanges")
    } else {
        .Call("NumericPtr_write_nums_to_subset", x, i, value, PACKAGE="IRanges")
    }
    x
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### length(as.numeric(b)) is equivalent to length(b)
### but the latter is MUCH faster!
setMethod("as.numeric", "NumericPtr",
    function(x)
    {
        NumericPtr.read(x, 1, length(x))
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Subsetting

setMethod("[", "NumericPtr",
    function(x, i, j, ..., drop=TRUE)
    {
        if (!missing(j) || length(list(...)) > 0)
            stop("invalid subsetting")
        if (missing(i))
            subset <- as.numeric(x)
        else
            subset <- NumericPtr.read(x, i)
        if (!drop)
            subset <- NumericPtr(subset)
        subset
    }
)

setReplaceMethod("[", "NumericPtr",
    function(x, i, j,..., value)
    {
        if (!missing(j) || length(list(...)) > 0)
            stop("invalid subsetting")

        ## We want to allow this: b[3] <- 4, even if storage.mode(value)
        ## is not "numeric"
        if (!is.numeric(value)) {
            if (length(value) >= 2)
                stop("'storage.mode(value)' must be \"numeric\"")
            tmp <- value
            value <- as.numeric(value)
            if (value != tmp)
                stop("'value' must be numeric")
        }
        ## Now 'value' is a numeric vector
        if (missing(i))
            return(NumericPtr.write(x, 1, length(x), value=value))
        NumericPtr.write(x, i, value=value)
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Data comparison.
###
### A wrapper to the very fast memcmp() C-function.
### Arguments MUST be the following or it will crash R:
###   x1, x2: "NumericPtr" objects
###   start1, start2, width: single integers
### In addition: 1 <= start1 <= start1+width-1 <= length(x1)
###              1 <= start2 <= start2+width-1 <= length(x2)
### WARNING: This function is voluntarly unsafe (it doesn't check its
### arguments) because we want it to be the fastest possible!
###

NumericPtr.compare <- function(x1, start1, x2, start2, width)
{
    .Call("NumericPtr_memcmp", x1, start1, x2, start2, width, PACKAGE="IRanges")
}

