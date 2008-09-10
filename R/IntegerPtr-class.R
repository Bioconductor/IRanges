### =========================================================================
### External pointer to an integer vector: the "IntegerPtr" class
### -------------------------------------------------------------------------
###
### The "IntegerPtr" class implements the concept of "RawPtr" objects but
### for integers instead of bytes.
### Some differences between "integer" and "IntegerPtr":
###
###   1. IntegerPtr(10) does not initialize its values (integer(10) does).
###
###   2. IntegerPtr(10)[i] produces an error if i is out of bounds.
###
###   3. IntegerPtr objects are faster:
###
###        > a <- integer(100000000)
###        > system.time(tmp <- a[])
###        [1] 0.65 0.30 0.95 0.00 0.00
###        > system.time(a[] <- 100:1)
###        [1] 3.08 0.52 3.61 0.00 0.00
###
###        > ib <- IntegerPtr(100000000)
###        > system.time(tmp <- ib[])
###        [1] 0.39 0.52 0.91 0.00 0.00
###        > system.time(ib[] <- 100:1)
###        [1] 0.56 0.00 0.56 0.00 0.00
###

setClass("IntegerPtr", representation(xp="externalptr"))


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Initialization.
###
### Note that unlike integer vectors, IntegerPtr objects are not initialized
### with 0's.
###

### This:
###   xi <- IntegerPtr(30)
### will call this "initialize" method.
setMethod("initialize", "IntegerPtr",
    function(.Object, length=0, initialize=FALSE, verbose=FALSE)
    {
        if (isSingleNumber(length)) {
            values <- NULL
        } else {
            values <- as.integer(length)
            length <- length(values)
        }
        length <- as.integer(length)
        if (length < 0)
            stop("'length' must be a non-negative integer")
        xp <- .Call("ExternalPtr_new", PACKAGE="IRanges")
        if (verbose)
            cat("Allocating memory for new", class(.Object), "object...")
        if (initialize)
            .Call("IntegerPtr_alloc_initialize", xp, length, PACKAGE="IRanges")
        else
            .Call("IntegerPtr_alloc", xp, length, PACKAGE="IRanges")
        if (verbose) {
            cat(" OK\n")
            show_string <- .Call("IntegerPtr_get_show_string", xp, PACKAGE="IRanges")
            cat("New", show_string, "successfully created\n")
        }
        .Object@xp <- xp
        if (!is.null(values) && (length == length(values)))
            .Object[] <- values
        .Object
    }
)

IntegerPtr <- function(...)
{
    new("IntegerPtr", ...)
}

setMethod("show", "IntegerPtr",
    function(object)
    {
        show_string <- .Call("IntegerPtr_get_show_string", object@xp, PACKAGE="IRanges")
        cat(show_string, "\n", sep="")
        ## What is correct here? The documentation (?show) says that 'show'
        ## should return an invisible 'NULL' but, on the other hand, the 'show'
        ## method for integers returns its 'object' argument...
        invisible(object)
    }
)

setMethod("length", "IntegerPtr",
    function(x)
    {
        .Call("IntegerPtr_length", x@xp, PACKAGE="IRanges")
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Read/write functions.
### These are almost safe wrappers to unsafe C functions ("almost" because
### they don't check for NAs in their arguments).
### If length(i) == 0 then the read functions return an empty vector
### and the write functions don't do anything.

IntegerPtr.read <- function(x, i, imax=integer(0))
{
    if (!is.integer(i))
        i <- as.integer(i)
    if (length(i) == 1) {
        if (length(imax) == 0)
            imax <- i
        else
            imax <- as.integer(imax)
        .Call("IntegerPtr_read_ints_from_i1i2", x@xp, i, imax, PACKAGE="IRanges")
    } else {
        .Call("IntegerPtr_read_ints_from_subset", x@xp, i, PACKAGE="IRanges")
    }
}

IntegerPtr.write <- function(x, i, imax=integer(0), value)
{
    if (!is.integer(value))
        stop("'value' must be an integer vector")
    if (!is.integer(i))
        i <- as.integer(i)
    if (length(i) == 1) {
        if (length(imax) == 0)
            imax <- i
        else
            imax <- as.integer(imax)
        .Call("IntegerPtr_write_ints_to_i1i2", x@xp, i, imax, value, PACKAGE="IRanges")
    } else {
        .Call("IntegerPtr_write_ints_to_subset", x@xp, i, value, PACKAGE="IRanges")
    }
    x
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### length(as.integer(ib)) is equivalent to length(ib)
### but the latter is MUCH faster!
setMethod("as.integer", "IntegerPtr",
    function(x)
    {
        IntegerPtr.read(x, 1, length(x))
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Subsetting

setMethod("[", "IntegerPtr",
    function(x, i, j, ..., drop=TRUE)
    {
        if (!missing(j) || length(list(...)) > 0)
            stop("invalid subsetting")
        if (missing(i))
            subset <- as.integer(x)
        else
            subset <- IntegerPtr.read(x, i)
        if (!drop)
            subset <- IntegerPtr(subset)
        subset
    }
)

setReplaceMethod("[", "IntegerPtr",
    function(x, i, j,..., value)
    {
        if (!missing(j) || length(list(...)) > 0)
            stop("invalid subsetting")

        ## We want to allow this: ib[3] <- 4, even if storage.mode(value)
        ## is not "integer"
        if (!is.integer(value)) {
            if (length(value) >= 2)
                stop("'storage.mode(value)' must be \"integer\"")
            tmp <- value
            value <- as.integer(value)
            if (value != tmp)
                stop("'value' must be an integer")
        }
        ## Now 'value' is an integer vector
        if (missing(i))
            return(IntegerPtr.write(x, 1, length(x), value=value))
        IntegerPtr.write(x, i, value=value)
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Equality

setMethod("==", signature(e1="IntegerPtr", e2="IntegerPtr"),
    function(e1, e2)
    {
        address(e1@xp) == address(e2@xp)
    }
)
setMethod("!=", signature(e1="IntegerPtr", e2="IntegerPtr"),
    function(e1, e2)
    {
        address(e1@xp) != address(e2@xp)
    }
)

### A wrapper to the very fast memcmp() C-function.
### Arguments MUST be the following or it will crash R:
###   x1, x2: "IntegerPtr" objects
###   start1, start2, width: single integers
### In addition: 1 <= start1 <= start1+width-1 <= length(x1)
###              1 <= start2 <= start2+width-1 <= length(x2)
### WARNING: This function is voluntarly unsafe (it doesn't check its
### arguments) because we want it to be the fastest possible!
IntegerPtr.compare <- function(x1, start1, x2, start2, width)
{
    .Call("IntegerPtr_memcmp", x1@xp, start1, x2@xp, start2, width, PACKAGE="IRanges")
}
