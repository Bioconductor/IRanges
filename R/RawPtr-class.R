### =========================================================================
### External pointer to a raw vector: the "RawPtr" class
### -------------------------------------------------------------------------
###
### An RawPtr object is a chunk of memory that:
###   a. Contains bytes (char at the C level).
###   b. Is readable and writable.
###   c. Has a passed by address semantic i.e. the data it contains are not
###      copied on object duplication. For example when doing
###        xr2 <- xr1
###      both xr1 and xr2 point to the same place in memory.
###      This is achieved by using R predefined type "externalptr".
###
### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "RawPtr" class.
### Note: instead of defining the "RawPtr" class with just one slot of type
### "externalptr" (it HAS an "externalptr", and nothing else), an alternative
### would be to simply extend the "externalptr" type.
### After all, an RawPtr object IS an "externalptr" object.
### However, I tried this but was not able to implement the "initialize" method
### in such a way that it returns a new instance of the "RawPtr" class (the
### returned object was ALWAYS the same instance everytime the method was
### called, I found no workaround).
###

setClass("RawPtr", representation(xp="externalptr"))

### A temporary "redirection" of the XRaw class to the RawPtr class. Only
### during the time needed to migrate all the BSgenome data packages to the
### new RawPtr class.
### TODO: Remove the XRaw class when all the BSgenome data packages are fixed.
setClass("XRaw", contains="RawPtr")


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Initialization.
###
### Note that unlike raw vectors, RawPtr objects are not initialized with 0's.
###

### This:
###   xr <- RawPtr(30)
### will call this "initialize" method.
setMethod("initialize", "RawPtr",
    function(.Object, length=0, verbose=FALSE)
    {
        if (!isSingleNumber(length))
            stop("'length' must be a single integer")
        length <- as.integer(length)
        if (length < 0)
            stop("'length' must be a non-negative integer")
        xp <- .Call("ExternalPtr_new", PACKAGE="IRanges")
        if (verbose)
            cat("Allocating memory for new", class(.Object), "object...")
        .Call("RawPtr_alloc", xp, length, PACKAGE="IRanges")
        if (verbose) {
            cat(" OK\n")
            show_string <- .Call("RawPtr_get_show_string", xp, PACKAGE="IRanges")
            cat("New", show_string, "successfully created\n")
        }
        .Object@xp <- xp
        .Object
    }
)

RawPtr <- function(...)
{
    new("RawPtr", ...)
}

setMethod("show", "RawPtr",
    function(object)
    {
        show_string <- .Call("RawPtr_get_show_string", object@xp, PACKAGE="IRanges")
        cat(show_string, "\n", sep="")
        ## What is correct here? The documentation (?show) says that 'show'
        ## should return an invisible 'NULL' but, on the other hand, the 'show'
        ## method for integers returns its 'object' argument...
        invisible(object)
    }
)

setMethod("length", "RawPtr",
    function(x)
    {
        .Call("RawPtr_length", x@xp, PACKAGE="IRanges")
    }
)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Read/write functions.
### These are almost safe wrappers to unsafe C functions ("almost" because
### they don't check for NAs in their arguments).
### If length(i) == 0 then the read functions return an empty vector
### and the write functions don't do anything.

RawPtr.readInts <- function(x, i, imax=integer(0))
{
    if (!is.integer(i))
        i <- as.integer(i)
    if (length(i) == 1) {
        if (length(imax) == 0)
            imax <- i
        else
            imax <- as.integer(imax)
        .Call("RawPtr_read_ints_from_i1i2", x@xp, i, imax, PACKAGE="IRanges")
    } else {
        .Call("RawPtr_read_ints_from_subset", x@xp, i, PACKAGE="IRanges")
    }
}

RawPtr.writeInts <- function(x, i, imax=integer(0), value)
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
        .Call("RawPtr_write_ints_to_i1i2", x@xp, i, imax, value, PACKAGE="IRanges")
    } else {
        .Call("RawPtr_write_ints_to_subset", x@xp, i, value, PACKAGE="IRanges")
    }
    x
}

### 'dec_lkup' must be NULL or a vector of integers
RawPtr.read <- function(x, i, imax=integer(0), dec_lkup=NULL)
{
    if (!is.integer(i))
        i <- as.integer(i)
    if (length(i) == 1) {
        if (length(imax) == 0)
            imax <- i
        else
            imax <- as.integer(imax)
        if (is.null(dec_lkup))
            .Call("RawPtr_read_chars_from_i1i2",
                  x@xp, i, imax, PACKAGE="IRanges")
        else
            .Call("RawPtr_read_enc_chars_from_i1i2",
                  x@xp, i, imax, dec_lkup, PACKAGE="IRanges")
    } else {
        if (is.null(dec_lkup))
            .Call("RawPtr_read_chars_from_subset",
                  x@xp, i, PACKAGE="IRanges")
        else
            .Call("RawPtr_read_enc_chars_from_subset",
                  x@xp, i, dec_lkup, PACKAGE="IRanges")
    }
}

### 'enc_lkup' must be NULL or a vector of integers
RawPtr.write <- function(x, i, imax=integer(0), value, enc_lkup=NULL)
{
    if (!isSingleString(value))
        stop("'value' must be a single string")
    if (!is.integer(i))
        i <- as.integer(i)
    if (length(i) == 1) {
        if (length(imax) == 0)
            imax <- i
        else
            imax <- as.integer(imax)
        if (is.null(enc_lkup))
            .Call("RawPtr_write_chars_to_i1i2",
                  x@xp, i, imax, value, PACKAGE="IRanges")
        else
            .Call("RawPtr_write_enc_chars_to_i1i2",
                  x@xp, i, imax, value, enc_lkup, PACKAGE="IRanges")
    } else {
        if (is.null(enc_lkup))
            .Call("RawPtr_write_chars_to_subset",
                  x@xp, i, value, PACKAGE="IRanges")
        else
            .Call("RawPtr_write_enc_chars_to_subset",
                  x@xp, i, value, enc_lkup, PACKAGE="IRanges")
    }
    x
}

### 'lkup' must be NULL or a vector of integers
RawPtr.copy <- function(dest, i, imax=integer(0), src, lkup=NULL)
{
    if (!is(src, "RawPtr"))
        stop("'src' must be an RawPtr object")
    if (!is.integer(i))
        i <- as.integer(i)
    if (length(i) == 1) {
        if (length(imax) == 0)
            imax <- i
        else
            imax <- as.integer(imax)
        if (is.null(lkup))
            .Call("RawPtr_copy_from_i1i2", dest@xp, src@xp,
                  i, imax, PACKAGE="IRanges")
        else
            .Call("RawPtr_translate_copy_from_i1i2", dest@xp, src@xp,
                  i, imax, lkup, PACKAGE="IRanges")
    } else {
        if (is.null(lkup))
            .Call("RawPtr_copy_from_subset", dest@xp, src@xp,
                  i, PACKAGE="IRanges")
        else
            .Call("RawPtr_translate_copy_from_subset", dest@xp, src@xp,
                  i, lkup, PACKAGE="IRanges")
    }
    dest
}

### 'lkup' must be NULL or a vector of integers
RawPtr.reverseCopy <- function(dest, i, imax=integer(0), src, lkup=NULL)
{
    if (!is(src, "RawPtr"))
        stop("'src' must be an RawPtr object")
    if (length(i) != 1)
        stop("'i' must be a single integer")
    if (!is.integer(i))
        i <- as.integer(i)
    if (length(imax) == 0)
        imax <- i
    else
        imax <- as.integer(imax)
    if (is.null(lkup))
        .Call("RawPtr_reverse_copy_from_i1i2", dest@xp, src@xp, i, imax, PACKAGE="IRanges")
    else
        .Call("RawPtr_reverse_translate_copy_from_i1i2", dest@xp, src@xp, i, imax, lkup, PACKAGE="IRanges")
    dest
}

### 'lkup' must be a vector of complexes
RawPtr.readComplexes <- function(x, i, imax=integer(0), lkup)
{
    if (!is.integer(i))
        i <- as.integer(i)
    if (length(i) == 1) {
        if (length(imax) == 0)
            imax <- i
        else
            imax <- as.integer(imax)
        .Call("RawPtr_read_complexes_from_i1i2",
              x@xp, i, imax, lkup, PACKAGE="IRanges")
    } else {
        .Call("RawPtr_read_complexes_from_subset",
              x@xp, i, lkup, PACKAGE="IRanges")
    }
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "RawPtr.append" function.
###

RawPtr.append <- function(x1, start1, width1, x2, start2, width2)
{
    if (!isSingleNumber(start1))
        stop("'start1' must be a single integer")
    if (!is.integer(start1))
        start1 <- as.integer(start1)
    if (!isSingleNumber(width1))
        stop("'width1' must be a single integer")
    if (!is.integer(width1))
        width1 <- as.integer(width1)

    if (!isSingleNumber(start2))
        stop("'start2' must be a single integer")
    if (!is.integer(start2))
        start2 <- as.integer(start2)
    if (!isSingleNumber(width2))
        stop("'width2' must be a single integer")
    if (!is.integer(width2))
        width2 <- as.integer(width2)

    ans_len <- width1 + width2
    ans <- RawPtr(ans_len)
    .Call("RawPtr_memcpy",
          ans@xp, 1L, x1@xp, start1, width1, PACKAGE="IRanges")
    .Call("RawPtr_memcpy",
          ans@xp, 1L + width1, x2@xp, start2, width2, PACKAGE="IRanges")
    ans
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### length(as.integer(xr)) is equivalent to length(xr)
### but the latter is MUCH faster!
setMethod("as.integer", "RawPtr",
    function(x)
    {
        RawPtr.readInts(x, 1, length(x))
    }
)

### Typical use:
###   xr <- RawPtr(15)
###   xr[] <- 65
###   toString(xr)
###   xr[] <- "Hello"
###   toString(xr)
### So this should always rewrite the content of an RawPtr object
### to itself, without any modification:
###   xr[] <- toString(xr)
### whatever the content of xr is!
setMethod("toString", "RawPtr",
    function(x, ...)
    {
        RawPtr.read(x, 1, length(x))
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Subsetting
###

### Select bytes from an RawPtr object.
### Typical use:
###   xr <- RawPtr(30)
###   xr[25:20]
###   xr[25:31] # subscript out of bounds
### Note: xr[] can be used as a shortcut for as.integer(xr)
setMethod("[", "RawPtr",
    function(x, i, j, ..., drop)
    {
        if (!missing(j) || length(list(...)) > 0)
            stop("invalid subsetting")
        if (missing(i))
            return(as.integer(x))
        RawPtr.readInts(x, i)
    }
)

### Replace bytes in an RawPtr object.
### Typical use:
###   xr <- RawPtr(30)
###   xr[] <- 12 # fill with 12
###   xr[3:10] <- 1:-2
###   xr[3:10] <- "Ab"
###   xr[0] <- 4 # subscript out of bounds
###   xr[31] <- 4 # subscript out of bounds
###   xr[3] <- -12 # subscript out of bounds
setReplaceMethod("[", "RawPtr",
    function(x, i, j,..., value)
    {
        if (!missing(j) || length(list(...)) > 0)
            stop("invalid subsetting")

        ## 'value' is a string
        if (is.character(value)) {
            if (missing(i))
                return(RawPtr.write(x, 1, length(x), value=value))
            return(RawPtr.write(x, i, value=value))
        }

        ## We want to allow this: xr[3] <- 4, even if storage.mode(value)
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
            return(RawPtr.writeInts(x, 1, length(x), value=value))
        RawPtr.writeInts(x, i, value=value)
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Equality.
###

### Be careful to the semantic of the "==" operator:
###   2 RawPtr objects are equals if their @xp slot is the
###   same "externalptr" instance (then they obviously have
###   the same length and contain the same data).
### With this definition, xr1 and xr2 can be 2 different RawPtr objects
### (xr1 != xr2) and contain the same data.
setMethod("==", signature(e1="RawPtr", e2="RawPtr"),
    function(e1, e2)
    {
        IRanges:::address(e1@xp) == IRanges:::address(e2@xp)
    }
)
setMethod("!=", signature(e1="RawPtr", e2="RawPtr"),
    function(e1, e2)
    {
        IRanges:::address(e1@xp) != IRanges:::address(e2@xp)
    }
)

### A wrapper to the very fast memcmp() C-function.
### Arguments MUST be the following or it will crash R:
###   x1, x2: RawPtr objects
###   start1, start2, width: single integers
### In addition: 1 <= start1 <= start1+width-1 <= length(x1)
###              1 <= start2 <= start2+width-1 <= length(x2)
### WARNING: This function is voluntarly unsafe (it doesn't check its
### arguments) because we want it to be the fastest possible!
RawPtr.compare <- function(x1, start1, x2, start2, width)
{
    .Call("RawPtr_memcmp", x1@xp, start1, x2@xp, start2, width, PACKAGE="IRanges")
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "PrintableRawPtr" class is a simple extention of the "RawPtr"
### class (no additional slots)

setClass("PrintableRawPtr", representation("RawPtr"))

setMethod("show", "PrintableRawPtr",
    function(object)
    {
        print(toString(object))
    }
)

### Safe alternative to 'strsplit(x, NULL, fixed=TRUE)[[1]]'.
safeExplode <- function(x)
{
    if (!isSingleString(x))
        stop("'x' must be a single string")
    .Call("safe_strexplode", x, PACKAGE="IRanges")
}

### pxr <- new("PrintableRawPtr", 10)
### pxr[] <- "ab-C."
### pxr[] == strsplit(toString(pxr), NULL, fixed=TRUE)[[1]]
setMethod("[", "PrintableRawPtr",
    function(x, i, j, ..., drop)
    {
        if (!missing(j) || length(list(...)) > 0)
            stop("invalid subsetting")
        if (missing(i))
            s <- toString(x)
        else
            s <- RawPtr.read(x, i)
        safeExplode(s)
    }
)
