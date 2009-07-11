### =========================================================================
### External pointer to a raw vector: the "RawPtr" class
### -------------------------------------------------------------------------
###

setClass("RawPtr", contains="SequencePtr")


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Initialization.
###
### Note that, unlike 'raw(99)', 'RawPtr(99)' does NOT initialize its
### data. Specify the 'val' argument if you want data initialization.
###

setMethod("initialize", "RawPtr",
    function(.Object, length=0L, val=NULL)
    {
        if (!isSingleNumber(length) || length < 0)
            stop("'length' must be a single non-negative integer")
        if (!is.integer(length))
            length <- as.integer(length)
        if (!is.null(val) && !is.raw(val)) {
            if (is.numeric(val)) {
                val <- as.raw(val)
            } else if (isSingleString(val)) {
                val <- charToRaw(val)
            } else {
                stop("don't know how to turn 'val' into a raw vector")
            }
        }
        .Call("RawPtr_new", length, val, PACKAGE="IRanges")
    }
)

RawPtr <- function(length=0L, val=NULL)
    new2("RawPtr", length=length, val=val, check=FALSE)

setMethod("show", "RawPtr",
    function(object)
    {
        show_string <- .Call("RawPtr_get_show_string", object, PACKAGE="IRanges")
        cat(show_string, "\n", sep="")
        ## What is correct here? The documentation (?show) says that 'show'
        ## should return an invisible 'NULL' but, on the other hand, the 'show'
        ## method for integers returns its 'object' argument...
        invisible(object)
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
        .Call("RawPtr_read_ints_from_i1i2", x, i, imax, PACKAGE="IRanges")
    } else {
        .Call("RawPtr_read_ints_from_subset", x, i, PACKAGE="IRanges")
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
        .Call("RawPtr_write_ints_to_i1i2", x, i, imax, value, PACKAGE="IRanges")
    } else {
        .Call("RawPtr_write_ints_to_subset", x, i, value, PACKAGE="IRanges")
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
                  x, i, imax, PACKAGE="IRanges")
        else
            .Call("RawPtr_read_enc_chars_from_i1i2",
                  x, i, imax, dec_lkup, PACKAGE="IRanges")
    } else {
        if (is.null(dec_lkup))
            .Call("RawPtr_read_chars_from_subset",
                  x, i, PACKAGE="IRanges")
        else
            .Call("RawPtr_read_enc_chars_from_subset",
                  x, i, dec_lkup, PACKAGE="IRanges")
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
                  x, i, imax, value, PACKAGE="IRanges")
        else
            .Call("RawPtr_write_enc_chars_to_i1i2",
                  x, i, imax, value, enc_lkup, PACKAGE="IRanges")
    } else {
        if (is.null(enc_lkup))
            .Call("RawPtr_write_chars_to_subset",
                  x, i, value, PACKAGE="IRanges")
        else
            .Call("RawPtr_write_enc_chars_to_subset",
                  x, i, value, enc_lkup, PACKAGE="IRanges")
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
            .Call("RawPtr_copy_from_i1i2",
                  dest, src, i, imax, PACKAGE="IRanges")
        else
            .Call("RawPtr_translate_copy_from_i1i2",
                   dest, src, i, imax, lkup, PACKAGE="IRanges")
    } else {
        if (is.null(lkup))
            .Call("RawPtr_copy_from_subset",
                  dest, src, i, PACKAGE="IRanges")
        else
            .Call("RawPtr_translate_copy_from_subset",
                  dest, src, i, lkup, PACKAGE="IRanges")
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
        .Call("RawPtr_reverse_copy_from_i1i2",
              dest, src, i, imax, PACKAGE="IRanges")
    else
        .Call("RawPtr_reverse_translate_copy_from_i1i2",
              dest, src, i, imax, lkup, PACKAGE="IRanges")
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
              x, i, imax, lkup, PACKAGE="IRanges")
    } else {
        .Call("RawPtr_read_complexes_from_subset",
              x, i, lkup, PACKAGE="IRanges")
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
    .Call("RawPtr_memcpy", ans, 1L, x1, start1, width1, PACKAGE="IRanges")
    .Call("RawPtr_memcpy", ans, 1L + width1, x2, start2, width2, PACKAGE="IRanges")
    ans
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion.
###
### TODO: add the "as.raw" and "as.character" methods.
###

setMethod("as.integer", "RawPtr",
    function(x, ...)
    {
        RawPtr.readInts(x, 1L, length(x))
    }
)

### Typical use:
###   x <- RawPtr(15, as.raw(65))
###   toString(x)
###   x <- RawPtr(5, charToRaw("Hello"))
###   toString(x)
### This should always rewrite the content of an RawPtr object
### to itself, without any modification:
###   RawPtr.write(x, 1, length(x), value=toString(x))
### whatever the content of 'x' is!
setMethod("toString", "RawPtr",
    function(x, ...)
    {
        RawPtr.read(x, 1, length(x))
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Data comparison.
###
### A wrapper to the very fast memcmp() C-function.
### Arguments MUST be the following or it will crash R:
###   x1, x2: RawPtr objects
###   start1, start2, width: single integers
### In addition: 1 <= start1 <= start1+width-1 <= length(x1)
###              1 <= start2 <= start2+width-1 <= length(x2)
### WARNING: This function is voluntarly unsafe (it doesn't check its
### arguments) because we want it to be the fastest possible!
###

RawPtr.compare <- function(x1, start1, x2, start2, width)
{
    .Call("RawPtr_memcmp", x1, start1, x2, start2, width, PACKAGE="IRanges")
}

