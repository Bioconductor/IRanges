### =========================================================================
### SharedRaw objects and SharedRaw_Pool objects
### -------------------------------------------------------------------------
###
### A SharedRaw object is an external pointer to an ordinary raw vector.
###

setClass("SharedRaw", contains="SharedVector")

setClass("SharedRaw_Pool", contains="SharedVector_Pool")


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Initialization.
###
### Note that, unlike 'raw(99)', 'SharedRaw(99)' does NOT initialize its
### data. Specify the 'val' argument if you want data initialization.
###

setMethod("initialize", "SharedRaw",
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
        .Call("SharedRaw_new", length, val, PACKAGE="IRanges")
    }
)

SharedRaw <- function(length=0L, val=NULL)
    new2("SharedRaw", length=length, val=val, check=FALSE)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Some low-level methods.
###

setMethod("address0", "SharedRaw",
    function(x) .Call("SharedRaw_address0", x, PACKAGE="IRanges")
)

setMethod("[[", "SharedRaw_Pool",
    function(x, i, j, ...)
    {
        if (!isSingleInteger(i) || i < 1L || i > length(x))
            stop("invalid subscript")
        ans <- SharedRaw()
        ans@xp <- x@xp_list[[i]]
        ans@.link_to_cached_object <- x@.link_to_cached_object_list[[i]]
        return(ans)
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Validity.
###

.valid.SharedRaw <- function(x)
{
    if (!isExternalVector(x@xp, tagtype="raw"))
        return(problemIfNotExternalVector("'x@xp'",
                                          tagmustbe="a raw vector"))
    NULL
}

setValidity2("SharedRaw", .valid.SharedRaw)

.valid.SharedRaw_Pool <- function(x)
{
    if (!all(sapply(x@xp_list,
                    function(elt) isExternalVector(elt, tagtype="raw"))))
        return(problemIfNotExternalVector("each element in 'x@xp_list'",
                                          tagmustbe="a raw vector"))
    NULL
}

setValidity2("SharedRaw_Pool", .valid.SharedRaw_Pool)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Read/write functions.
### These are almost safe wrappers to unsafe C functions ("almost" because
### they don't check for NAs in their arguments).
### If length(i) == 0 then the read functions return an empty vector
### and the write functions don't do anything.

SharedRaw.readInts <- function(x, i, imax=integer(0))
{
    if (!is.integer(i))
        i <- as.integer(i)
    if (length(i) == 1) {
        if (length(imax) == 0)
            imax <- i
        else
            imax <- as.integer(imax)
        .Call("SharedRaw_read_ints_from_i1i2", x, i, imax, PACKAGE="IRanges")
    } else {
        .Call("SharedRaw_read_ints_from_subset", x, i, PACKAGE="IRanges")
    }
}

SharedRaw.writeInts <- function(x, i, imax=integer(0), value)
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
        .Call("SharedRaw_write_ints_to_i1i2", x, i, imax, value, PACKAGE="IRanges")
    } else {
        .Call("SharedRaw_write_ints_to_subset", x, i, value, PACKAGE="IRanges")
    }
    x
}

### 'dec_lkup' must be NULL or a vector of integers
SharedRaw.read <- function(x, i, imax=integer(0), dec_lkup=NULL)
{
    if (!is.integer(i))
        i <- as.integer(i)
    if (length(i) == 1) {
        if (length(imax) == 0)
            imax <- i
        else
            imax <- as.integer(imax)
        if (is.null(dec_lkup))
            .Call("SharedRaw_read_chars_from_i1i2",
                  x, i, imax, PACKAGE="IRanges")
        else
            .Call("SharedRaw_read_enc_chars_from_i1i2",
                  x, i, imax, dec_lkup, PACKAGE="IRanges")
    } else {
        if (is.null(dec_lkup))
            .Call("SharedRaw_read_chars_from_subset",
                  x, i, PACKAGE="IRanges")
        else
            .Call("SharedRaw_read_enc_chars_from_subset",
                  x, i, dec_lkup, PACKAGE="IRanges")
    }
}

### 'enc_lkup' must be NULL or a vector of integers
SharedRaw.write <- function(x, i, imax=integer(0), value, enc_lkup=NULL)
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
            .Call("SharedRaw_write_chars_to_i1i2",
                  x, i, imax, value, PACKAGE="IRanges")
        else
            .Call("SharedRaw_write_enc_chars_to_i1i2",
                  x, i, imax, value, enc_lkup, PACKAGE="IRanges")
    } else {
        if (is.null(enc_lkup))
            .Call("SharedRaw_write_chars_to_subset",
                  x, i, value, PACKAGE="IRanges")
        else
            .Call("SharedRaw_write_enc_chars_to_subset",
                  x, i, value, enc_lkup, PACKAGE="IRanges")
    }
    x
}

### 'lkup' must be NULL or a vector of integers
SharedRaw.copy <- function(dest, i, imax=integer(0), src, lkup=NULL)
{
    if (!is(src, "SharedRaw"))
        stop("'src' must be an SharedRaw object")
    if (!is.integer(i))
        i <- as.integer(i)
    if (length(i) == 1) {
        if (length(imax) == 0)
            imax <- i
        else
            imax <- as.integer(imax)
        if (is.null(lkup))
            .Call("SharedRaw_copy_from_i1i2",
                  dest, src, i, imax, PACKAGE="IRanges")
        else
            .Call("SharedRaw_translate_copy_from_i1i2",
                   dest, src, i, imax, lkup, PACKAGE="IRanges")
    } else {
        if (is.null(lkup))
            .Call("SharedRaw_copy_from_subset",
                  dest, src, i, PACKAGE="IRanges")
        else
            .Call("SharedRaw_translate_copy_from_subset",
                  dest, src, i, lkup, PACKAGE="IRanges")
    }
    dest
}

### 'lkup' must be NULL or a vector of integers
SharedRaw.reverseCopy <- function(dest, i, imax=integer(0), src, lkup=NULL)
{
    if (!is(src, "SharedRaw"))
        stop("'src' must be an SharedRaw object")
    if (length(i) != 1)
        stop("'i' must be a single integer")
    if (!is.integer(i))
        i <- as.integer(i)
    if (length(imax) == 0)
        imax <- i
    else
        imax <- as.integer(imax)
    if (is.null(lkup))
        .Call("SharedRaw_reverse_copy_from_i1i2",
              dest, src, i, imax, PACKAGE="IRanges")
    else
        .Call("SharedRaw_reverse_translate_copy_from_i1i2",
              dest, src, i, imax, lkup, PACKAGE="IRanges")
    dest
}

### 'lkup' must be a vector of complexes
SharedRaw.readComplexes <- function(x, i, imax=integer(0), lkup)
{
    if (!is.integer(i))
        i <- as.integer(i)
    if (length(i) == 1) {
        if (length(imax) == 0)
            imax <- i
        else
            imax <- as.integer(imax)
        .Call("SharedRaw_read_complexes_from_i1i2",
              x, i, imax, lkup, PACKAGE="IRanges")
    } else {
        .Call("SharedRaw_read_complexes_from_subset",
              x, i, lkup, PACKAGE="IRanges")
    }
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "SharedRaw.append" function.
###

SharedRaw.append <- function(x1, start1, width1, x2, start2, width2)
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
    ans <- SharedRaw(ans_len)
    .Call("SharedRaw_memcpy", ans, 1L, x1, start1, width1, PACKAGE="IRanges")
    .Call("SharedRaw_memcpy", ans, 1L + width1, x2, start2, width2, PACKAGE="IRanges")
    ans
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion.
###
### TODO: add the "as.raw" and "as.character" methods.
###

setMethod("as.integer", "SharedRaw",
    function(x, ...)
    {
        SharedRaw.readInts(x, 1L, length(x))
    }
)

### Typical use:
###   x <- SharedRaw(15, as.raw(65))
###   toString(x)
###   x <- SharedRaw(5, charToRaw("Hello"))
###   toString(x)
### This should always rewrite the content of an SharedRaw object
### to itself, without any modification:
###   SharedRaw.write(x, 1, length(x), value=toString(x))
### whatever the content of 'x' is!
setMethod("toString", "SharedRaw",
    function(x, ...)
    {
        SharedRaw.read(x, 1, length(x))
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Data comparison.
###
### A wrapper to the very fast memcmp() C-function.
### Arguments MUST be the following or it will crash R:
###   x1, x2: SharedRaw objects
###   start1, start2, width: single integers
### In addition: 1 <= start1 <= start1+width-1 <= length(x1)
###              1 <= start2 <= start2+width-1 <= length(x2)
### WARNING: This function is voluntarly unsafe (it doesn't check its
### arguments) because we want it to be the fastest possible!
###

SharedRaw.compare <- function(x1, start1, x2, start2, width)
{
    .Call("SharedRaw_memcmp", x1, start1, x2, start2, width, PACKAGE="IRanges")
}

