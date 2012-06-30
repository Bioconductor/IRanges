### =========================================================================
### SharedRaw objects and SharedRaw_Pool objects
### -------------------------------------------------------------------------
###
### A SharedRaw object is an external pointer to an ordinary raw vector.
### A SharedRaw_Pool object is *conceptually* a list of SharedRaw
### objects but is actually NOT *implemented* as a list of such objects.
### See SharedVector-class.R file for the representation details.
###

setClass("SharedRaw", contains="SharedVector")

setClass("SharedRaw_Pool", contains="SharedVector_Pool")


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Initialization.
###

### Note that, unlike 'raw(99)', 'SharedRaw(99)' does NOT initialize its
### data. Specify the 'val' argument if you want data initialization.
SharedRaw <- function(length=0L, val=NULL)
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
    .Call2("SharedRaw_new", length, val, PACKAGE="IRanges")
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Some low-level methods.
###

setMethod("[[", "SharedRaw_Pool",
    function(x, i, j, ...)
    {
        if (!isSingleInteger(i) || i < 1L || i > length(x))
            stop("invalid subscript")
        ans <- SharedRaw()
        ans@xp <- x@xp_list[[i]]
        ans@.link_to_cached_object <- x@.link_to_cached_object_list[[i]]
        ans
    }
)

setReplaceMethod("[[", "SharedRaw_Pool",
    function(x, i, j, ..., value)
    {
        if (!isSingleInteger(i) || i < 1L || i > length(x))
            stop("invalid subscript")
        if (class(value) != "SharedRaw")
            stop("replacement value must be a SharedRaw instance")
        x@xp_list[[i]] <- value@xp
        x@.link_to_cached_object_list[[i]] <- value@.link_to_cached_object
        x
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Validity.
###

.valid.SharedRaw <- function(x)
{
    if (!tagIsVector(x@xp, tagtype="raw"))
        return(problemIfNotExternalVector("'x@xp'",
                                          tagmustbe="a raw vector"))
    NULL
}

setValidity2("SharedRaw", .valid.SharedRaw)

.valid.SharedRaw_Pool <- function(x)
{
    if (!all(sapply(x@xp_list,
                    function(elt) tagIsVector(elt, tagtype="raw"))))
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
        .Call2("SharedRaw_read_ints_from_i1i2", x, i, imax, PACKAGE="IRanges")
    } else {
        .Call2("SharedRaw_read_ints_from_subscript", x, i, PACKAGE="IRanges")
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
        .Call2("SharedRaw_write_ints_to_i1i2", x, i, imax, value, PACKAGE="IRanges")
    } else {
        .Call2("SharedRaw_write_ints_to_subscript", x, i, value, PACKAGE="IRanges")
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
            .Call2("SharedRaw_read_chars_from_i1i2",
                  x, i, imax, PACKAGE="IRanges")
        else
            .Call2("SharedRaw_read_enc_chars_from_i1i2",
                  x, i, imax, dec_lkup, PACKAGE="IRanges")
    } else {
        if (is.null(dec_lkup))
            .Call2("SharedRaw_read_chars_from_subscript",
                  x, i, PACKAGE="IRanges")
        else
            .Call2("SharedRaw_read_enc_chars_from_subscript",
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
            .Call2("SharedRaw_write_chars_to_i1i2",
                  x, i, imax, value, PACKAGE="IRanges")
        else
            .Call2("SharedRaw_write_enc_chars_to_i1i2",
                  x, i, imax, value, enc_lkup, PACKAGE="IRanges")
    } else {
        if (is.null(enc_lkup))
            .Call2("SharedRaw_write_chars_to_subscript",
                  x, i, value, PACKAGE="IRanges")
        else
            .Call2("SharedRaw_write_enc_chars_to_subscript",
                  x, i, value, enc_lkup, PACKAGE="IRanges")
    }
    x
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
        .Call2("SharedRaw_read_complexes_from_i1i2",
              x, i, imax, lkup, PACKAGE="IRanges")
    } else {
        .Call2("SharedRaw_read_complexes_from_subscript",
              x, i, lkup, PACKAGE="IRanges")
    }
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
### This should always rewrite the content of a SharedRaw object
### to itself, without any modification:
###   SharedRaw.write(x, 1, length(x), value=toString(x))
### whatever the content of 'x' is!
setMethod("toString", "SharedRaw",
    function(x, ...) SharedRaw.read(x, 1, length(x))
)

