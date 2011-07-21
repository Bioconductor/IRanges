### =========================================================================
### SharedInteger objects
### -------------------------------------------------------------------------
###
### A SharedInteger object is an external pointer to an ordinary integer
### vector.
###

setClass("SharedInteger", contains="SharedVector")


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Initialization.
###
### Note that, unlike 'integer(99)', 'SharedInteger(99)' does NOT initialize its
### data. Specify the 'val' argument if you want data initialization.
###

SharedInteger <- function(length=0L, val=NULL)
{
    if (!isSingleNumber(length) || length < 0)
        stop("'length' must be a single non-negative integer")
    if (!is.integer(length))
        length <- as.integer(length)
    if (!is.null(val)) {
        if (!is.numeric(val))
            stop("'val' must be a numeric vector")
        if (!storage.mode(val) == "integer")
            storage.mode(val) <- "integer"
    }
    .Call2("SharedInteger_new", length, val, PACKAGE="IRanges")
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Validity.
###

.valid.SharedInteger <- function(x)
{
    if (!tagIsVector(x@xp, tagtype="integer"))
        return(problemIfNotExternalVector("'x@xp'",
                                          tagmustbe="an integer vector"))
    NULL
}

setValidity2("SharedInteger", .valid.SharedInteger)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Read/write functions.
### These are almost safe wrappers to unsafe C functions ("almost" because
### they don't check for NAs in their arguments).
### If length(i) == 0 then the read functions return an empty vector
### and the write functions don't do anything.
###

SharedInteger.read <- function(x, i, imax=integer(0))
{
    if (!is.integer(i))
        i <- as.integer(i)
    if (length(i) == 1) {
        if (length(imax) == 0)
            imax <- i
        else
            imax <- as.integer(imax)
        .Call2("SharedInteger_read_ints_from_i1i2", x, i, imax, PACKAGE="IRanges")
    } else {
        .Call2("SharedInteger_read_ints_from_subscript", x, i, PACKAGE="IRanges")
    }
}

SharedInteger.write <- function(x, i, imax=integer(0), value)
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
        .Call2("SharedInteger_write_ints_to_i1i2", x, i, imax, value, PACKAGE="IRanges")
    } else {
        .Call2("SharedInteger_write_ints_to_subscript", x, i, value, PACKAGE="IRanges")
    }
    x
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion.
###

setMethod("as.integer", "SharedInteger",
    function(x, ...) SharedInteger.read(x, 1L, length(x))
)

