### =========================================================================
### SharedDouble objects
### -------------------------------------------------------------------------
###
### A SharedDouble object is an external pointer to an ordinary double
### vector.
###

setClass("SharedDouble", contains="SharedVector")


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Initialization.
###
### Note that, unlike 'numeric(99)', 'SharedDouble(99)' does NOT initialize its
### data. Specify the 'val' argument if you want data initialization.
###

SharedDouble <- function(length=0L, val=NULL)
{
    if (!isSingleNumber(length) || length < 0)
        stop("'length' must be a single non-negative integer")
    if (!is.integer(length))
        length <- as.integer(length)
    if (!is.null(val)) {
        if (!is.numeric(val))
            stop("'val' must be a numeric vector")
        if (!storage.mode(val) == "double")
            storage.mode(val) <- "double"
    }
    .Call2("SharedDouble_new", length, val, PACKAGE="IRanges")
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Validity.
###

.valid.SharedDouble <- function(x)
{
    if (!tagIsVector(x@xp, tagtype="double"))
        return(problemIfNotExternalVector("'x@xp'",
                                          tagmustbe="a double vector"))
    NULL
}

setValidity2("SharedDouble", .valid.SharedDouble)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Read/write functions.
### These are almost safe wrappers to unsafe C functions ("almost" because
### they don't check for NAs in their arguments).
### If length(i) == 0 then the read functions return an empty vector
### and the write functions don't do anything.
###

SharedDouble.read <- function(x, i, imax=integer(0))
{
    if (!is.integer(i))
        i <- as.integer(i)
    if (length(i) == 1) {
        if (length(imax) == 0)
            imax <- i
        else
            imax <- as.integer(imax)
        .Call2("SharedDouble_read_nums_from_i1i2", x, i, imax, PACKAGE="IRanges")
    } else {
        .Call2("SharedDouble_read_nums_from_subscript", x, i, PACKAGE="IRanges")
    }
}

SharedDouble.write <- function(x, i, imax=integer(0), value)
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
        .Call2("SharedDouble_write_nums_to_i1i2", x, i, imax, value, PACKAGE="IRanges")
    } else {
        .Call2("SharedDouble_write_nums_to_subscript", x, i, value, PACKAGE="IRanges")
    }
    x
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion.
###

setMethod("as.numeric", "SharedDouble",
    function(x, ...) SharedDouble.read(x, 1L, length(x))
)

