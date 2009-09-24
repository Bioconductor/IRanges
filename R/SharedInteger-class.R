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

setMethod("initialize", "SharedInteger",
    function(.Object, length=0L, val=NULL)
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
        .Call("SharedInteger_new", length, val, PACKAGE="IRanges")
    }
)

SharedInteger <- function(length=0L, val=NULL)
    new2("SharedInteger", length=length, val=val, check=FALSE)

setMethod("show", "SharedInteger",
    function(object)
    {
        show_string <- .Call("SharedInteger_get_show_string", object, PACKAGE="IRanges")
        cat(show_string, "\n", sep="")
        ## What is correct here? The documentation (?show) says that 'show'
        ## should return an invisible 'NULL' but, on the other hand, the 'show'
        ## method for integers returns its 'object' argument...
        invisible(object)
    }
)


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
        .Call("SharedInteger_read_ints_from_i1i2", x, i, imax, PACKAGE="IRanges")
    } else {
        .Call("SharedInteger_read_ints_from_subset", x, i, PACKAGE="IRanges")
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
        .Call("SharedInteger_write_ints_to_i1i2", x, i, imax, value, PACKAGE="IRanges")
    } else {
        .Call("SharedInteger_write_ints_to_subset", x, i, value, PACKAGE="IRanges")
    }
    x
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion.
###

setMethod("as.integer", "SharedInteger",
    function(x, ...) SharedInteger.read(x, 1L, length(x))
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Data comparison.
###
### A wrapper to the very fast memcmp() C-function.
### Arguments MUST be the following or it will crash R:
###   x1, x2: "SharedInteger" objects
###   start1, start2, width: single integers
### In addition: 1 <= start1 <= start1+width-1 <= length(x1)
###              1 <= start2 <= start2+width-1 <= length(x2)
### WARNING: This function is voluntarly unsafe (it doesn't check its
### arguments) because we want it to be the fastest possible!
###

SharedInteger.compare <- function(x1, start1, x2, start2, width)
{
    .Call("SharedInteger_memcmp", x1, start1, x2, start2, width, PACKAGE="IRanges")
}

