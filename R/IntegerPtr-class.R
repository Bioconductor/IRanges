### =========================================================================
### External pointer to an integer vector: the "IntegerPtr" class
### -------------------------------------------------------------------------
###

setClass("IntegerPtr", contains="SequencePtr")


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Initialization.
###
### Note that, unlike 'integer(99)', 'IntegerPtr(99)' does NOT initialize its
### data. Specify the 'val' argument if you want data initialization.
###

setMethod("initialize", "IntegerPtr",
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
        .Call("IntegerPtr_new", length, val, PACKAGE="IRanges")
    }
)

IntegerPtr <- function(length=0L, val=NULL)
    new2("IntegerPtr", length=length, val=val, check=FALSE)

setMethod("show", "IntegerPtr",
    function(object)
    {
        show_string <- .Call("IntegerPtr_get_show_string", object, PACKAGE="IRanges")
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
###

IntegerPtr.read <- function(x, i, imax=integer(0))
{
    if (!is.integer(i))
        i <- as.integer(i)
    if (length(i) == 1) {
        if (length(imax) == 0)
            imax <- i
        else
            imax <- as.integer(imax)
        .Call("IntegerPtr_read_ints_from_i1i2", x, i, imax, PACKAGE="IRanges")
    } else {
        .Call("IntegerPtr_read_ints_from_subset", x, i, PACKAGE="IRanges")
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
        .Call("IntegerPtr_write_ints_to_i1i2", x, i, imax, value, PACKAGE="IRanges")
    } else {
        .Call("IntegerPtr_write_ints_to_subset", x, i, value, PACKAGE="IRanges")
    }
    x
}

IntegerPtr.copy <- function(dest, i, imax=integer(0), src)
{
    if (!is(src, "IntegerPtr"))
        stop("'src' must be an IntegerPtr object")
    if (!is.integer(i))
        i <- as.integer(i)
    if (length(i) == 1) {
        if (length(imax) == 0) 
            imax <- i
        else
            imax <- as.integer(imax)
        .Call("IntegerPtr_copy_from_i1i2",
              dest, src, i, imax, PACKAGE="IRanges")
    } else {
        .Call("IntegerPtr_copy_from_subset",
              dest, src, i, PACKAGE="IRanges")
    }
    dest
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion.
###

setMethod("as.integer", "IntegerPtr",
    function(x, ...) IntegerPtr.read(x, 1L, length(x))
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Data comparison.
###
### A wrapper to the very fast memcmp() C-function.
### Arguments MUST be the following or it will crash R:
###   x1, x2: "IntegerPtr" objects
###   start1, start2, width: single integers
### In addition: 1 <= start1 <= start1+width-1 <= length(x1)
###              1 <= start2 <= start2+width-1 <= length(x2)
### WARNING: This function is voluntarly unsafe (it doesn't check its
### arguments) because we want it to be the fastest possible!
###

IntegerPtr.compare <- function(x1, start1, x2, start2, width)
{
    .Call("IntegerPtr_memcmp", x1, start1, x2, start2, width, PACKAGE="IRanges")
}

