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

setMethod("initialize", "SharedDouble",
    function(.Object, length=0L, val=NULL)
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
        .Call("SharedDouble_new", length, val, PACKAGE="IRanges")
    }
)

SharedDouble <- function(length=0L, val=NULL)
    new2("SharedDouble", length=length, val=val, check=FALSE)

setMethod("show", "SharedDouble",
    function(object)
    {
        show_string <- .Call("SharedDouble_get_show_string", object, PACKAGE="IRanges")
        cat(show_string, "\n", sep="")
        ## What is correct here? The documentation (?show) says that 'show'
        ## should return an invisible 'NULL' but, on the other hand, the 'show'
        ## method for numerics returns its 'object' argument...
        invisible(object)
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Validity.
###

.valid.SharedDouble <- function(x)
{
    if (!extends(typeoftag(x@xp), "double"))
        return("'x@xp' must be an external pointer to a double vector")
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
        .Call("SharedDouble_read_nums_from_i1i2", x, i, imax, PACKAGE="IRanges")
    } else {
        .Call("SharedDouble_read_nums_from_subset", x, i, PACKAGE="IRanges")
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
        .Call("SharedDouble_write_nums_to_i1i2", x, i, imax, value, PACKAGE="IRanges")
    } else {
        .Call("SharedDouble_write_nums_to_subset", x, i, value, PACKAGE="IRanges")
    }
    x
}

SharedDouble.copy <- function(dest, i, imax=integer(0), src)
{
  if (!is(src, "SharedDouble"))
    stop("'src' must be an SharedDouble object")
  if (!is.integer(i))
    i <- as.integer(i)
  if (length(i) == 1) {
    if (length(imax) == 0) 
      imax <- i
    else
      imax <- as.integer(imax)
    .Call("SharedDouble_copy_from_i1i2",
          dest, src, i, imax, PACKAGE="IRanges")
  } else {
    .Call("SharedDouble_copy_from_subset",
          dest, src, i, PACKAGE="IRanges")
  }
  dest
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion.
###

setMethod("as.numeric", "SharedDouble",
    function(x, ...) SharedDouble.read(x, 1L, length(x))
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Data comparison.
###
### A wrapper to the very fast memcmp() C-function.
### Arguments MUST be the following or it will crash R:
###   x1, x2: "SharedDouble" objects
###   start1, start2, width: single integers
### In addition: 1 <= start1 <= start1+width-1 <= length(x1)
###              1 <= start2 <= start2+width-1 <= length(x2)
### WARNING: This function is voluntarly unsafe (it doesn't check its
### arguments) because we want it to be the fastest possible!
###

SharedDouble.compare <- function(x1, start1, x2, start2, width)
{
    .Call("SharedDouble_memcmp", x1, start1, x2, start2, width, PACKAGE="IRanges")
}

