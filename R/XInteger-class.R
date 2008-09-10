### =========================================================================
### XInteger objects
### -------------------------------------------------------------------------
###
### The XInteger class is a container for storing an external sequence of
### integers.
###

setClass("XInteger",
    contains="XSeq",
    representation(
        xdata="IntegerPtr"
    )
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Constructor.
###

XInteger <- function(length=0L, initialize=FALSE)
{
    if (isSingleNumber(length)) {
        if (!is.integer(length))
            length <- as.integer(length)
        if (length < 0)
            stop("'length' cannot be negative")
        values <- NULL
    } else {
        values <- length
        if (!is.numeric(values))
            stop("values must be numeric")
        if (!is.integer(values))
            values <- as.integer(values)
        length <- length(values)
    }
    xdata <- IntegerPtr(length=length, initialize=initialize)
    if (!is.null(values))
        xdata[] <- values
    new("XInteger", xdata=xdata, offset=0L, length=length)
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Subsetting.
###

### FIXME: Take @offset and @length into account!
### FIXME: Currently return a non initialized XInteger object of length i
###        if length(i) == 1 and drop=FALSE! 
setMethod("[", "XInteger",
    function(x, i, j, ..., drop=TRUE)
    {
        if (!missing(j) || length(list(...)) > 0)
            stop("invalid subsetting")
        if (missing(i)) {
            if (!drop)
                return(x)
            return(as.integer(x@xdata))
        }
        ans <- IntegerPtr.read(x@xdata, i)
        if (!drop)
            ans <- XInteger(ans)
        ans
    }
)

setReplaceMethod("[", "XInteger",
    function(x, i, j,..., value)
        stop("attempt to modify the value of a ", class(x), " instance")
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "show" method.
###

### FIXME: Take @offset and @length into account!
toNumSnippet <- function(x, width = getOption("width"))
{
    if (is(x, "XInteger"))
        x <- x@xdata
    width <- max(0, width - 4)
    element_length <- format.info(x[seq_len(min(length(x), width %/% 2))])[1] + 1
    number_of_elements <- min(length(x), width %/% element_length)
    if (number_of_elements == length(x))
        ending <- ""
    else
        ending <- " ..."
        paste(paste(format(x[seq_len(number_of_elements)]), collapse = " "), ending, sep = "")
}

setMethod("show", "XInteger",
    function(object)
    {
        lo <- length(object)
        cat("  ", lo, "-integer \"", class(object), "\" instance\n", sep="")
        cat(" [1] ")
        cat(toNumSnippet(object, getOption("width") - 4))
        cat("\n")
        ## What is correct here? The documentation (?show) says that 'show'
        ## should return an invisible 'NULL' but, on the other hand, the 'show'
        ## method for intergers returns its 'object' argument...
        invisible(object)
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Comparison.
###

### FIXME: Compare the contents, not the addresses!
setMethod("==", signature(e1="XInteger", e2="XInteger"),
    function(e1, e2) { e1@xdata == e2@xdata }
)

setMethod("!=", signature(e1="XInteger", e2="XInteger"),
    function(e1, e2) { e1@xdata != e2@xdata }
)

