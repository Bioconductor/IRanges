### =========================================================================
### XNumeric objects
### -------------------------------------------------------------------------
###
### The XNumeric class is a container for storing an external sequence of
### numeric values (stored as double at the C level).
###

setClass("XNumeric",
    contains="XSequence",
    representation(
        xdata="NumericPtr"
    )
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Constructor.
###

XNumeric <- function(length=0L, initialize=FALSE)
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
        if (!storage.mode(values) == "double")
            storage.mode(values) <- "double"
        length <- length(values)
    }
    xdata <- NumericPtr(length=length, initialize=initialize)
    if (!is.null(values))
        xdata[] <- values
    new("XNumeric", xdata=xdata, offset=0L, length=length)
}

