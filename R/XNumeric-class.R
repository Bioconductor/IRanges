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

setMethod("initialize", "XNumeric",
    function(.Object, length=0L, val=NULL)
    {
        if (!isSingleNumber(length) || length < 0)
            stop("'length' must be a single non-negative integer")
        if (!is.integer(length))
            length <- as.integer(length)
        .Object@xdata <- NumericPtr(length=length, val=val)
        .Object@offset <- 0L
        .Object@length <- length
        .Object
    }
)

XNumeric <- function(length=0L, val=NULL)
    new("XNumeric", length=length, val=val)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion
###

### From standard vectors to XNumeric objects:
setAs("numeric", "XNumeric", function(from) XNumeric(length(from), val=from))
setAs("numeric", "XSequence", function(from) as(from, "XNumeric"))

### From XNumeric objects to standard vectors:
setMethod("as.numeric", "XNumeric",
    function(x, ...) NumericPtr.read(x@xdata, x@offset + 1L, x@offset + x@length)
)
setMethod("as.vector", c("XNumeric", "missing"),
    function(x, mode) as.numeric(x)
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Subsetting.
###

setMethod("[", "XNumeric",
          function(x, i, j, ..., drop=TRUE)
          {
            if (!missing(j) || length(list(...)) > 0)
              stop("invalid subsetting")
            if (missing(i)) {
              if (!drop)
                return(x)
              return(as.numeric(x@xdata))
            }
            if (!is.numeric(i) || any(is.na(i)))
              stop("invalid subsetting")
            if (any(i < 1) || any(i > length(x)))
              stop("subscript out of bounds")
            if (drop)
              return(NumericPtr.read(x@xdata, x@offset + i))
            xdata <- NumericPtr(length(i))
            NumericPtr.copy(xdata, x@offset + i, src=x@xdata)
            x@xdata <- xdata
            x@offset <- 0L
            x@length <- length(xdata)
            x
          }
          )

