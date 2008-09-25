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

setAs("numeric", "XNumeric", function(from) {
  XNumeric(length(from), val = from)
})

setAs("numeric", "XSequence", function(from) {
  as(from, "XNumeric")
})

setMethod("as.numeric", "XNumeric",
          function(x) NumericPtr.read(x@xdata, x@offset + 1L,
                                      x@offset + x@length))

setMethod("as.vector", c("XNumeric", "missing"),
          function(x, mode) as.numeric(x))
