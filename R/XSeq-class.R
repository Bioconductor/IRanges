### =========================================================================
### XSeq objects
### -------------------------------------------------------------------------
###
### The XSeq class is a general container for storing an "external sequence".
###

setClassUnion("VectorPtr", c("RawPtr", "IntegerPtr", "NumericPtr"))

setClass("XSeq",
    representation(
        "VIRTUAL",
        xdata="VectorPtr",  # an external pointer to the "seed" data
        offset="integer",   # a single integer
        length="integer"    # a single integer
    ),
    prototype(
        offset=0L,
        length=0L
    )
)


### Move this definition to the upcoming XRaw-class.R
setClass("XRaw",
    contains="XSeq",
    representation(
        xdata="RawPtr"
    )
)

### Move this definition to the upcoming XNumeric-class.R
setClass("XNumeric",
    contains="XSeq",
    representation(
        xdata="NumericPtr"
    )
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Accessor methods.
###

setMethod("length", "XSeq", function(x) x@length)

