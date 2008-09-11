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


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Accessor methods.
###

setMethod("length", "XSeq", function(x) x@length)

