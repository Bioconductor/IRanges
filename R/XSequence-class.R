### =========================================================================
### XSequence objects
### -------------------------------------------------------------------------
###
### The XSequence class is a general container for storing an "external
### sequence".
###

setClass("XSequence",
    representation(
        "VIRTUAL",
        xdata="SequencePtr",  # an external pointer to the "seed" data
        offset="integer",     # a single integer
        length="integer"      # a single integer
    ),
    prototype(
        offset=0L,
        length=0L
    )
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Accessor methods.
###

setMethod("length", "XSequence", function(x) x@length)

