### =========================================================================
### XRaw objects
### -------------------------------------------------------------------------
###
### The XRaw class is a container for storing an external sequence of
### bytes (stored as char values at the C level).
###

setClass("XRaw",
    contains="XSequence",
    representation(
        xdata="RawPtr"
    )
)

