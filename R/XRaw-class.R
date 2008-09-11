### =========================================================================
### XRaw objects
### -------------------------------------------------------------------------
###
### The XRaw class is a container for storing an external sequence of
### bytes (stored as char values at the C level).
###

#setClass("XRaw",
#    contains="XSequence",
#    representation(
#        xdata="RawPtr"
#    )
#)

### A temporary "redirection" of the XRaw class to the RawPtr class. Only
### during the time needed to migrate all the BSgenome data packages to the
### new RawPtr class.
### TODO: Implement the real XRaw class once all the BSgenome data packages
### are fixed.
setClass("XRaw", contains="RawPtr")

