### =========================================================================
### XRawList objects
### -------------------------------------------------------------------------
###
### An XRawList object is *conceptually* a list of XRaw objects
### but is actually not *implemented* as a list of such objects.
### This is to avoid having to generate long lists of S4 objects which the
### current S4 implementation is *very* inefficient at.
###

setClass("XRawList",
    contains="XVectorList",
    representation(
        pool="SharedRaw_Pool"
    ),
    prototype(
        elementType="XRaw"
    )
)

