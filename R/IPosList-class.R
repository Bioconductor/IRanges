### =========================================================================
### IPosList objects
### -------------------------------------------------------------------------


setClass("IPosList",
    contains="IntegerPosList",
    representation("VIRTUAL"),
    prototype(elementType="IPos")
)

setClass("SimpleIPosList",
    contains=c("IPosList", "SimpleIntegerPosList")
)

