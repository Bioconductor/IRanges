### =========================================================================
### IPosList objects
### -------------------------------------------------------------------------


setClass("IPosList",
    contains=c("PosList", "IntegerRangesList"),
    representation("VIRTUAL"),
    prototype(elementType="IPos")
)

setClass("SimpleIPosList",
    contains=c("IPosList", "SimplePosList", "SimpleIntegerRangesList")
)

