### =========================================================================
### IFWRangesList objects
### -------------------------------------------------------------------------


setClass("IFWRangesList",
    contains=c("FWRangesList", "IntegerRangesList"),
    representation("VIRTUAL"),
    prototype(elementType="IFWRanges")
)

setClass("SimpleIFWRangesList",
    contains=c("IFWRangesList", "SimpleFWRangesList", "SimpleIntegerRangesList")
)

