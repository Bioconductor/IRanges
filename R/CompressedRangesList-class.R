### =========================================================================
### CompressedRangesList objects
### -------------------------------------------------------------------------
###

setClass("CompressedRangesList",
    contains=c("RangesList", "CompressedList"),
    representation("VIRTUAL")
)

setClass("CompressedPosList",
    contains=c("PosList", "CompressedRangesList"),
    representation("VIRTUAL")
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### start(), end(), width(), and pos() getters
###

setMethod("start", "CompressedRangesList",
    function(x) relist(start(unlist(x, use.names=FALSE)), x)
)

setMethod("end", "CompressedRangesList",
    function(x) relist(end(unlist(x, use.names=FALSE)), x)
)

setMethod("width", "CompressedRangesList",
    function(x) relist(width(unlist(x, use.names=FALSE)), x)
)

setMethod("pos", "CompressedPosList",
    function(x) relist(pos(unlist(x, use.names=FALSE)), x)
)

