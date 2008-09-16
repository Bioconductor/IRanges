## Only accepts IRanges instances as elements
setClass("IRangesCollection")

setMethod("rangeClass", "IRangesCollection", function(x) "IRanges")
