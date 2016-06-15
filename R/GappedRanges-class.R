### =========================================================================
### GappedRanges objects  DEFUNCT!  DEFUNCT!  DEFUNCT!  DEFUNCT!  DEFUNCT!
### -------------------------------------------------------------------------
###

setClass("GappedRanges",
    contains="Ranges",
    representation(cnirl="CompressedNormalIRangesList"),
    prototype(elementType="NormalIRanges")
)

setMethod("length", "GappedRanges",
    function(x) .Defunct(msg="GappedRanges objects are defunct")
)

setMethod("start", "GappedRanges",
    function(x, ...) .Defunct(msg="GappedRanges objects are defunct")
)

setMethod("end", "GappedRanges",
    function(x, ...) .Defunct(msg="GappedRanges objects are defunct")
)

setGeneric("ngap", function(x) standardGeneric("ngap"))
setMethod("ngap", "GappedRanges",
    function(x) .Defunct(msg="GappedRanges objects are defunct")
)

setMethod("names", "GappedRanges",
    function(x) .Defunct(msg="GappedRanges objects are defunct")
)

setReplaceMethod("names", "GappedRanges",
    function(x, value) .Defunct(msg="GappedRanges objects are defunct")
)

.valid.GappedRanges <- function(x)
    .Defunct(msg="GappedRanges objects are defunct")

setValidity2("GappedRanges", .valid.GappedRanges)

setAs("CompressedNormalIRangesList", "GappedRanges",
    function(from) new("GappedRanges", cnirl=from)
)
setAs("CompressedIRangesList", "GappedRanges",
    function(from) as(as(from, "CompressedNormalIRangesList"), "GappedRanges")
)

setAs("GappedRanges", "CompressedNormalIRangesList", function(from) from@cnirl)
setAs("GappedRanges", "NormalIRangesList", function(from) from@cnirl)
setAs("GappedRanges", "CompressedIRangesList", function(from) from@cnirl)
setAs("GappedRanges", "IRangesList", function(from) from@cnirl)
setAs("GappedRanges", "RangesList", function(from) from@cnirl)

.as.data.frame.GappedRanges <- function(x, row.names=NULL, optional=FALSE, ...)
{
    ans <- callNextMethod(unname(x), row.names, optional, ...)
    ans$ngap <- ngap(x)
    ans$names <- names(x)
    ans
}
setMethod("as.data.frame", "GappedRanges", .as.data.frame.GappedRanges)

setMethod("show", "GappedRanges",
    function(object) .Defunct(msg="GappedRanges objects are defunct")
)

setMethod("getListElement", "GappedRanges",
    function(x, i, exact=TRUE) .Defunct(msg="GappedRanges objects are defunct")
)

setMethod("elementNROWS", "GappedRanges",
    function(x) .Defunct(msg="GappedRanges objects are defunct")
)

setMethod("extractROWS", "GappedRanges",
    function(x, i) .Defunct(msg="GappedRanges objects are defunct")
)

setMethod("c", "GappedRanges",
    function(x, ..., recursive=FALSE)
        .Defunct(msg="GappedRanges objects are defunct")
)

