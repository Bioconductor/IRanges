### =========================================================================
### Binning objects
### -------------------------------------------------------------------------
###

setClass("Binning",
    contains="Grouping",
    representation(
        bins="CompressedIntegerList"
    )
)

setMethod("length", "Binning", function(x) length(x@bins))

setValidity2("Binning",
    function(x) .Defunct(msg="the Binning class is defunct")
)

Binning <- function(group=integer(), names=NULL)
{
    .Defunct(msg="the Binning class is defunct")
}

