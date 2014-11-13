### =========================================================================
### 'mapCoords' and 'pmapCoords' generics
### -------------------------------------------------------------------------
###
###

setGeneric("mapCoords", signature=c("from", "to"),
    function(from, to, ...) standardGeneric("mapCoords")
)

setGeneric("pmapCoords", signature=c("from", "to"),
    function(from, to, ...) standardGeneric("pmapCoords")
)
