### =========================================================================
### 'mapCoords' and 'pmapCoords' generics
### -------------------------------------------------------------------------
###
###

setGeneric("mapCoords", signature=c("x", "to"),
    function(x, to, ...) standardGeneric("mapCoords")
)

setGeneric("pmapCoords", signature=c("x", "to"),
    function(x, to, ...) standardGeneric("pmapCoords")
)
