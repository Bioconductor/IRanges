### =========================================================================
### 'map' and 'pmap' generics
### -------------------------------------------------------------------------
###

## Methods for 'map' in GenomicRanges are defunct.
## Use 'mapCoords' and 'pmapCoords' instead.

setGeneric("map", function(from, to, ...) standardGeneric("map"))

setGeneric("pmap", function(from, to, ...) standardGeneric("pmap"))
