### =========================================================================
### 'map' and 'pmap' generics
### -------------------------------------------------------------------------
###

## Methods for 'map' in GenomicRanges are deprecated.
## Use 'mapCoords' and 'pmapCoords' instead.

setGeneric("map", function(from, to, ...) standardGeneric("map"))

setGeneric("pmap", function(from, to, ...) standardGeneric("pmap"))
