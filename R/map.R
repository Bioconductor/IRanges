### =========================================================================
### 'map' and 'pmap' generics
### -------------------------------------------------------------------------
###
### Methods for 'map' that produces a GRangesMapping instance are 
### in GenomicRanges.
###

setGeneric("map", function(from, to, ...) standardGeneric("map"))

setGeneric("pmap", function(from, to, ...) standardGeneric("pmap"))

