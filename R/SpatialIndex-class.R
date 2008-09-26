### =========================================================================
### SpatialIndex objects
### -------------------------------------------------------------------------

setClass("SpatialIndex", contains = "VIRTUAL")

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Generics to be implemented by SpatialIndex derivatives
###

## find objects in the index that overlap those in a query set
setGeneric("overlap", function(object, query, ...) standardGeneric("overlap"))

