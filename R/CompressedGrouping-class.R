### =========================================================================
### CompressedGrouping objects
### -------------------------------------------------------------------------

setClass("CompressedGrouping",
### TODO: contain VIRTUAL after R 3.4 release
         contains=c("Grouping", "CompressedIntegerList"))

setClass("CompressedManyToOneGrouping",
         contains=c("ManyToOneGrouping", "CompressedGrouping"))

setClass("CompressedManyToManyGrouping",
         contains=c("BaseManyToManyGrouping", "CompressedGrouping"))

### -------------------------------------------------------------------------
### Grouping API implementation
### ----------------------------
###

setMethod("grouplengths", "CompressedGrouping",
          function(x, i=NULL) grouplengths(PartitioningByEnd(x), i))

setMethod("nobj", "CompressedManyToOneGrouping",
          function(x) nobj(PartitioningByEnd(x)))

