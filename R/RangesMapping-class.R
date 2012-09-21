### =========================================================================
### RangesMapping objects
### -------------------------------------------------------------------------
###
### A RangesMapping encodes a mapping of a set of ranges to some other
### coordinate space.
###

## Conceptually, a RangesMapping is a matching of each query range to
## one or more elements in a subject. The geometry of the query range
## is then transformed according to that matching. Thus, this data
## class combines a Hits object with a set of transformed ranges.

## In IRanges, we do not have any tabular structure that links a space
## with an interval, except for RangedData. We could use a RangedData
## in place of this class, but that might be too low-level. Instead,
## we have an accessor for each, and support a coercion.

setClass("RangesMapping",
         representation(hits = "Hits", space = "Rle",
                        ranges = "Ranges"))

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Accessors
###

hits <- function(x) x@hits

setMethod("space", "RangesMapping", function(x) x@space)

setMethod("ranges", "RangesMapping", function(x) x@ranges)

setMethod("dim", "RangesMapping", function(x) dim(hits(x)))

setMethod("length", "RangesMapping", function(x) length(ranges(x)))

setMethod("subjectHits", "RangesMapping", function(x) subjectHits(hits(x)))

setMethod("queryHits", "RangesMapping", function(x) queryHits(hits(x)))

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion
###

setAs("RangesMapping", "RangedData", function(from) {
  RangedData(ranges(from), space = space(from), as(hits(from), "DataFrame"))
})

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The 'map' generic, methods of which produce instances of this class
###

setGeneric("map", function(from, to, ...) standardGeneric("map"))

setGeneric("pmap", function(from, to, ...) standardGeneric("pmap"))

