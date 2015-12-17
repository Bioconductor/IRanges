### =========================================================================
### CompressedHitsList objects
### -------------------------------------------------------------------------

### [H.P. - 2015/12/17] Why do we need this? Where is it used? Note that the
### CompressedHitsList representation is possible only in the rare situation
### where the concatenation of all the list elements results in a Hits object
### that is sorted by queryHits (this is a constraint for any Hits object).
### This relies so much on luck that I wonder if it makes sense to use the
### CompressedList trick to represent a list of Hits objects.
### TODO: Maybe deprecate this.

setClass("CompressedHitsList",
    prototype = prototype(elementType = "Hits",
                          unlistData = new("Hits")),
    contains="CompressedList")


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Accessors
###

setMethod("subjectHits", "CompressedHitsList", function(x) subjectHits(x@unlistData))

setMethod("queryHits", "CompressedHitsList", function(x) queryHits(x@unlistData))

setMethod("queryLength", "CompressedHitsList", function(x) queryLength(x@unlistData))
setMethod("subjectLength", "CompressedHitsList", function(x) subjectLength(x@unlistData))


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Constructor
###

CompressedHitsList <- function(hits, query)
{
  if (!(is(query, "CompressedIRangesList")))
    stop("'query' must be a 'CompressedIRangesList' object")
  if (!is(hits, "Hits"))
    stop("'hits' must be a 'Hits' object")

  qspace <- space(query)
  hspace <- as.integer(qspace[queryHits(hits)])
  partitioning <- PartitioningByEnd(hspace, names=names(query@partitioning), NG=length(names(query@partitioning)))
  newCompressedList0("CompressedHitsList", unlistData=hits, partitioning=partitioning)
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion
###

## return as.matrix as on Hits, with indices adjusted

setMethod("as.matrix", "CompressedHitsList", function(x) {
  cbind(queryHits=queryHits(x), subjectHits=subjectHits(x))
})

