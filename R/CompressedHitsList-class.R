### =========================================================================
### CompressedHitsList objects
### -------------------------------------------------------------------------

### [H.P. - 2015/12/17] Why do we need this? Where is it used?

setClass("CompressedHitsList",
    prototype = prototype(elementType = "Hits",
                          unlistData = new("Hits")),
    contains="CompressedList")


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Accessors
###

setMethod("from", "CompressedHitsList", function(x) from(x@unlistData))
setMethod("to", "CompressedHitsList", function(x) to(x@unlistData))

setMethod("nLnode", "CompressedHitsList", function(x) nLnode(x@unlistData))
setMethod("nRnode", "CompressedHitsList", function(x) nRnode(x@unlistData))


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

