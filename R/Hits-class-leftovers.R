### =========================================================================
### IMPORTANT NOTE - 4/29/2014
### Most of the stuff that used to be in the IRanges/R/Hits-class.R file was
### moved to the S4Vectors package (to R/Hits-class.R).
### The stuff that could not be moved there was *temporarily* kept here in
### Hits-class-leftovers.R but will need to find a new home (in S4Vectors
### or in IRanges).
###


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion
###

setAs("Hits", "DataFrame", function(from) {
  DataFrame(as.matrix(from),
            if (!is.null(mcols(from))) mcols(from)
            else S4Vectors:::make_zero_col_DataFrame(length(from)))
})

.as.data.frame.Hits <- function(x, row.names=NULL, optional=FALSE, ...)
{
    as.data.frame(as(x, "DataFrame"), row.names=row.names, optional=optional,
                  ...)
}
setMethod("as.data.frame", "Hits", .as.data.frame.Hits)

### Turn SortedByQueryHits object 'from' into a PartitioningByEnd object that
### describes the grouping of hits by query.
.from_SortedByQueryHits_to_PartitioningByEnd <- function(from)
    PartitioningByEnd(queryHits(from), NG=queryLength(from))
setAs("SortedByQueryHits", "PartitioningByEnd",
    .from_SortedByQueryHits_to_PartitioningByEnd
)
setAs("SortedByQueryHits", "Partitioning",
    .from_SortedByQueryHits_to_PartitioningByEnd
)
setAs("SortedByQueryHits", "Ranges",
    .from_SortedByQueryHits_to_PartitioningByEnd
)
setAs("SortedByQueryHits", "IRanges",
    function(from)
        as(.from_SortedByQueryHits_to_PartitioningByEnd(from), "IRanges")
)

### Turn SortedByQueryHits object 'from' into a CompressedIntegerList object
### with one list element per element in the original query.
.from_SortedByQueryHits_to_CompressedIntegerList <- function(from)
{
    ans_partitioning <- .from_SortedByQueryHits_to_PartitioningByEnd(from)
    relist(subjectHits(from), ans_partitioning)
}
setAs("SortedByQueryHits", "CompressedIntegerList",
    .from_SortedByQueryHits_to_CompressedIntegerList
)
setAs("SortedByQueryHits", "IntegerList",
    .from_SortedByQueryHits_to_CompressedIntegerList
)
setAs("SortedByQueryHits", "List",
    .from_SortedByQueryHits_to_CompressedIntegerList
)

.as.list.SortedByQueryHits <- function(x)
    as.list(.from_SortedByQueryHits_to_CompressedIntegerList(x))
setMethod("as.list", "SortedByQueryHits", .as.list.SortedByQueryHits)

.from_Hits_to_CompressedIntegerList <- function(from)
{
    as(as(from, "SortedByQueryHits"), "CompressedIntegerList")
}

setAs("Hits", "List", .from_Hits_to_CompressedIntegerList)
setAs("Hits", "IntegerList", .from_Hits_to_CompressedIntegerList)
setAs("Hits", "CompressedIntegerList", .from_Hits_to_CompressedIntegerList)

setMethod("as.list", "Hits", function(x) as.list(as(x, "SortedByQueryHits")))

setAs("Hits", "Grouping",
      function(from) ManyToManyGrouping(as(from, "List"), nobj=nRnode(from)))
