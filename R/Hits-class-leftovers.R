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
            else new("DataFrame", nrows = length(from)))
})

### S3/S4 combo for as.data.frame.Hits
as.data.frame.Hits <- function(x, row.names=NULL, optional=FALSE, ...)
{
    if (!(is.null(row.names) || is.character(row.names)))
        stop("'row.names' must be NULL or a character vector")
    if (!identical(optional, FALSE) || length(list(...)))
        warning("'optional' and arguments in '...' are ignored")
    as.data.frame(as(x, "DataFrame"), row.names = row.names)
}
setMethod("as.data.frame", "Hits", as.data.frame.Hits)

### Turn Hits object 'from' into a PartitioningByEnd object that describes
### the grouping of hits by query.
.from_Hits_to_PartitioningByEnd <- function(from)
    PartitioningByEnd(queryHits(from), NG=queryLength(from))
setAs("Hits", "PartitioningByEnd", .from_Hits_to_PartitioningByEnd)
setAs("Hits", "Partitioning", .from_Hits_to_PartitioningByEnd)
setAs("Hits", "Ranges", .from_Hits_to_PartitioningByEnd)
setAs("Hits", "IRanges",
    function(from) as(.from_Hits_to_PartitioningByEnd(from), "IRanges")
)

### Turn Hits object 'from' into a CompressedIntegerList object with one list
### element per element in the original query.
.from_Hits_to_CompressedIntegerList <- function(from)
{
    ans_partitioning <- .from_Hits_to_PartitioningByEnd(from)
    relist(subjectHits(from), ans_partitioning)
}
setAs("Hits", "CompressedIntegerList", .from_Hits_to_CompressedIntegerList)
setAs("Hits", "IntegerList", .from_Hits_to_CompressedIntegerList)
setAs("Hits", "List", .from_Hits_to_CompressedIntegerList)

### S3/S4 combo for as.list.Hits
.as.list.Hits <- function(x) as.list(.from_Hits_to_CompressedIntegerList(x))
as.list.Hits <- function(x, ...) .as.list.Hits(x, ...)
setMethod("as.list", "Hits", .as.list.Hits)

setAs("Hits", "list", function(from) as.list(from))

