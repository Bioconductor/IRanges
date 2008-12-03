### =========================================================================
### GenomicRanges objects
### -------------------------------------------------------------------------

## Defines convenience accessors on top of RangesList

setClass("GenomicRanges", contains = "RangesList")

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Accessor methods.
###

setGeneric("genome", function(x, ...) standardGeneric("genome"))
setMethod("genome", "GenomicRanges", function(x) universe(x))

setGeneric("genome<-", function(x, value) standardGeneric("genome<-"))
setReplaceMethod("genome", "RangesList",
                 function(x, value) {
                   universe(x) <- value
                   x
                 })

setGeneric("chrom", function(x, ...) standardGeneric("chrom"))
setMethod("chrom", "GenomicRanges", function(x) {
  chrom <- names(x)
  if (!is.null(chrom))
    chrom <- rep(factor(chrom, chrom), sapply(elements(x), length))
  chrom
})

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Constructor
###

GenomicRanges <- function(..., genome = NULL)
{
  RangesList(..., universe = genome)
}
