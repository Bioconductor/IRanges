### =========================================================================
### GenomicRanges objects
### -------------------------------------------------------------------------

## A list of ChromRanges, each with a slot for each strand

setClass("GenomicRanges",
         representation(genome = "character"),
         prototype(genome = "hg18"),
         contains = "ValuedIRangesList")

setMethod("elementClass", "GenomicRanges", function(x) "ChromRanges")
