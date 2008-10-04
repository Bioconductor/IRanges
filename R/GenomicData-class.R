### =========================================================================
### GenomicData objects
### -------------------------------------------------------------------------

## Extends RangedData to add convenience accessors

setClass("GenomicData",
         representation(genome = "character"),
         contains = "RangedData")

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Accessor methods.
###

setGeneric("genome", function(object, ...) standardGeneric("genome"))
setMethod("genome", "GenomicData", function(object) annotation(object))

setGeneric("strand", function(object, ...) standardGeneric("strand"))
setMethod("strand", "GenomicData", function(object) {
  strand <- object[["strand"]]
  if (is.null(strand))
    strand <- rep(NA, length(object))
### FIXME: just necessary because we have no XFactor
  strand <- as.factor(strand)
  levels(strand) <- c("+", "-")
  strand
})

setGeneric("chrom", function(object, ...) standardGeneric("chrom"))
setMethod("chrom", "GenomicData", function(object) {
  design <- design(object) 
  chrom <- design[["chrom"]]
  if (is.null(chrom)) {
    chrom <- rownames(design)
    if (is.null(chrom))
      chrom <- paste("chr", seq_len(nrow(design)), sep = "")
    chrom <- factor(chrom, chrom)
  }
  rep(chrom, width(ranges(object)))
})

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Validity.
###

.valid.GenomicData <- function(x)
{
  ## lengths of objects in 'data' should equal length of Ranges
  if (length(genome) > 1)
    return("'genome' length greater than 1")
  NULL
}

setValidity2("GenomicData", .valid.GenomicData)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Constructor.
###

GenomicData <- function(..., genome = character()) {
  if (!is.character(genome) || length(genome) > 1)
    stop("genome must be a character vector of length 0 or 1")
  if (!all(sapply(list(...), is, "ChromData")))
    stop("all range elements must be ChromData instances")
  rl <- RangesList(...)
  new("GenomicData", rl, genome = genome)
}

