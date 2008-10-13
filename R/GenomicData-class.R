### =========================================================================
### GenomicData objects
### -------------------------------------------------------------------------

## Extends RangedData to add convenience accessors

setClass("GenomicData", contains = "RangedData")

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Accessor methods.
###

setGeneric("genome", function(object, ...) standardGeneric("genome"))
setMethod("genome", "GenomicData", function(object) annotation(object))

setGeneric("strand", function(object, ...) standardGeneric("strand"))
setMethod("strand", "GenomicData", function(object) {
  strand <- object[["strand"]]
  if (is.null(strand))
    strand <- rep(NA, nrow(object))
### FIXME: just necessary because we have no XFactor
  levs <- levels(strand())
  factor(levs[strand], levs)
})

setMethod("strand", "missing", function(object) {
  factor(levels=c("-","+","*"))
})
  
setGeneric("chrom", function(object, ...) standardGeneric("chrom"))
setMethod("chrom", "GenomicData", function(object) {
  chrom <- names(object)
  if (is.null(chrom))
    chrom <- paste("chr", seq_len(length(object)), sep = "")
  chrom <- factor(chrom, chrom)
  rep(chrom, sapply(elements(ranges(object)), length))
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

GenomicData <- function(ranges, ..., strand = NULL, chrom = NULL, genome = NULL)
{
  if (!is.null(chrom) && length(chrom) != length(ranges))
    stop("length of 'chrom' (if non-NULL) must match length of 'ranges'")
  if (!is.null(genome) && !isSingleString(genome))
    stop("'genome' must be a single string")
  gd <- new("GenomicData",
            RangedData(ranges, ..., splitter = chrom, annotation = genome))
  if (!is.null(strand)) {
    if (length(strand) != length(ranges))
      stop("length of 'strand' (if non-NULL) must match length of 'ranges'")
    if (!all(strand[!is.na(strand)] %in% levels(strand())))
      stop("strand values should be 'NA', '-', '+' or '*'")
    gd[["strand"]] <- as.integer(factor(strand, levels(strand())))
  }
  gd
}

