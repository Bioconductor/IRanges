### =========================================================================
### GenomicData objects
### -------------------------------------------------------------------------

## Extends RangedData to add convenience accessors

setClass("GenomicData", contains = "RangedData")

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Accessor methods.
###

setGeneric("genome", function(x, ...) standardGeneric("genome"))
setMethod("genome", "GenomicData", function(x) annotation(x))

setGeneric("strand", function(x, ...) standardGeneric("strand"))
setMethod("strand", "GenomicData", function(x) {
  strand <- x[["strand"]]
  if (is.null(strand))
    strand <- rep(NA, nrow(x))
### FIXME: just necessary because we have no XFactor
  levs <- levels(strand())
  factor(levs[strand], levs)
})

setMethod("strand", "missing", function(x) {
  factor(levels=c("-","+","*"))
})
  
setGeneric("chrom", function(x, ...) standardGeneric("chrom"))
setMethod("chrom", "GenomicData", function(x) {
  chrom <- names(x)
  if (is.null(chrom))
    chrom <- paste("chr", seq_len(length(x)), sep = "")
  chrom <- factor(chrom, chrom)
  rep(chrom, sapply(elements(ranges(x)), length))
})

## score: a common track column

setGeneric("score", function(x, ...) standardGeneric("score"))
setGeneric("score<-", function(x, ..., value) standardGeneric("score<-"))

setMethod("score", "GenomicData", function(x) x[["score"]])
setReplaceMethod("score", "GenomicData", function(x, value) {
  if (!is.numeric(value))
    stop("score must be numeric")
  if (length(value) != nrow(x))
    stop("number of scores must equal the number of rows")
  x[["score"]] <- value
  x
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

