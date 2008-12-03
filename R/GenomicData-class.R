### =========================================================================
### GenomicData objects
### -------------------------------------------------------------------------

## Extends RangedData to add convenience accessors

setClass("GenomicData", representation(ranges = "GenomicRanges"),
         contains = "RangedData")

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Accessor methods.
###

setMethod("genome", "GenomicData", function(x) universe(x))

setReplaceMethod("genome", "GenomicData",
                 function(x, value) {
                   genome(x@ranges) <- value
                   x
                 })

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
  
setMethod("chrom", "GenomicData", function(x) {
  chrom(ranges(x))
})

## score: a common track column

setGeneric("score", function(x, ...) standardGeneric("score"))
setGeneric("score<-", function(x, ..., value) standardGeneric("score<-"))

setMethod("score", "GenomicData", function(x) {
  score <- x[["score"]]
  if (is.null(score) && ncol(x) > 0)
    score <- x[[1]]
  score
})
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
  if (length(chrom) > length(ranges))
    stop("length of 'chrom' greater than length of 'ranges'")
  if (length(chrom) > 0 && (length(ranges) %% length(chrom) != 0))
    stop("length of 'ranges' not a multiple of 'chrom' length")
  if (!is.null(genome) && !isSingleString(genome))
    stop("'genome' must be a single string")
  rd <- RangedData(ranges, ..., space = chrom, universe = genome)
  rd@ranges <- as(ranges(rd), "GenomicRanges")
  gd <- new("GenomicData", rd)
  if (!is.null(strand)) {
    if (length(strand) != length(ranges))
      stop("length of 'strand' (if non-NULL) must match length of 'ranges'")
    if (!all(strand[!is.na(strand)] %in% levels(strand())))
      stop("strand values should be 'NA', '-', '+' or '*'")
    gd[["strand"]] <- as.integer(factor(strand, levels(strand())))
  }
  gd
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion
###

setAs("RangedData", "GenomicData",
      function(from) {
        ## casting RangedData up to GenomicData not automatic
        ## 'ranges' needs to be cast up to a GenomicRanges
        new("GenomicData", ranges = as(ranges(from), "GenomicRanges"),
            values = values(from))
      })

setAs("XRle", "GenomicData",
      function(from)
      {
        as(as(from, "RangedData"), "GenomicData")
      })

setAs("RangesList", "GenomicData",
      function(from)
      {
        as(as(from, "RangedData"), "GenomicData")
      })

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Combining and splitting.
###

## It is common for people to use do.call("c", l), but dispatch does
## not work so well when the arguments are named (inheritance not considered).
setMethod("c", "GenomicData", function(x, ..., recursive = FALSE) {
  if (missing(x))
    callNextMethod(...)
  else callNextMethod(x, ...)
})
