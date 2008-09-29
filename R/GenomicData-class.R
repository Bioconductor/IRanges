### =========================================================================
### GenomicData objects
### -------------------------------------------------------------------------

## A list of ChromData instances, with a genome identifier

setClass("GenomicData",
         representation(genome = "character"),
         contains = "RangedDataList")

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Accessor methods.
###

setGeneric("genome", function(object, ...) standardGeneric("genome"))
setMethod("genome", "GenomicData", function(object) object@genome)

setMethod("elementClass", "GenomicData", function(x) "ChromData")

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

