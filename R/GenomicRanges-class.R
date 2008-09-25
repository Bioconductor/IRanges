### =========================================================================
### GenomicRanges objects
### -------------------------------------------------------------------------

## A list of ChromRanges

setClass("GenomicRanges",
         representation(genome = "character"),
         contains = "ValuedIRangesList")

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Accessor methods.
###

setGeneric("genome", function(object, ...) standardGeneric("genome"))
setMethod("genome", "GenomicRanges", function(object) object@genome)

setMethod("elementClass", "GenomicRanges", function(x) "ChromRanges")

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Validity.
###

.valid.GenomicRanges <- function(x)
{
  ## lengths of objects in 'data' should equal length of Ranges
  if (length(genome) > 1)
    return("'genome' length greater than 1")
  NULL
}

setValidity2("GenomicRanges", .valid.GenomicRanges)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Constructor.
###

GenomicRanges <- function(..., genome = character()) {
  if (!is.character(genome) || length(genome) > 1)
    stop("genome must be a character vector of length 0 or 1")
  if (!all(sapply(list(...), is, "ChromRanges")))
    stop("all range elements must be ChromRanges instances")
  rl <- RangesList(...)
  new("GenomicRanges", rl, genome = genome)
}

