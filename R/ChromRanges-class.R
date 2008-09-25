### =========================================================================
### ChromRanges objects
### -------------------------------------------------------------------------

setClass("ChromRanges", contains = "ValuedIRanges")

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Validity.
###

.valid.ChromRanges <- function(x)
{
  strand <- strand(x)
  if (!is.factor(strand) || levels(strand) != c("+", "-"))
    return("strand must be a factor with levels '+' and '-'")
  NULL
}

setValidity2("ChromRanges", .valid.ChromRanges)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Accessor methods.
###

setGeneric("strand", function(object, ...) standardGeneric("strand"))
setMethod("strand", "ChromRanges", function(object) {
  strand <- object[["strand"]]
  if (is.null(strand))
    strand <- rep(NA, length(object))
  strand <- as.factor(strand)
  levels(strand) <- c("+", "-")
  strand
})

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Constructor.
###

ChromRanges <- function(ranges = IRanges(), strand = NULL, ...) {
  vir <- ValuedIRanges(ranges, ...)
  if (!is.null(strand)) {
    if (length(strand) != length(vir))
      stop("'strand' length must match the number of ranges")
    if (!is.character(strand) && !is.factor(strand))
      stop("'strand' must be a character vector or factor")
    strand <- factor(strand, levels=c("+", "-"))
    vir[["strand"]] <- XInteger(length(strand), as.integer(strand))
  }
  new("ChromRanges", vir)
}
