### =========================================================================
### ChromData objects
### -------------------------------------------------------------------------

setClassUnion("factorORNULL", c("factor", "NULL"))

## For storing data on regions in a chromosome

### FIXME: once we have XFactor, should use for 'strand' slot
setClass("ChromData", representation(strand = "factorORNULL"),
         contains = "RangedData")

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Validity.
###

.valid.ChromData <- function(x)
{
  strand <- strand(x)
  if (levels(strand) != c("+", "-"))
    return("strand must be a factor with levels '+' and '-'")
  NULL
}

setValidity2("ChromData", .valid.ChromData)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Accessor methods.
###

setGeneric("strand", function(object, ...) standardGeneric("strand"))
setMethod("strand", "ChromData", function(object) {
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

ChromData <- function(ranges = IRanges(), values = NULL, strand = NULL) {
  rd <- RangedData(ranges, values)
  if (!is.null(strand)) {
    if (length(strand) != length(rd))
      stop("'strand' length must match the number of ranges")
    if (!is.character(strand) && !is.factor(strand))
      stop("'strand' must be a character vector or factor")
    strand <- factor(strand, levels=c("+", "-"))
  }
  new("ChromData", rd, strand = strand)
}
