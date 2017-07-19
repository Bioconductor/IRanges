### =========================================================================
### Selection of features and columns by intervals and column names
### -------------------------------------------------------------------------

setClass("RangedSelection",
         representation(ranges = "RangesList", colnames = "character"))

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Accessor methods.
###

setMethod("ranges", "RangedSelection",
    function(x, use.names=TRUE, use.mcols=FALSE) x@ranges
)
setReplaceMethod("ranges", "RangedSelection",
                 function(x, value) {
                   x@ranges <- value
                   x
                 })

setMethod("colnames", "RangedSelection",
          function(x, do.NULL = TRUE, prefix = "col") x@colnames)
setReplaceMethod("colnames", "RangedSelection",
                 function(x, value) {
                   x@colnames <- value
                   x
                 })

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Constructor.
###

RangedSelection <- function(ranges = RangesList(), colnames = character()) {
  if (!is(ranges, "RangesList"))
    stop("'ranges' must be a RangesList")
  if (!is.character(colnames) || S4Vectors:::anyMissing(colnames))
    stop("'colnames' must be a character vector without missing values")
  new("RangedSelection", ranges = ranges, colnames = colnames)
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion.
###

setAs("RangesList", "RangedSelection", function(from) RangedSelection(from))
