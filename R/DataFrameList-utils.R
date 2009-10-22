### =========================================================================
### DataFrameList utilities
### -------------------------------------------------------------------------

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Combining.
###

setMethod("cbind", "DataFrameList",
          function(..., deparse.level=1) mendoapply(cbind, ...))

setMethod("rbind", "DataFrameList",
          function(..., deparse.level=1) mendoapply(rbind, ...))
