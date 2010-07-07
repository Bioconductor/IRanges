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

setMethod("stack", "DataFrameList",
          function(x, indName = "space")
          {
            DataFrame(.stack.ind(x, indName), x,
                      row.names = unlist(rownames(x)))
          })
