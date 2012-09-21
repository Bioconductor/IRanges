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
          function(x, index.var = "name")
          {
            DataFrame(.stack.ind(x, index.var), x,
                      row.names = unlist(lapply(x, rownames)))
          })
