### =========================================================================
### DataFrame utilities
### -------------------------------------------------------------------------

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Splitting.
###

setMethod("relistToClass", "data.frame",
    function(x) "CompressedSplitDFrameList"
)

setMethod("relistToClass", "DataFrame",
    function(x) "CompressedSplitDFrameList"
)

setMethod("mstack", "DataFrame", function(..., .index.var = "name") {
  stack(DataFrameList(...), index.var = .index.var)
})
