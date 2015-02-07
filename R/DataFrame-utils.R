### =========================================================================
### DataFrame utilities
### -------------------------------------------------------------------------

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Splitting.
###

setMethod("relistToClass", "data.frame",
    function(x) "CompressedSplitDataFrameList"
)

setMethod("relistToClass", "DataFrame",
    function(x) "CompressedSplitDataFrameList"
)

setMethod("mstack", "DataFrame", function(..., .index.var = "name") {
  stack(DataFrameList(...), index.var = .index.var)
})
