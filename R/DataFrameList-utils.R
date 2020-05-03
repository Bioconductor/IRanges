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
            DataFrame(S4Vectors:::stack_index(x, index.var),
                      unlist(x, use.names=FALSE),
                      row.names = unlist(lapply(x, rownames)))
          })

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Transforming.
###

setClass("SDFLWrapperForTransform",
         representation(delegate = "SplitDataFrameList"),
         contains="Vector")

setMethod("colnames", "SDFLWrapperForTransform", function(x) {
  commonColnames(x@delegate)
})

setMethod("[[", "SDFLWrapperForTransform", function (x, i, j, ...) {
  x@delegate[,i]
})

setReplaceMethod("[[", "SDFLWrapperForTransform", function(x, i, j, ..., value) {
  x@delegate[,i] <- value
  x
})

setMethod(S4Vectors:::`column<-`, "SDFLWrapperForTransform",
          function(x, name, value)
{
    x[[name]] <- value
    x
})

setMethod("as.env", "SDFLWrapperForTransform",
          function(x, enclos = parent.frame(2)) {
              env <- S4Vectors:::makeEnvForNames(x, colnames(x), enclos)
              S4Vectors:::addSelfRef(x@delegate, env)
          })

transform.SplitDataFrameList <- function(`_data`, ...) {
  illConceivedWrapper <- new("SDFLWrapperForTransform", delegate=`_data`)
  transform.DataFrame(illConceivedWrapper, ...)@delegate
}

setMethod("transform", "SplitDataFrameList", transform.SplitDataFrameList)
