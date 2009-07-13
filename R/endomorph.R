### =========================================================================
### endomorph()
### -------------------------------------------------------------------------
###

setGeneric("endomorph", signature = "X",
           function(X, FUN, ...) standardGeneric("endomorph"))

setMethod("endomorph", "list",
          function(X, FUN, ...) lapply(X = X, FUN = FUN, ...))

setMethod("endomorph", "data.frame",
          function(X, FUN, ...) as.data.frame(lapply(X = X, FUN = FUN, ...)))

### =========================================================================
### mendomorph()
### -------------------------------------------------------------------------
###

setGeneric("mendomorph", signature = "...",
           function(FUN, ..., MoreArgs = NULL) standardGeneric("mendomorph"))

setMethod("mendomorph", "list", function(FUN, ..., MoreArgs = NULL)
          mapply(FUN = FUN, ..., MoreArgs = MoreArgs))

setMethod("mendomorph", "data.frame", function(FUN, ..., MoreArgs = NULL)
          as.data.frame(mapply(FUN = FUN, ..., MoreArgs = MoreArgs)))
