### =========================================================================
### endoapply()
### -------------------------------------------------------------------------
###

setGeneric("endoapply", signature = "X",
           function(X, FUN, ...) standardGeneric("endoapply"))

setMethod("endoapply", "list",
          function(X, FUN, ...) lapply(X = X, FUN = FUN, ...))

setMethod("endoapply", "data.frame",
          function(X, FUN, ...) as.data.frame(lapply(X = X, FUN = FUN, ...)))

### =========================================================================
### mendoapply()
### -------------------------------------------------------------------------
###

setGeneric("mendoapply", signature = "...",
           function(FUN, ..., MoreArgs = NULL) standardGeneric("mendoapply"))

setMethod("mendoapply", "list", function(FUN, ..., MoreArgs = NULL)
          mapply(FUN = FUN, ..., MoreArgs = MoreArgs, SIMPLIFY = FALSE))

setMethod("mendoapply", "data.frame", function(FUN, ..., MoreArgs = NULL)
          as.data.frame(mapply(FUN = FUN, ..., MoreArgs = MoreArgs,
                               SIMPLIFY = FALSE)))
