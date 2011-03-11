### =========================================================================
### Annotated objects
### -------------------------------------------------------------------------

setClass("Annotated", representation("VIRTUAL", metadata = "list"))

setGeneric("metadata", function(x, ...) standardGeneric("metadata"))
setMethod("metadata", "Annotated",
          function(x) {
              if (is.null(x@metadata) || is.character(x@metadata))
                  list(metadata = x@metadata)
              else
                  x@metadata
          })

setGeneric("metadata<-",
           function(x, ..., value) standardGeneric("metadata<-"))
setReplaceMethod("metadata", "Annotated",
                 function(x, value) {
                     if (!is.list(value))
                         stop("replacement 'metadata' value must be a list")
                     if (!length(value))
                         names(value) <- NULL # instead of character()
                     x@metadata <- value
                     x
                 })
