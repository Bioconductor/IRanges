### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "runsum", "runwtsum", and "runq" generics.
###

setGeneric("runsum", signature="x",
           function(x, k, endrule = c("drop", "constant"), ...)
               standardGeneric("runsum"))

setGeneric("runmean", signature="x",
           function(x, k, endrule = c("drop", "constant"), ...)
               standardGeneric("runmean"))

setGeneric("runwtsum", signature="x",
           function(x, k, wt, endrule = c("drop", "constant"), ...)
               standardGeneric("runwtsum"))

setGeneric("runq", signature="x",
           function(x, k, i, endrule = c("drop", "constant"), ...)
               standardGeneric("runq"))
