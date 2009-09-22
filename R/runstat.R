### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "runsum", "runwtsum", and "runq" generics.
###

setGeneric("runsum", signature="x",
           function(x, k) standardGeneric("runsum"))

setGeneric("runwtsum", signature="x",
           function(x, k, wt) standardGeneric("runwtsum"))

setGeneric("runq", signature="x",
           function(x, k, i) standardGeneric("runq"))
