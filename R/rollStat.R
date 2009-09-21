### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "rollSum", "rollWeightedSum", and "rollQ" generics.
###

setGeneric("rollSum", signature="x",
           function(x, width) standardGeneric("rollSum"))

setGeneric("rollWeightedSum", signature="x",
           function(x, width, weight) standardGeneric("rollWeightedSum"))

setGeneric("rollQ", signature="x",
           function(x, width, which) standardGeneric("rollQ"))
