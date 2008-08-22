### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The generic for Views.
###

setGeneric("viewMins", signature="x",
    function(x, na.rm = FALSE) standardGeneric("viewMins")
)

setGeneric("viewMaxs", signature="x",
    function(x, na.rm = FALSE) standardGeneric("viewMaxs")
)

setGeneric("viewSums", signature="x",
    function(x, na.rm = FALSE) standardGeneric("viewSums")
)
