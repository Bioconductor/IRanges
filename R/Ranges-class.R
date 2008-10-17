### Ranges is a virtual class that serves as the base for all range containers

setClass("Ranges", contains = "VIRTUAL")

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "isEmpty" generic.
### TODO: default implementation using width()

setGeneric("isEmpty", function(x) standardGeneric("isEmpty"))

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "reduce" generic.
###

setGeneric("reduce", signature="x",
           function(x, with.inframe.attrib=FALSE) standardGeneric("reduce")
           )

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "gaps" generic.
###
###

setGeneric("gaps", signature="x",
           function(x, start=NA, end=NA)
           standardGeneric("gaps")
           )

### More generics should be moved here
