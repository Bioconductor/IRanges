### =========================================================================
### Comparing and ordering vector-like objects
### -------------------------------------------------------------------------
###


### Method signatures for binary comparison operators.
.BIN_COMP_OP_SIGNATURES <- list(
    c("Vector", "Vector"),
    c("Vector", "ANY"),
    c("ANY", "Vector")
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Element-wise (aka "parallel") comparison of 2 Ranges objects.
###

setGeneric("compare", function(x, y) standardGeneric("compare"))

setMethods("==", .BIN_COMP_OP_SIGNATURES,
    function(e1, e2) { compare(e1, e2) == 0L }
)

setMethods("<=", .BIN_COMP_OP_SIGNATURES,
    function(e1, e2) { compare(e1, e2) <= 0L }
)

### The comparison methods below are based on == and <=.

setMethods("!=", .BIN_COMP_OP_SIGNATURES, function(e1, e2) { !(e1 == e2) })

setMethods(">=", .BIN_COMP_OP_SIGNATURES, function(e1, e2) { e2 <= e1 })

setMethods("<", .BIN_COMP_OP_SIGNATURES, function(e1, e2) { !(e2 <= e1) })

setMethods(">", .BIN_COMP_OP_SIGNATURES, function(e1, e2) { !(e1 <= e2) })


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### unique()
###
### This method is based on duplicated().
###

### S3/S4 combo for unique.Vector
unique.Vector <- function(x, incomparables=FALSE, ...)
    extractROWS(x, !duplicated(x, incomparables=incomparables, ...))
setMethod("unique", "Vector", unique.Vector)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### %in%
###
### This method is based on match().
###

setMethods("%in%", .BIN_COMP_OP_SIGNATURES,
    function(x, table) { !is.na(match(x, table)) }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### sort()
###
### This method is based on order().
###

### S3/S4 combo for sort.Vector
.sort.Vector <- function(x, decreasing=FALSE, na.last=NA)
    extractROWS(x, order(x, na.last=na.last, decreasing=decreasing))
sort.Vector <- function (x, decreasing=FALSE, ...)
    .sort.Vector(x, decreasing=decreasing, ...)
setMethod("sort", "Vector", sort.Vector)

