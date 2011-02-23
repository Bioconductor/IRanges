### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### isConstant()
###

setGeneric("isConstant", function(x) standardGeneric("isConstant"))

### There are many ways to implement the "isConstant" method for integer
### vectors:
###   isConstant1 <- function(x) {length(x) <= 1L || all(x == x[1L])}
###   isConstant2 <- function(x) {length(unique(x)) <= 1L}
###   isConstant3 <- function(x) {length(x) <= 1L || all(duplicated(x)[-1L])}
###   isConstant4 <- function(x) {length(x) <= 1L ||
###                               sum(duplicated(x)) == length(x) - 1L}
###   isConstant5 <- function(x) {length(x) <= 1L || min(x) == max(x)}
###   isConstant6 <- function(x) {length(x) <= 1L ||
###                               {rx <- range(x); rx[1L] == rx[2L]}}
### Which one is faster is hard to guess. It happens to be isConstant5():
### it's 2.7x faster than isConstant1(), 6x faster than isConstant2(), 11x
### faster than isConstant3(), 5.2x faster than isConstant4() and 1.6x faster
### than isConstant6().
### Results obtained on 'x0 <- rep.int(112L, 999999L)' with R-2.13 Under
### development (unstable) (2011-01-08 r53945).
setMethod("isConstant", "integer",
    function(x) {length(x) <= 1L || min(x) == max(x)}
)

### Using all.equal() ensures that TRUE is returned on c(11/3, 2/3+4/3+5/3).
setMethod("isConstant", "numeric",
    function(x) {length(x) <= 1L || isTRUE(all.equal(min(x), max(x)))}
)

