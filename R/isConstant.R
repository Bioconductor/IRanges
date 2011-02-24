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

### For this method we use a modified version of isConstant5() above that
### handles NAs.
setMethod("isConstant", "integer",
    function(x)
    {
        if (length(x) <= 1L)
            return(TRUE)
        x_min <- min(x, na.rm=FALSE)
        if (!is.na(x_min))  # success means 'x' contains no NAs
            return(x_min == max(x, na.rm=FALSE))
        ## From here 'x' is guaranteed to have a length >= 2 and to contain
        ## at least an NA.
        ## 'min(x, na.rm=TRUE)' issues a warning if 'x' contains only NAs.
        ## In that case, and in that case only, it returns Inf.
        x_min <- suppressWarnings(min(x, na.rm=TRUE))
        if (x_min == Inf)
            return(NA)
        ## From here 'x' is guaranteed to contain a mix of NAs and non-NAs.
        x_max <- max(x, na.rm=TRUE)
        if (x_min == x_max)
            return(NA)
        FALSE
    }
)

### Like the method for integer vectors this method also uses a comparison
### between min(x) and max(x). In addition it needs to handle rounding errors
### and special values: NA, NaN, Inf and -Inf.
### Using all.equal() ensures that TRUE is returned on c(11/3, 2/3+4/3+5/3).
setMethod("isConstant", "numeric",
    function(x)
    {
        if (length(x) <= 1L)
            return(TRUE)
        x_min <- min(x, na.rm=FALSE)
        if (!is.na(x_min)) {  # success means 'x' contains no NAs and no NaNs
            x_max <- max(x, na.rm=FALSE)
            if (is.finite(x_min) && is.finite(x_max))
                return(isTRUE(all.equal(x_min, x_max)))
            if (x_min == x_max)  # both are Inf or both are -Inf
                return(NA)
            return(FALSE)
        }
        ## From here 'x' is guaranteed to have a length >= 2 and to contain
        ## at least an NA or NaN.
        ## 'min(x, na.rm=TRUE)' issues a warning if 'x' contains only NAs
        ## and NaNs.
        x_min <- suppressWarnings(min(x, na.rm=TRUE))
        if (x_min == Inf) {
            ## Only possible values in 'x' are NAs, NaNs or Infs.
            is_in_x <- c(NA, NaN, Inf) %in% x
            if (is_in_x[2L] && is_in_x[3L])
                return(FALSE)
            return(NA)
        }
        ## From here 'x' is guaranteed to contain at least one value that is
        ## not NA or NaN or Inf.
        x_max <- max(x, na.rm=TRUE)
        if (x_max == -Inf) {
            ## Only possible values in 'x' are NAs, NaNs or -Infs.
            is_in_x <- c(NA, NaN, -Inf) %in% x
            if (is_in_x[2L] && is_in_x[3L])
                return(FALSE)
            return(NA)
        }
        if (is.infinite(x_min) || is.infinite(x_max))
            return(FALSE)
        if (!isTRUE(all.equal(x_min, x_max)))
            return(FALSE)
        if (NaN %in% x)
            return(FALSE)
        return(NA)
    }
)

setMethod("isConstant", "array", function(x) isConstant(as.vector(x)))

