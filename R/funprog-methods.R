### =========================================================================
### Functional programming methods
### -------------------------------------------------------------------------
###

#.ReduceDefault <- base::Reduce
#environment(.ReduceDefault) <- topenv()
.ReduceDefault <- function (f, x, init, right = FALSE, accumulate = FALSE) 
{
    mis <- missing(init)
    len <- length(x)
    if (len == 0L) 
        return(if (mis) NULL else init)
    f <- match.fun(f)
#    if (!is.vector(x) || is.object(x)) 
#        x <- as.list(x)
    ind <- seq_len(len)
    if (mis) {
        if (right) {
            init <- x[[len]]
            ind <- ind[-len]
        }
        else {
            init <- x[[1L]]
            ind <- ind[-1L]
        }
    }
    if (!accumulate) {
        if (right) {
            for (i in rev(ind)) init <- f(x[[i]], init)
        }
        else {
            for (i in ind) init <- f(init, x[[i]])
        }
        init
    }
    else {
        len <- length(ind) + 1L
        out <- vector("list", len)
        if (mis) {
            if (right) {
                out[[len]] <- init
                for (i in rev(ind)) {
                    init <- f(x[[i]], init)
                    out[[i]] <- init
                }
            }
            else {
                out[[1L]] <- init
                for (i in ind) {
                    init <- f(init, x[[i]])
                    out[[i]] <- init
                }
            }
        }
        else {
            if (right) {
                out[[len]] <- init
                for (i in rev(ind)) {
                    init <- f(x[[i]], init)
                    out[[i]] <- init
                }
            }
            else {
                for (i in ind) {
                    out[[i]] <- init
                    init <- f(init, x[[i]])
                }
                out[[len]] <- init
            }
        }
        if (all(sapply(out, length) == 1L)) 
            out <- unlist(out, recursive = FALSE)
        out
    }
}

setMethod("Reduce", "List", .ReduceDefault)
  
.FilterDefault <- base::Filter
environment(.FilterDefault) <- topenv()
setMethod("Filter", "List", .FilterDefault)

.FindDefault <- base::Find
environment(.FindDefault) <- topenv()
setMethod("Find", "List", .FindDefault)

.MapDefault <- base::Map
environment(.MapDefault) <- topenv()
setMethod("Map", "List", .MapDefault)
 
setMethod("Position", "List",
    function(f, x, right = FALSE, nomatch = NA_integer_)
    {
        ## In R-2.12, base::Position() was modified to use seq_along()
        ## internally. The problem is that seq_along() was a primitive
        ## that would let the user define methods for it (otherwise it
        ## would have been worth defining a "seq_along" method for Vector
        ## objects). So we need to redefine seq_along() locally in order
        ## to make base_Position() work.
        seq_along <- function(along.with) seq_len(length(along.with))
        base_Position <- base::Position
        environment(base_Position) <- environment()
        base_Position(f, x, right = right, nomatch = nomatch)
    }
)

