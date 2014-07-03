### =========================================================================
### IMPORTANT NOTE - 4/29/2014
### Most of the stuff that used to be in the IRanges/R/Vector-class.R file
### was moved to the S4Vectors package (to R/Vector-class.R).
### The stuff that could not be moved there was *temporarily* kept here in
### Vector-class-leftovers.R but will need to find a new home (in S4Vectors
### or in IRanges).
###


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Internal utility.
###

setMethod("showAsCell", "list", function(object)
          rep.int("########", length(object)))


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Convenience wrappers for common subsetting operations.
###

### S3/S4 combo for head.Vector
head.Vector <- function(x, n=6L, ...)
{
    if (!isSingleNumber(n))
        stop("'n' must be a single integer")
    if (!is.integer(n))
        n <- as.integer(n)
    x_NROW <- NROW(x)
    if (n >= 0L) {
        n <- min(n, x_NROW)
    } else {
        n <- max(x_NROW + n, 0L)
    }
    extractROWS(x, IRanges(start=1L, width=n))
}
setMethod("head", "Vector", head.Vector)

## S3/S4 combo for tail.Vector
tail.Vector <- function(x, n=6L, ...)
{
    if (!isSingleNumber(n))
        stop("'n' must be a single integer")
    if (!is.integer(n))
        n <- as.integer(n)
    x_NROW <- NROW(x)
    if (n >= 0L) {
        n <- min(n, x_NROW)
    } else {
        n <- max(x_NROW + n, 0L)
    }
    extractROWS(x, IRanges(end=x_NROW, width=n))
}
setMethod("tail", "Vector", tail.Vector)

### S3/S4 combo for window.Vector
window.Vector <- function(x, start=NA, end=NA, width=NA,
                             frequency=NULL, delta=NULL, ...)
{
    i <- solveUserSEWForSingleSeq(NROW(x), start, end, width)
    if (!is.null(frequency) || !is.null(delta)) {
        i <- stats:::window.default(seq_len(NROW(x)),
                                    start=start(i),
                                    end=end(i),
                                    frequency=frequency,
                                    deltat=delta, ...)
        attributes(i) <- NULL
    }
    extractROWS(x, i)
}
setMethod("window", "Vector", window.Vector)

### S3/S4 combo for window.vector
### FIXME: This method alters the semantic of stats::window() on ordinary
### vectors (the result has no 'tsp' attribute). Not really acceptable.
window.vector <- window.Vector
setMethod("window", "vector", window.vector)

### S3/S4 combo for window.factor
### FIXME: This method alters the semantic of stats::window() on factors
### (the result has no 'tsp' attribute). Not really acceptable.
window.factor <- window.Vector
setMethod("window", "factor", window.factor)

### S3/S4 combo for window.NULL
window.NULL <- function(x, ...) NULL
setMethod("window", "NULL", window.NULL)

### S3/S4 combo for window<-.Vector
`window<-.Vector` <- function(x, start=NA, end=NA, width=NA, ..., value)
{
    i <- solveUserSEWForSingleSeq(NROW(x), start, end, width)
    li <- width(i)
    if (li == 0L) {
        ## Surprisingly, in that case, `[<-` on standard vectors does not
        ## even look at 'value'. So neither do we...
        return(x)
    }
    lv <- NROW(value)
    if (lv == 0L)
        stop("replacement has length zero")
    value <- normalizeSingleBracketReplacementValue(value, x)
    if (li != lv) {
        if (li %% lv != 0L)
            warning("number of values supplied is not a sub-multiple ",
                    "of the number of values to be replaced")
        value <- extractROWS(value, rep(seq_len(lv), length.out=li))
    }
    c(window(x, end=start(i)-1L),
      value,
      window(x, start=end(i)+1L))
}
setReplaceMethod("window", "Vector", `window<-.Vector`)

### S3/S4 combo for window<-.vector
`window<-.vector` <- `window<-.Vector`
setReplaceMethod("window", "vector", `window<-.vector`)

### S3/S4 combo for window<-.factor
`window<-.factor` <- function(x, start=NA, end=NA, width=NA, ..., value)
{
    levels <- levels(x)
    x <- as.character(x)
    value <- as.character(value)
    factor(callGeneric(), levels=levels)
}
setReplaceMethod("window", "factor", `window<-.factor`)

setMethod("rev", "Vector",
    function(x)
    {
        if (length(x) == 0L)
            return(x)
        x[length(x):1]
    }
)

setMethod("rep", "Vector", function(x, ...)
          x[rep(seq_len(length(x)), ...)])

setMethod("rep.int", "Vector",
    function(x, times) x[rep.int(seq_len(length(x)), times)]
)

setMethod("subset", "Vector",
          function(x, subset, select, drop = FALSE, ...) {
            i <- S4Vectors:::evalqForSubset(subset, x, ...)
            j <- S4Vectors:::evalqForSelect(select, mcols(x), ...)
            mcols(x) <- mcols(x)[,j,drop=FALSE]
            x[i, drop=drop]
          })


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Combining.
###

rbindRowOfNAsToMetadatacols <- function(x) {
  x_mcols <- mcols(x)
  if (!is.null(x_mcols))
    mcols(x)[nrow(x_mcols)+1L,] <- NA
  x
}

rbind.mcols <- function(x, ...)
{
    args <- list(x, ...)
    mcols_list <- lapply(args, mcols)
    if (length(mcols_list) == 1L)
        return(mcols_list[[1L]])
    mcols_is_null <- sapply(mcols_list, is.null)
    if (all(mcols_is_null))
        return(NULL)
    makeZeroColDataFrame <- function(nr) new("DataFrame", nrows=nr)
    mcols_list[mcols_is_null] <- lapply(elementLengths(args)[mcols_is_null],
                                        makeZeroColDataFrame)
    colnames_list <- lapply(mcols_list, colnames)
    allCols <- unique(unlist(colnames_list, use.names=FALSE))
    fillCols <- function(df) {
        if (nrow(df))
            df[setdiff(allCols, colnames(df))] <- DataFrame(NA)
        df
    }
    do.call(rbind, lapply(mcols_list, fillCols))
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Evaluating.
###

setMethod("eval", c("expression", "Vector"),
          function(expr, envir, enclos = parent.frame())
          eval(expr, as.env(envir, enclos))
          )

setMethod("eval", c("language", "Vector"),
          function(expr, envir, enclos = parent.frame())
          eval(expr, as.env(envir, enclos))
          )

setMethod("with", "Vector",
          function(data, expr, ...)
          {
            S4Vectors:::safeEval(substitute(expr), data, parent.frame(), ...)
          })


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion.
###

as.list.Vector <- function(x) as.list(as(x, "List"))

setMethod("as.list", "Vector", as.list.Vector)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### mstack()
###

setGeneric("mstack", function(..., .index.var = "name")
           standardGeneric("mstack"), signature = "...")

setMethod("mstack", "Vector", function(..., .index.var = "name") {
  if (!isSingleString(.index.var))
    stop("'.index.var' must be a single, non-NA string")
  args <- list(...)
  combined <- compress_listData(args)
  df <- .stack.ind(args, .index.var)
  if (!is.null(mcols(combined)))
    df <- cbind(mcols(combined), df)
  mcols(combined) <- df
  combined
})

setMethod("mstack", "vector",
          function(..., .index.var = "name")
          {
            if (!isSingleString(.index.var))
              stop("'.index.var' must be a single, non-NA string")
            args <- list(...)
            combined <- compress_listData(args)
            df <- DataFrame(.stack.ind(args, .index.var), combined)
            if (ncol(df) == 2L)
              colnames(df)[2] <- "value"
            df
          })


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Looping methods.
###

#.tapplyDefault <- base::tapply
#environment(.tapplyDefault) <- topenv()
.tapplyDefault <-
function(X, INDEX, FUN = NULL, ..., simplify = TRUE) 
{
    if (!is.null(FUN))
        FUN <- match.fun(FUN)
    if (missing(INDEX))
        stop("'INDEX' is missing")
    if (!is(INDEX, "RleList")) {
        if (!is.list(INDEX) && !is(INDEX, "Rle"))
            INDEX <- Rle(INDEX)
        INDEX <- RleList(INDEX)
    }
    nI <- length(INDEX)
    namelist <- vector("list", nI)
    names(namelist) <- names(INDEX)
    extent <- integer(nI)
    nx <- length(X)
    one <- 1L
    group <- Rle(one, nx)
    ngroup <- one
    for (i in seq_len(nI)) {
        index <- INDEX[[i]]
        if (!is.factor(runValue(index)))
            runValue(index) <- factor(runValue(index))
        offset <- index
        runValue(offset) <- ngroup * (as.integer(runValue(index)) - one)
        if (length(index) != nx) 
            stop("arguments must have same length")
        namelist[[i]] <- levels(index)
        extent[i] <- nlevels(index)
        group <- group + offset
        ngroup <- ngroup * nlevels(index)
    }
    if (is.null(FUN))
        return(as.vector(group))
    groupRanges <- splitRanges(group)
    ans <- lapply(groupRanges, function(i) FUN(extractROWS(X, i), ...))
    index <- as.integer(names(ans))
    if (simplify && all(unlist(lapply(ans, length), use.names=FALSE) == 1L)) {
        ansmat <- array(dim = extent, dimnames = namelist)
        ans <- unlist(ans, recursive = FALSE)
    }
    else {
        ansmat <-
          array(vector("list", prod(extent)), dim = extent,
                dimnames = namelist)
    }
    if (length(index) > 0) {
        names(ans) <- NULL
        ansmat[index] <- ans
    }
    ansmat
}
setMethod("tapply", c("Vector", "ANY"), .tapplyDefault)
setMethod("tapply", c("ANY", "Vector"), .tapplyDefault)
setMethod("tapply", c("Vector", "Vector"), .tapplyDefault)

.shiftApplyInternal <-
function(SHIFT, X, Y, FUN, ..., OFFSET = 0L, simplify = TRUE, verbose = FALSE)
{
    FUN <- match.fun(FUN)
    N <- length(X)
    if (N != length(Y))
        stop("'X' and 'Y' must be of equal length")

    if (!is.integer(SHIFT))
        SHIFT <- as.integer(SHIFT)
    if (length(SHIFT) == 0 || S4Vectors:::anyMissingOrOutside(SHIFT, 0L))
        stop("all 'SHIFT' values must be non-negative")

    if (!is.integer(OFFSET))
        OFFSET <- as.integer(OFFSET)
    if (length(OFFSET) == 0 || S4Vectors:::anyMissingOrOutside(OFFSET, 0L))
        stop("'OFFSET' must be non-negative")

    ## Perform X setup
    shiftedStartX <- rep.int(1L + OFFSET, length(SHIFT))
    shiftedEndX <- N - SHIFT

    ## Perform Y setup
    shiftedStartY <- 1L + SHIFT
    shiftedEndY <- rep.int(N - OFFSET, length(SHIFT))

    if (verbose) {
        maxI <- length(SHIFT)
        ans <-
          sapply(seq_len(length(SHIFT)),
                 function(i) {
                     cat("\r", i, "/", maxI)
                     FUN(window(X, start = shiftedStartX[i], end = shiftedEndX[i]),
                         window(Y, start = shiftedStartY[i], end = shiftedEndY[i]),
                         ...)
                 }, simplify = simplify)
        cat("\n")
    } else {
        ans <-
          sapply(seq_len(length(SHIFT)),
                 function(i)
                     FUN(window(X, start = shiftedStartX[i], end = shiftedEndX[i]),
                         window(Y, start = shiftedStartY[i], end = shiftedEndY[i]),
                         ...),
                 simplify = simplify)
    }
    ans
}

setMethod("shiftApply", signature(X = "Vector", Y = "Vector"),
          .shiftApplyInternal)

setMethod("shiftApply", signature(X = "vector", Y = "vector"),
          .shiftApplyInternal)

.aggregateInternal <-
function(x, by, FUN, start = NULL, end = NULL, width = NULL,
         frequency = NULL, delta = NULL, ..., simplify = TRUE)
{
    FUN <- match.fun(FUN)
    if (!missing(by)) {
        if (is.list(by)) {
            return(callGeneric(x = as.data.frame(x), by = by, FUN = FUN, ...))
        }
        start <- start(by)
        end <- end(by)
    } else {
        if (!is.null(width)) {
            if (is.null(start))
                start <- end - width + 1L
            else if (is.null(end))
                end <- start + width - 1L
        }
        start <- as(start, "integer")
        end <- as(end, "integer")
    }
    if (length(start) != length(end))
        stop("'start', 'end', and 'width' arguments have unequal length")
    n <- length(start)
    if (!is.null(names(start)))
        indices <- structure(seq_len(n), names = names(start))
    else
        indices <- structure(seq_len(n), names = names(end))
    if (is.null(frequency) && is.null(delta)) {
        sapply(indices, function(i)
               FUN(window(x, start = start[i], end = end[i]), ...),
               simplify = simplify)
    } else {
        frequency <- rep(frequency, length.out = n)
        delta <- rep(delta, length.out = n)
        sapply(indices, function(i)
               FUN(window(x, start = start[i], end = end[i],
                   frequency = frequency[i], delta = delta[i]),
                   ...),
               simplify = simplify)
    }
}

setMethod("aggregate", "Vector", .aggregateInternal)

setMethod("aggregate", "vector", .aggregateInternal)

setMethod("aggregate", "matrix", stats:::aggregate.default)

setMethod("aggregate", "data.frame", stats:::aggregate.data.frame)

setMethod("aggregate", "ts", stats:::aggregate.ts)
