### =========================================================================
### IMPORTANT NOTE - 4/29/2014
### Most of the stuff that used to be in the IRanges/R/Vector-class.R file
### was moved to the S4Vectors package (to R/Vector-class.R).
### The stuff that could not be moved there was *temporarily* kept here in
### Vector-class-leftovers.R but will need to find a new home (in S4Vectors
### or in IRanges).
###

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Other subsetting-related operations
###

### S3/S4 combo for window<-.Vector
`window<-.Vector` <- function(x, start=NA, end=NA, width=NA, ..., value) {
    window(x, start, end, width, ...) <- value
    x
}
`.window<-.Vector` <- function(x, start=NA, end=NA, width=NA, ..., value)
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
setReplaceMethod("window", "Vector", `.window<-.Vector`)

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

setMethod("subset", "Vector",
          function(x, subset, select, drop = FALSE, ...) {
            i <- S4Vectors:::evalqForSubset(subset, x, ...)
            if (!is.null(mcols(x))) {
                j <- S4Vectors:::evalqForSelect(select, mcols(x), ...)
                mcols(x) <- mcols(x)[,j,drop=FALSE]
            }
            x[i, drop=drop]
          })

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
    df <- cbind(df, mcols(combined))
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
### Looping methods
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
    nx <- NROW(X)
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

