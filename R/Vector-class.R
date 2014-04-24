### =========================================================================
### Vector objects
### -------------------------------------------------------------------------
###
### The Vector virtual class is a general container for storing a finite
### sequence i.e. an ordered finite collection of elements.
###

### Is it the right place for this?
setClassUnion("vectorORfactor", c("vector", "factor"))

setClassUnion("DataTableORNULL", c("DataTable", "NULL"))

setClass("Vector",
    contains="Annotated",
    representation(
        "VIRTUAL",
        elementMetadata="DataTableORNULL"
    )
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Internal utility.
###

setGeneric("showAsCell",  # not exported
    function(object) standardGeneric("showAsCell")
)

setMethod("showAsCell", "ANY", function(object) {
  if (length(dim(object)) > 2)
    dim(object) <- c(nrow(object), prod(tail(dim(object), -1)))
  if (NCOL(object) > 1) {
    class(object) <- setdiff(class(object), "AsIs")
    df <- as.data.frame(object[, head(seq_len(ncol(object)), 3), drop = FALSE])
    attempt <- do.call(paste, df)
    if (ncol(object) > 3)
      attempt <- paste(attempt, "...")
    attempt
  } else if (NCOL(object) == 0L) {
    rep.int("", NROW(object))
  } else {
    attempt <- try(as.vector(object), silent=TRUE)
    if (is(attempt, "try-error"))
      rep.int("########", length(object))
    else attempt
  }
})
setMethod("showAsCell", "list", function(object)
          rep.int("########", length(object)))
setMethod("showAsCell", "Vector", function(object)
          rep.int("########", length(object)))


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Accessor methods.
###

setMethod("NROW", "Vector", function(x) length(x))

### 3 accessors for the same slot: elementMetadata(), mcols(), and values().
### mcols() is the recommended one, use of elementMetadata() or values() is
### discouraged.
setGeneric("elementMetadata",
    function(x, use.names=FALSE, ...) standardGeneric("elementMetadata")
)

setMethod("elementMetadata", "Vector",
    function(x, use.names=FALSE, ...)
    {
        if (!isTRUEorFALSE(use.names)) 
            stop("'use.names' must be TRUE or FALSE")
        ans <- x@elementMetadata
        if (use.names && !is.null(ans))
            rownames(ans) <- names(x)
        ans
    }
)

setGeneric("mcols",
    function(x, use.names=FALSE, ...) standardGeneric("mcols")
)

setMethod("mcols", "Vector",
    function(x, use.names=FALSE, ...)
        elementMetadata(x, use.names=use.names, ...)
)

setGeneric("values", function(x, ...) standardGeneric("values"))

setMethod("values", "Vector", function(x, ...) elementMetadata(x, ...))

setGeneric("elementMetadata<-",
           function(x, ..., value) standardGeneric("elementMetadata<-"))

setReplaceMethod("elementMetadata", "Vector",
                 function(x, ..., value) {
                     if (!is(value, "DataTableORNULL"))
                         stop("replacement 'elementMetadata' value must be a DataTable object or NULL")
                     if ("elementMetadata" %in% names(attributes(x))) {
                         if (!is.null(value) && length(x) != nrow(value))
                             stop("the number of rows in elementMetadata 'value' ",
                                     "(if non-NULL) must match the length of 'x'")
                         if (!is.null(value))
                             rownames(value) <- NULL
                         x@elementMetadata <- value
                     }
                     x
                 })

setGeneric("mcols<-", function(x, ..., value) standardGeneric("mcols<-"))

setReplaceMethod("mcols", "Vector",
    function(x, ..., value) `elementMetadata<-`(x, ..., value=value)
)

setGeneric("values<-", function(x, ..., value) standardGeneric("values<-"))

setReplaceMethod("values", "Vector",
                 function(x, value) {
                     elementMetadata(x) <- value
                     x
                 })

setGeneric("rename", function(x, value, ...) standardGeneric("rename"))

.renameVector <- function(x, value, ...) {
  if (missing(value))
    newNames <- c(...)
  else newNames <- c(value, ...)
  badOldNames <- setdiff(names(newNames), names(x))
  if (length(badOldNames))
    stop("Some 'from' names in value not found on 'x': ",
         paste(badOldNames, collapse = ", "))
  names(x)[match(names(newNames), names(x))] <- newNames
  x
}

setMethod("rename", "vector", .renameVector)
setMethod("rename", "Vector", .renameVector)

setMethod("anyNA", "Vector", function(x) any(is.na(x)))

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Validity.
###

.valid.Vector.length <- function(x)
{
    x_len <- length(x)
    if (!isSingleInteger(x_len) || x_len < 0L)
        return("'length(x)' must be a single non-negative integer")
    if (!is.null(names(x_len)))
        return("'length(x)' must be an unnamed number")
    NULL
}

.valid.Vector.names <- function(x)
{
    x_names <- names(x)
    if (is.null(x_names))
        return(NULL)
    if (!is.character(x_names) || !is.null(names(x_names)))
        return("'names(x)' must be NULL or an unnamed character vector")
    if (length(x_names) != length(x))
        return("when not NULL, 'names(x)' must have the length of 'x'")
    NULL
}

.valid.Vector.mcols <- function(x)
{
    x_mcols <- mcols(x)
    if (!is(x_mcols, "DataTableORNULL"))
        return("'mcols(x)' must be a DataTable object or NULL")
    if (is.null(x_mcols))
        return(NULL)
    ## 'x_mcols' is a DataTable object.
    if (nrow(x_mcols) != length(x)) {
        msg <- c("number of rows in DataTable 'mcols(x)' ",
                 "must match length of 'x'")
        return(paste(msg, collapse=""))
    }
    if (!is.null(rownames(x_mcols)) && !identical(rownames(x_mcols), names(x))) {
        msg <- c("the rownames of DataTable 'mcols(x)' ",
                 "must match the names of 'x'")
        return(paste(msg, collapse=""))
    }
    NULL
}

.valid.Vector <- function(x)
{
    c(.valid.Vector.length(x),
      .valid.Vector.names(x),
      .valid.Vector.mcols(x))
}
setValidity2("Vector", .valid.Vector)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Subsetting.
###

setMethod("extractROWS", c("NULL", "ANY"), function(x, i) NULL)

.extractROWSWithBracket <- function(x, i) {
  if (is(i, "Ranges"))
    i <- extractROWS(seq_len(NROW(x)), i)
  ## dynamically call [i,,,..,drop=FALSE] with as many "," as length(dim)-1
  ndim <- max(length(dim(x)), 1L)
  i <- normalizeSingleBracketSubscript(i, x, byrow = ndim > 1L)
  args <- rep(alist(foo=), ndim)
  names(args) <- NULL
  args[[1]] <- i
  args <- c(list(x), args, list(drop = FALSE))
  do.call(`[`, args)
}

setMethod("extractROWS", c("matrix", "ANY"), function(x, i) {
  if (missing(i))
    return(x)
  return(.extractROWSWithBracket(x, i))
})

setMethod("extractROWS", c("vectorORfactor", "ANY"),
    function(x, i)
    {
        if (missing(i))
            return(x)
        if (is(i, "Rle")) {
            if(is.logical(runValue(i))) {
                i <- as(i, "IRanges")
            } else {
                i <- as.vector(i)
            }
        }
        if (!is(i, "Ranges")) {
            return(x[i])
        }
        ## Which one is faster, vector_seqselect or vector_subsetByRanges?
        ans <- .Call2("vector_seqselect", x, start(i), width(i),
                      PACKAGE="IRanges")
        #ans <- .Call2("vector_subsetByRanges", x, start(i), width(i),
        #              PACKAGE="IRanges")
        if (is.factor(x))
            attributes(ans) <- list(levels=levels(x), class="factor")
        ans
    }
)

setMethod("extractROWS", c("ANY", "ANY"),
          function(x, i)
          {
            if (missing(i))
              return(x)
            .extractROWSWithBracket(x, i)
          })

setMethod("[", "Vector",
    function(x, i, j, ..., drop=TRUE)
    {
        if (!missing(j) || length(list(...)) > 0L)
            stop("invalid subsetting")
        extractROWS(x, i)
    }
)

setMethod("replaceROWS", c("vectorORfactor", "ANY"),
    function(x, i, value)
    {
        i <- extractROWS(setNames(seq_along(x), names(x)), i)
        x[i] <- value
        x
    }
)

### Works on any Vector object for which c() and [ work. Assumes 'value' is
### compatible with 'x'.
setMethod("replaceROWS", c("Vector", "ANY"),
    function(x, i, value)
    {
        idx <- seq_along(x)
        i <- extractROWS(setNames(idx, names(x)), i)
        ## Assuming that objects of class 'class(x)' can be combined with c().
        ans <- c(x, value)
        idx[i] <- length(x) + seq_len(length(value))
        ## Assuming that [ works on objects of class 'class(x)'.
        ans <- ans[idx]
        ## Restore the original decoration.
        metadata(ans) <- metadata(x)
        names(ans) <- names(x)
        mcols(ans) <- mcols(x)
        ans
    }
)

setReplaceMethod("[", "Vector",
    function(x, i, j, ..., value)
    {
        if (!missing(j) || length(list(...)) > 0L)
            stop("invalid subsetting")
        if (missing(i) || !is(i, "Ranges"))
            i <- normalizeSingleBracketSubscript(i, x)
        if (is(i, "Ranges"))
            li <- sum(width(i))
        else
            li <- length(i)
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
        replaceROWS(x, i, value)
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Simple helper functions for some common subsetting operations.
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

setGeneric("fixedColumnNames", function(x) standardGeneric("fixedColumnNames"))

setMethod("fixedColumnNames", "ANY", function(x) character())

setMethod("subset", "Vector",
          function(x, subset, select, drop = FALSE, ...) {
            i <- S4Vectors:::evalqForSubset(subset, x, ...)
            j <- S4Vectors:::evalqForSelect(select, mcols(x), ...)
            mcols(x) <- mcols(x)[,j,drop=FALSE]
            x[i, drop=drop]
          })

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

setMethod("as.logical", "Vector",
    function(x) as.vector(x, mode="logical")
)
setMethod("as.integer", "Vector",
    function(x) as.vector(x, mode="integer")
)
setMethod("as.numeric", "Vector",
    function(x) as.vector(x, mode="numeric")
)
setMethod("as.double", "Vector",
    function(x) as.vector(x, mode="double")
)
setMethod("as.complex", "Vector",
    function(x) as.vector(x, mode="complex")
)
setMethod("as.character", "Vector",
    function(x) as.vector(x, mode="character")
)
setMethod("as.raw", "Vector",
    function(x) as.vector(x, mode="raw")
)

setAs("Vector", "vector", function(from) as.vector(from))
setAs("Vector", "logical", function(from) as.logical(from))
setAs("Vector", "integer", function(from) as.integer(from))
setAs("Vector", "numeric", function(from) as.numeric(from))
setAs("Vector", "complex", function(from) as.complex(from))
setAs("Vector", "character", function(from) as.character(from))
setAs("Vector", "raw", function(from) as.raw(from))

setAs("Vector", "data.frame", function(from) as.data.frame(from))

### S3/S4 combo for as.data.frame.Vector
as.data.frame.Vector <- function(x, row.names=NULL, optional=FALSE, ...)
{
    x <- as.vector(x)
    as.data.frame(x, row.names=NULL, optional=optional, ...)
}
setMethod("as.data.frame", "Vector", as.data.frame.Vector)

makeFixedColumnEnv <- function(x, parent, tform = identity) {
  env <- new.env(parent=parent)
  lapply(fixedColumnNames(x), function(nm) {
    accessor <- get(nm, parent, mode="function")
    makeActiveBinding(nm, function() {
      val <- tform(accessor(x))
      rm(list=nm, envir=env)
      assign(nm, val, env)
      val
    }, env)
  })
  env
}

setMethod("as.env", "Vector", function(x, enclos, tform = identity) {
  S4Vectors:::addSelfRef(x, makeFixedColumnEnv(x, as.env(mcols(x), enclos, tform), tform))
})

as.list.Vector <- function(x) {
  as.list(as(x, "List"))
}

setMethod("as.list", "Vector", as.list.Vector)

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
    l <- list(x, ...)
    l_mcols <- lapply(l, mcols)
    no_mcols <- sapply(l_mcols, is.null)
    if (all(no_mcols))
        return(NULL)
    newDf <- function(nr) new("DataFrame", nrows = nr)
    l_mcols[no_mcols] <- lapply(elementLengths(l[no_mcols]), newDf)
    allCols <- unique(do.call(c, lapply(l_mcols, colnames)))
    fillCols <- function(df) {
      if (nrow(df))
          df[setdiff(allCols, colnames(df))] <- DataFrame(NA)
      df
    }
    do.call(rbind, lapply(l_mcols, fillCols))
}

.c.Vector <- function(x, ..., recursive = FALSE)
{
    if (!is.null(mcols(x)))
      mcols(x) <- rbind.mcols(x, ...)
    x
}

setMethod("c", "Vector",
          function(x, ..., recursive = FALSE)
          stop("missing 'c' method for Vector class ", class(x)))

### FIXME: This method doesn't work properly on DataTable objects if 'after'
### is >= 1 and < length(x).
setMethod("append", c("Vector", "Vector"),
    function(x, values, after=length(x))
    {
        if (!isSingleNumber(after))
            stop("'after' must be a single number")
        x_len <- length(x)
        if (after == 0L)
            c(values, x)
        else if (after >= x_len)
            c(x, values)
        else
            c(head(x, n=after), values, tail(x, n=-after))
    }
)


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

setGeneric("shiftApply", signature = c("X", "Y"),
           function(SHIFT, X, Y, FUN, ..., OFFSET = 0L, simplify = TRUE,
                    verbose = FALSE)
           standardGeneric("shiftApply"))

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
