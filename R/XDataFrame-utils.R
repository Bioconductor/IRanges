### =========================================================================
### XDataFrame utilities
### -------------------------------------------------------------------------

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Splitting and combining.
###

setMethod("split", "XDataFrame", function(x, f, drop = FALSE) {
  splitInd <- split(seq_len(nrow(x)), f, drop)
  do.call("SplitXDataFrame", lapply(splitInd, function(ind) x[ind,,drop=FALSE]))
})

## we define generics here with just '...' as this is possible in R 2.8
## note that if we omit 'deparse.level' from the formals, things break

setGeneric("cbind", function(..., deparse.level=1) standardGeneric("cbind"),
           signature = "...")

setMethod("cbind", "XDataFrame", function(..., deparse.level=1) {
  XDataFrame(...)
})

setGeneric("rbind", function(..., deparse.level=1) standardGeneric("rbind"),
           signature = "...")

setMethod("rbind", "XDataFrame", function(..., deparse.level=1) {
  args <- list(...)
  args <- args[sapply(args, length) > 0] ## drop zero column args
  if (length(args)) {
    haverows <- sapply(args, nrow) > 0
    if (any(haverows))
      args <- args[haverows] ## drop zero row args
    else return(args[[1]]) ## if none have rows, return first with columns
  } else return(XDataFrame()) ## if none have cols, return empty
  
  xdf <- args[[1]]
  if (length(args) == 1)
    return(xdf)
  
  for (i in seq(length(args))) {
    argi <- args[[i]]
    if (!identical(colnames(xdf), colnames(argi)))
      stop("column names for arg ", i, " do not match those of first arg")
  }
  
  cn <- colnames(xdf)
  cl <- sapply(elements(xdf), class)
  cols <- lapply(seq_len(length(xdf)), function(i) {
### TODO: handle factor levels (c() drops factors to vectors)
    as(do.call("c", lapply(args, `[[`, cn[i])), cl[i])
  })
  names(cols) <- colnames(xdf)
  ans <- do.call("XDataFrame", cols)

  rn <- unlist(lapply(args, rownames))
  if (!is.null(rn)) {
    if (length(rn) != nrow(ans)) {
      rn <- NULL
    } else if (any(duplicated(rn)))
      rn <- make.unique(rn)
  }
  rownames(ans) <- rn
  
  ans
})

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Evaluating
###

setClassUnion("expressionORlanguage", c("expression", "language"))

setMethod("eval", c("expressionORlanguage", "XDataFrame"),
          function(expr, envir,
                   enclos = if(is.list(envir) || is.pairlist(envir))
                   parent.frame() else baseenv())
          {
            env <- new.env(parent = enclos)
            for (col in colnames(envir))
              makeActiveBinding(col, function() {
                val <- envir[[col]]
                rm(list=col, envir=env)
                assign(col, val, env)
                val
              }, env)
            eval(expr, env)
          })
