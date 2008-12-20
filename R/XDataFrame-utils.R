### =========================================================================
### XDataFrame utilities
### -------------------------------------------------------------------------

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Splitting and combining.
###

setMethod("split", "XDataFrame",
          function(x, f, drop = FALSE) {
            TypedList("SplitXDataFrameList", elements = x, splitFactor = f)
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
  hasrows <- unlist(lapply(args, nrow)) > 0
  hascols <- unlist(lapply(args, ncol)) > 0

  if (!any(hasrows | hascols)) {
    return(XDataFrame())
  } else if (!any(hasrows)) {
    return(args[[which(hascols)[1]]])
  } else if (sum(hasrows) == 1) {
    return(args[[which(hasrows)]])
  } else {
    args <- args[hasrows]
  }

  xdf <- args[[1]]

  for (i in 2:length(args)) {
    if (ncol(xdf) != ncol(args[[i]]))
      stop("number of columns for arg ", i, " do not match those of first arg")
    if (!identical(colnames(xdf), colnames(args[[i]])))
      stop("column names for arg ", i, " do not match those of first arg")
  }

  if (ncol(xdf) == 0) {
    ans <- XDataFrame()
    ans@nrows <- sum(unlist(lapply(args, nrow)))
  } else {
    cn <- colnames(xdf)
    cl <- unlist(lapply(as.list(xdf, use.names = FALSE), class))
    factors <- unlist(lapply(as.list(xdf, use.names = FALSE), is.factor))
    cols <- lapply(seq_len(length(xdf)), function(i) {
      cols <- lapply(args, `[[`, cn[i])
      if (factors[i]) { # combine factor levels, coerce to character
        levs <- unique(do.call(c, lapply(cols, levels)))
        cols <- lapply(cols, as.character)
      }
      combined <- do.call(c, cols)
      if (factors[i])
        combined <- factor(combined, levs)
      ## this coercion needed only because we extracted ([[) above
      ## which brings external -> internal
      ## external objects should support external combination (c)
      as(combined, cl[i])
    })
    names(cols) <- colnames(xdf)
    ans <- do.call(XDataFrame, cols)
  }

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
