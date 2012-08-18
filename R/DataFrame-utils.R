### =========================================================================
### DataFrame utilities
### -------------------------------------------------------------------------

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Splitting and combining.
###

setMethod("splitAsListReturnedClass", "data.frame",
    function(x) "CompressedSplitDataFrameList"
)

setMethod("splitAsListReturnedClass", "DataFrame",
    function(x) "CompressedSplitDataFrameList"
)

setMethod("cbind", "DataFrame",
          function(..., deparse.level=1) {
            ans <- DataFrame(...)
            mcols(ans) <- rbind.mcols(...)
            ans
          })

setMethod("rbind", "DataFrame", function(..., deparse.level=1) {
  args <- list(...)
  hasrows <- unlist(lapply(args, nrow), use.names=FALSE) > 0L
  hascols <- unlist(lapply(args, ncol), use.names=FALSE) > 0L

  if (!any(hasrows | hascols)) {
    return(DataFrame())
  } else if (!any(hasrows)) {
    return(args[[which(hascols)[1L]]])
  } else if (sum(hasrows) == 1) {
    return(args[[which(hasrows)]])
  } else {
    args <- args[hasrows]
  }

  df <- args[[1L]]

  for (i in 2:length(args)) {
    if (ncol(df) != ncol(args[[i]]))
      stop("number of columns for arg ", i, " do not match those of first arg")
    if (!identical(colnames(df), colnames(args[[i]])))
      stop("column names for arg ", i, " do not match those of first arg")
  }

  if (ncol(df) == 0) {
    ans <- DataFrame()
    ans@nrows <- sum(unlist(lapply(args, nrow), use.names=FALSE))
  } else {
    cn <- colnames(df)
    cl <- unlist(lapply(as.list(df, use.names = FALSE), class))
    factors <- unlist(lapply(as.list(df, use.names = FALSE), is.factor))
    cols <- lapply(seq_len(length(df)), function(i) {
      cols <- lapply(args, `[[`, cn[i])
      if (factors[i]) { # combine factor levels, coerce to character
        levs <- unique(unlist(lapply(cols, levels), use.names=FALSE))
        cols <- lapply(cols, as.character)
      }
      combined <- do.call(c, unname(cols))
      if (factors[i])
        combined <- factor(combined, levs)
      ## this coercion needed only because we extracted ([[) above
      ## which brings external -> internal
      ## external objects should support external combination (c)
      as(combined, cl[i])
    })
    names(cols) <- colnames(df)
    ans <- do.call(DataFrame, cols)
  }

  rn <- unlist(lapply(args, rownames), use.names=FALSE)
  if (!is.null(rn)) {
    if (length(rn) != nrow(ans)) {
      rn <- NULL
    } else if (anyDuplicated(rn))
      rn <- make.unique(rn, sep = "")
  }
  rownames(ans) <- rn

  if (!is.null(mcols(df))) {
    df_mcols <- mcols(df)
    if (all(sapply(args, function(x) identical(mcols(x), df_mcols))))
      mcols(ans) <- df_mcols
  }
  
  ans
})

## We are overriding all 'formula' calls to aggregate, and
## stats:::aggregate.formula depends on quoting a formula expression
## (x ~ y) in its first argument. Thus, we need some computing on the
## language, which may not be very robust.
setMethod("aggregate", "formula", function(x, data, ...) {
  mc <- sys.call(-1)
  mc[[1]] <- quote(stats:::aggregate.formula)
  if (is(data, "DataFrame")) {
    data <- as.data.frame(data)
    ## depending on the formula, this may or not be a valid subclass
    ## of DataFrame, so we just explicitly create a DataFrame here
    ##DataFrame(callGeneric())
    mc[[3]] <- data
    DataFrame(eval(mc, parent.frame(2)))
  } else eval(mc, parent.frame(2)) ## for e.g. data.frame
})
