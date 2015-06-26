### =========================================================================
### IMPORTANT NOTE - 9/4/2014
### Most of the stuff that used to be in the IRanges/R/List-class.R file
### was moved to the S4Vectors package (to R/List-class.R).
### The stuff that could not be moved there was *temporarily* kept here in
### List-class-leftovers.R but will need to find a new home (in S4Vectors
### or in IRanges).
###

## NOTE: while the 'c' function does not have an 'x', the generic does
## c() is a primitive, so 'x' can be missing; dispatch is by position,
## although sometimes this does not work so well, so it's best to keep
## names off the parameters whenever feasible.
setMethod("c", "SimpleList",
          function(x, ..., recursive = FALSE) {
              slot(x, "listData") <-
                do.call(c, lapply(unname(list(x, ...)), as.list))
              if (!is.null(mcols(x)))
                mcols(x) <- S4Vectors:::rbind_mcols(x, ...)
              x
          })

.stack.ind <- function(x, index.var = "name") {
  if (length(names(x)) > 0) {
    spaceLabels <- names(x)
  } else {
    spaceLabels <- seq_len(length(x))
  }
  ind <- Rle(factor(spaceLabels, levels = unique(spaceLabels)),
             elementLengths(x))
  do.call(DataFrame, structure(list(ind), names = index.var))
}

### FIXME: need a recursive argument, when TRUE we call stack on
### unlist result, instead of coercing to DataFrame.

setMethod("stack", "List",
          function(x, index.var = "name", value.var = "value", name.var = NULL)
          {
            value <- unlist(x, use.names=FALSE)
            index <- .stack.ind(x, index.var)
            unlistsToList <- extends(x@elementType, "List")
            if (unlistsToList) {
              df <- cbind(S4Vectors:::ensureMcols(value), index)
            } else {
              df <- DataFrame(index, as(value, "DataFrame"))
              colnames(df)[2] <- value.var
            }
            if (!is.null(name.var)) {
              nms <- as.character(unlist(lapply(x, names)))
              if (length(nms) == 0L) {
                rngs <- IRanges(1L, width=elementLengths(x))
                nms <- as.character(as.integer(rngs))
              }
              df[[name.var]] <- factor(nms, unique(nms))
            }
            if (!is.null(mcols(x))) {
              df <- cbind(df, mcols(x))
            }
            if (unlistsToList) {
              mcols(value) <- df
              value
            } else {
              df
            }
          })

