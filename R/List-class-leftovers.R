### =========================================================================
### IMPORTANT NOTE - 9/4/2014
### Most of the stuff that used to be in the IRanges/R/List-class.R file
### was moved to the S4Vectors package (to R/List-class.R).
### The stuff that could not be moved there was *temporarily* kept here in
### List-class-leftovers.R but will need to find a new home (in S4Vectors
### or in IRanges).
###


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
            df <- DataFrame(.stack.ind(x, index.var),
                            as(unlist(x, use.names=FALSE), "DataFrame"))
            colnames(df)[2] <- value.var
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
            df
          })

