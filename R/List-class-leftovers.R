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
             elementNROWS(x))
  do.call(DataFrame, structure(list(ind), names = index.var))
}

### FIXME: need a recursive argument, when TRUE we call stack on
### unlist result, instead of coercing to DataFrame.

setMethod("stack", "List",
          function(x, index.var = "name", value.var = "value", name.var = NULL)
          {
            value <- unlist(x, use.names=FALSE)
            index <- .stack.ind(x, index.var)
            unlistsToVector <- is(value, "Vector")
            if (unlistsToVector) {
                df <- cbind(index, S4Vectors:::ensureMcols(unname(value)))
            } else {
              df <- DataFrame(index, as(value, "DataFrame"))
              colnames(df)[2] <- value.var
            }
            if (!is.null(name.var)) {
              nms <- as.character(unlist(lapply(x, names)))
              if (length(nms) == 0L) {
                rngs <- IRanges(1L, width=elementNROWS(x))
                nms <- as.integer(rngs)
              } else {
                nms <- factor(nms, unique(nms))
              }
              df[[name.var]] <- nms
              df <- df[c(index.var, name.var, value.var)]
            }
            if (!is.null(mcols(x))) {
                df <- cbind(df,
                            mcols(x)[togroup(PartitioningByEnd(x)),,drop=FALSE])
            }
            if (unlistsToVector) {
              mcols(value) <- df
              value
            } else {
              df
            }
          })

setMethod("stack", "matrix",
          function(x, row.var = names(dimnames(x))[1L],
                   col.var = names(dimnames(x))[2L])
          {
              l <- x
              attributes(l) <- NULL
              lens <- elementNROWS(l)
              rn <- rownames(x)
              if (is.null(rn))
                  rn <- seq_along(nrow(x))
              cn <- colnames(x)
              if (is.null(cn))
                  cn <- seq_along(ncol(x))
              ans <- DataFrame(row=rep(rn[row(x)], lens),
                               col=rep(Rle(cn, rep(nrow(x), ncol(x))), lens),
                               stack(List(l)))
              if (is.null(row.var)) row.var <- "row"
              if (is.null(col.var)) col.var <- "col"
              colnames(ans) <- c(row.var, col.var)
              ans
          })
