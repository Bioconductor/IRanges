### =========================================================================
### expand methods
### -------------------------------------------------------------------------
###

setGeneric("expand", signature="x",
           function(x, ...) 
               standardGeneric("expand")
)

## A helper function to do the work
.expandOneCol <- function(x, colname, keepEmptyRows)
{
    if (!is(x, "DataFrame"))
        stop("'x' must be a DataFrame object")
    if (!isSingleString(colname) && !isSingleNumber(colname))
        stop("'colname' must be a single string or number")
    col <- x[[colname]]
    if (is.null(col))
        stop("'colname' must be a valid colname name or index")
    if(keepEmptyRows){
        col[elementNROWS(col)==0] <- NA
    }
    idx <- rep(seq_len(nrow(x)), elementNROWS(col))
    ans <- x[idx, ]
    ans[[colname]] <- unlist(col, use.names=FALSE)
    ans
}

## A better helper
.expand <- function(x, colnames, keepEmptyRows){
  for(colname in colnames) {
    x <- .expandOneCol(x, colname, keepEmptyRows)
  }
  x
}

### FIXME: should make is.recursive a generic in base R
isRecursive <- function(x) is.recursive(x) || is(x, "List")

defaultIndices <- function(x) {
    which(vapply(x, isRecursive, logical(1L)))
}

setMethod("expand", "DataFrame",
          function(x, colnames, keepEmptyRows = FALSE){
              stopifnot(isTRUEorFALSE(keepEmptyRows))
              if (missing(colnames)) {
                  colnames <- defaultIndices(x)
              }
              .expand(x, colnames, keepEmptyRows)
          }
          )

setMethod("expand", "Vector",
          function(x, colnames, keepEmptyRows = FALSE){
              stopifnot(isTRUEorFALSE(keepEmptyRows))
              if (missing(colnames)) {
                  colnames <- defaultIndices(mcols(x))
              }
              df <- mcols(x)
              df[["__index__"]] <- seq_along(x)
              ex <- .expand(df, colnames, keepEmptyRows)
              mcols(x) <- NULL
              ans <- x[ex[["__index__"]]]
              ex[["__index__"]] <- NULL
              mcols(ans) <- ex
              ans
          }
          )

## Assume that the named columns have the same geometry and expand
## them simultaneously; this is different from the cartesian product
## expansion above.
.expandByColumnSet <- function(x, colnames, keepEmptyRows) {
  if (length(colnames) == 0L)
    return(x)
  if(keepEmptyRows) {
    emptyRows <- elementNROWS(col) == 0L
    x[emptyRows, colnames] <- rep(NA, sum(emptyRows))
  }
  ans <- x[togroup(x[[colnames[1L]]]),,drop=FALSE]
  ans[colnames] <- lapply(x[colnames], unlist, use.names = FALSE)
  ans
}

