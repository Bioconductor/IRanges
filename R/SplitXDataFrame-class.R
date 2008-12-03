### =========================================================================
### SplitXDataFrame objects
### -------------------------------------------------------------------------

## a facade that virtually rbind's an XDataFrameList
## main constraint is that each XDataFrame has the same column names

setClass("SplitXDataFrame", contains = "XDataFrameList")

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Accessor methods.
###

setMethod("dim", "SplitXDataFrame",
          function(x) {
            ncol <- 0
            if (length(x))
              ncol <- ncol(x[[1]])
            as.integer(c(sum(unlist(lapply(elements(x), nrow))), ncol))
          })

setMethod("dimnames", "SplitXDataFrame",
          function(x) {
            list(unlist(lapply(x, rownames), use.names=FALSE),
                 if (length(x)) colnames(x[[1]]) else NULL)
          })

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Validity.
###

.valid.SplitXDataFrame <- function(x) {
  if (length(x)) {
    firstNames <- colnames(x[[1]])
    if (!all(sapply(elements(x),
                    function(df) identical(firstNames, colnames(df)))))
      return("column counts or names differ across elements")
  }
  NULL
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Constructor.
###

SplitXDataFrame <- function(...)
{
  xdfs <- list(...)
  NAMES <- names(xdfs)
  names(xdfs) <- NULL
  new("SplitXDataFrame", elements=xdfs, NAMES=NAMES)
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Splitting and combining.
###

setMethod("unlist", "SplitXDataFrame",
          function(x, recursive = TRUE, use.names = TRUE) {
            if (!missing(recursive))
              warning("'recursive' argument ignored")
            ans <- as(x, "XDataFrame")
            if (!use.names)
              rownames(ans) <- NULL
            ans
          })

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion
###

setAs("SplitXDataFrame", "XDataFrame", function(from) {
  xdf <- XDataFrame(do.call("rbind", elements(from)),
                    row.names = rownames(from))
  xdf@nrows <- nrow(from) # ensure number of rows is preserved
  xdf
})


setMethod("as.data.frame", "SplitXDataFrame",
          function(x, row.names=NULL, optional=FALSE, ...)
          {
            if (!(is.null(row.names) || is.character(row.names)))
              stop("'row.names'  must be NULL or a character vector")
            if (!missing(optional) || length(list(...)))
              warning("'optional' and arguments in '...' ignored")
            as.data.frame(as(x, "XDataFrame"), row.names = row.names)
          })
