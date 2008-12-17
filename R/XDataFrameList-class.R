### =========================================================================
### XDataFrameList objects
### -------------------------------------------------------------------------

setClass("XDataFrameList",
         prototype = prototype(elementClass="XDataFrame", compressible = TRUE),
         contains = "TypedList")

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Accessor methods.
###

setMethod("dim", "XDataFrameList",
          function(x) {
            ncol <- 0
            if (length(x))
              ncol <- ncol(x[[1]])
            as.integer(c(sum(unlist(lapply(as.list(x, use.names = FALSE), nrow))), ncol))
          })

setMethod("dimnames", "XDataFrameList",
          function(x) {
            list(unlist(lapply(x, rownames), use.names=FALSE),
                 if (length(x)) colnames(x[[1]]) else NULL)
          })

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Validity.
###

.valid.XDataFrameList <- function(x) {
  if (length(x)) {
    firstNames <- colnames(x[[1]])
    if (!all(sapply(as.list(x, use.names = FALSE),
                    function(df) identical(firstNames, colnames(df)))))
      return("column counts or names differ across elements")
  }
  NULL
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Constructor.
###

XDataFrameList <- function(..., compress = TRUE)
{
  TypedList("XDataFrameList", elements = list(...), compress = compress)
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion
###

setAs("XDataFrameList", "XDataFrame", function(from) unlist(from))

setMethod("as.data.frame", "XDataFrameList",
          function(x, row.names=NULL, optional=FALSE, ...)
          {
            if (!(is.null(row.names) || is.character(row.names)))
              stop("'row.names'  must be NULL or a character vector")
            if (!missing(optional) || length(list(...)))
              warning("'optional' and arguments in '...' ignored")
            as.data.frame(as(x, "XDataFrame"), row.names = row.names)
          })
