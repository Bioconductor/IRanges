### =========================================================================
### XDataFrameList objects
### -------------------------------------------------------------------------

setClass("XDataFrameList",
         prototype = prototype(elementClass="XDataFrame", compressible = FALSE),
         contains = "TypedList")

setClass("SplitXDataFrameList",
         prototype = prototype(elementClass="XDataFrame", compressible = FALSE),
         contains = "XDataFrameList")

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

### FIXME: make separate rownames, colnames methods, because the
### rownames calculation can be _very_ slow
setMethod("dimnames", "XDataFrameList",
          function(x) {
            list(unlist(lapply(x, rownames), use.names=FALSE),
                 if (length(x)) colnames(x[[1]]) else NULL)
          })

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Validity.
###

.valid.SplitXDataFrameList <- function(x) {
  if (length(x)) {
    firstNames <- colnames(x[[1]])
    if (!all(sapply(as.list(x, use.names = FALSE),
                    function(df) identical(firstNames, colnames(df)))))
      return("column counts or names differ across elements")
  }
  NULL
}

setValidity2("SplitXDataFrameList", .valid.SplitXDataFrameList)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Constructor.
###

XDataFrameList <- function(...)
{
  TypedList("XDataFrameList", elements = list(...), compress = FALSE)
}

SplitXDataFrameList <- function(...)
{
  TypedList("SplitXDataFrameList", elements = list(...), compress = FALSE)
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion
###

setAs("SplitXDataFrameList", "XDataFrame", function(from) unlist(from))

setMethod("as.data.frame", "SplitXDataFrameList",
          function(x, row.names=NULL, optional=FALSE, ...)
          {
            if (!(is.null(row.names) || is.character(row.names)))
              stop("'row.names'  must be NULL or a character vector")
            if (!missing(optional) || length(list(...)))
              warning("'optional' and arguments in '...' ignored")
            as.data.frame(as(x, "XDataFrame"), row.names = row.names)
          })
