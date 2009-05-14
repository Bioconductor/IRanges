### =========================================================================
### XDataFrameList objects
### -------------------------------------------------------------------------

setClass("XDataFrameList", representation("VIRTUAL"),
         prototype = prototype(elementType = "XDataFrame"),
         contains = "AnnotatedTypedListV2")
setClass("SimpleXDataFrameList",
         prototype = prototype(elementType = "XDataFrame"),
         contains = c("AnnotatedSimpleTypedList", "XDataFrameList"))

setClass("SplitXDataFrameList", representation("VIRTUAL"),
         prototype = prototype(elementType = "XDataFrame"),
         contains = "XDataFrameList")
setClass("SimpleSplitXDataFrameList",
         prototype = prototype(elementType = "XDataFrame"),
         contains = c("SplitXDataFrameList", "SimpleXDataFrameList"))
setClass("CompressedSplitXDataFrameList",
         prototype = prototype(elementType = "XDataFrame",
                               unlistData = new("XDataFrame")),
         contains = c("SplitXDataFrameList", "AnnotatedCompressedTypedList"))

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Accessor methods.
###

setMethod("dim", "SimpleXDataFrameList",
          function(x) {
            if (length(x) == 0L)
              c(0L, 0L)
            else {
              nrow <- sum(unlist(lapply(as.list(x, use.names = FALSE), nrow)))
              ncol <- ncol(x[[1]])
              as.integer(c(nrow, ncol))
            }
          })
  
setMethod("dim", "CompressedSplitXDataFrameList",
          function(x) {
            if (length(x) == 0L)
              c(0L, 0L)
            else
              dim(unlist(x))
          })

### FIXME: make separate rownames, colnames methods, because the
### rownames calculation can be _very_ slow
setMethod("dimnames", "SimpleXDataFrameList",
          function(x) {
            if (length(x) == 0L)
              list(character(), character())
            else
              list(unlist(lapply(x, rownames), use.names=FALSE),
                   colnames(x[[1]]))
          })

setMethod("dimnames", "CompressedSplitXDataFrameList",
          function(x) {
            if (length(x) == 0L)
              list(character(), character())
            else
              dimnames(unlist(x))
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
  TypedListV2("SimpleXDataFrameList", list(...))
}

SplitXDataFrameList <- function(..., compress = FALSE)
{
  if (compress)
    listClass <- "CompressedSplitXDataFrameList"
  else
    listClass <- "SimpleSplitXDataFrameList"
  TypedListV2(listClass, list(...))
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
