### =========================================================================
### DataFrameList objects
### -------------------------------------------------------------------------

setClass("DataFrameList", representation("VIRTUAL"),
         prototype = prototype(elementType = "DataFrame"),
         contains = "Sequence")
setClass("SimpleDataFrameList",
         prototype = prototype(elementType = "DataFrame"),
         contains = c("SimpleList", "DataFrameList"))

setClass("SplitDataFrameList", representation("VIRTUAL"),
         prototype = prototype(elementType = "DataFrame"),
         contains = "DataFrameList")
setClass("SimpleSplitDataFrameList",
         prototype = prototype(elementType = "DataFrame"),
         contains = c("SplitDataFrameList", "SimpleDataFrameList"))
setClass("CompressedSplitDataFrameList",
         prototype = prototype(elementType = "DataFrame",
                               unlistData = new("DataFrame")),
         contains = c("SplitDataFrameList", "CompressedList"))

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Accessor methods.
###

setMethod("dim", "SimpleDataFrameList",
          function(x) {
            if (length(x) == 0L)
              c(0L, 0L)
            else {
              nrow <- sum(unlist(lapply(as.list(x, use.names = FALSE), nrow)))
              ncol <- ncol(x[[1]])
              as.integer(c(nrow, ncol))
            }
          })
  
setMethod("dim", "CompressedSplitDataFrameList",
          function(x) {
            if (length(x) == 0L)
              c(0L, 0L)
            else
              dim(unlist(x))
          })

### FIXME: make separate rownames, colnames methods, because the
### rownames calculation can be _very_ slow
setMethod("dimnames", "SimpleDataFrameList",
          function(x) {
            if (length(x) == 0L)
              list(character(), character())
            else
              list(unlist(lapply(x, rownames), use.names=FALSE),
                   colnames(x[[1]]))
          })

setMethod("dimnames", "CompressedSplitDataFrameList",
          function(x) {
            if (length(x) == 0L)
              list(character(), character())
            else
              dimnames(unlist(x))
          })

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Validity.
###

.valid.SplitDataFrameList <- function(x) {
  if (length(x)) {
    firstNames <- colnames(x[[1]])
    if (!all(sapply(as.list(x, use.names = FALSE),
                    function(df) identical(firstNames, colnames(df)))))
      return("column counts or names differ across elements")
  }
  NULL
}

setValidity2("SplitDataFrameList", .valid.SplitDataFrameList)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Constructor.
###

DataFrameList <- function(...)
{
  listData <- list(...)
  if (length(listData) == 1 && is.list(listData[[1]]))
    listData <- listData[[1]]
  newSimpleList("SimpleDataFrameList", listData)
}

SplitDataFrameList <- function(..., compress = TRUE)
{
  listData <- list(...)
  if (length(listData) == 1 && is.list(listData[[1]]))
    listData <- listData[[1]]
  if (length(listData) > 0 && !is(listData[[1]], "DataFrame")) {
    if (is.null(names(listData)))
      names(listData) <- paste("X", seq_len(length(listData)), sep = "")
    listData <- do.call(Map, c(list(DataFrame), listData))
  }
  if (compress)
    newCompressedList("CompressedSplitDataFrameList", listData)
  else
    newSimpleList("SimpleSplitDataFrameList", listData)
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Subsetting.
###

setMethod("[", "SimpleSplitDataFrameList",
          function(x, i, j, ..., drop)
          {
            if (!missing(j))
              x@listData <- lapply(x@listData, function(y) y[,j,drop=FALSE])
            if (!missing(i))
              x <- callNextMethod(x, i)

            if (((nargs() - !missing(drop)) > 2) &&
                (length(x@listData) > 0) && (ncol(x@listData[[1]]) == 1) &&
                (missing(drop) || drop)) {
              uniqueClasses <-
                unique(unlist(lapply(x@listData, function(y) class(y[[1]]))))
              if (all(uniqueClasses %in% 
                      c("raw", "logical", "integer", "numeric", "character",
                        "complex", "Rle")))
                x <- SimpleAtomicList(lapply(x@listData, "[[", 1))
              else if (identical(uniqueClasses, "IRanges"))
                x <- IRangesList(lapply(x@listData, "[[", 1), compress=FALSE)
              else if (unlist(lapply(uniqueClasses,
                                     function(y) extends(y, "Ranges"))))
                x <- RangesList(lapply(x@listData, "[[", 1))
            }

            x
          })

setMethod("[", "CompressedSplitDataFrameList",
          function(x, i, j, ..., drop)
          {
            if (!missing(j))
              x@unlistData <- x@unlistData[, j, drop=FALSE]
            if (!missing(i))
              x <- callNextMethod(x, i)

            if (((nargs() - !missing(drop)) > 2) &&
                (ncol(x@unlistData) == 1) && (missing(drop) || drop)) {
              dataClass <- class(x@unlistData[[1]])
              if (dataClass %in% 
                  c("raw", "logical", "integer", "numeric", "character",
                    "complex", "Rle"))
                x <-
                  CompressedAtomicList(x@unlistData[[1]],
                                       partitioning = x@partitioning)
              else if (dataClass == "IRanges")
                x <-
                  new2("CompressedIRangesList", unlistData = x@unlistData[[1]],
                       partitioning = x@partitioning)
            }

            x
          })

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion
###

setAs("SplitDataFrameList", "DataFrame", function(from) unlist(from))

setMethod("as.data.frame", "SplitDataFrameList",
          function(x, row.names=NULL, optional=FALSE, ...)
          {
            if (!(is.null(row.names) || is.character(row.names)))
              stop("'row.names'  must be NULL or a character vector")
            if (!missing(optional) || length(list(...)))
              warning("'optional' and arguments in '...' ignored")
            as.data.frame(as(x, "DataFrame"), row.names = row.names)
          })
