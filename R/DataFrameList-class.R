### =========================================================================
### DataFrameList objects
### -------------------------------------------------------------------------

setClass("DataFrameList", representation("VIRTUAL"),
         prototype = prototype(elementType = "DataFrame"),
         contains = "Sequence")
setClass("SimpleDataFrameList",
         prototype = prototype(elementType = "DataFrame"),
         contains = c("DataFrameList", "SimpleList"))

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

setMethod("nrow", "DataFrameList",
          function(x)
          {
            if (length(x) == 0L)
              0L
            else
              elementLengths(x)
          })

setMethod("ncol", "DataFrameList",
          function(x)
          {
            if (length(x) == 0L)
              0L
            else
              unlist(lapply(x, ncol))
          })

setMethod("dim", "DataFrameList",
          function(x)
          {
            cbind(nrow(x), ncol(x))
          })

setMethod("rownames", "DataFrameList",
          function(x, do.NULL = TRUE, prefix = "row")
          {
            CharacterList(lapply(x, rownames, do.NULL = do.NULL, prefix = prefix))
          })

setMethod("colnames", "DataFrameList",
          function(x, do.NULL = TRUE, prefix = "col")
          {
            CharacterList(lapply(x, colnames, do.NULL = do.NULL, prefix = prefix))
          })

setMethod("dimnames", "DataFrameList",
          function(x)
          {
            list(rownames(x), colnames(x))
          })

setReplaceMethod("rownames", "SimpleDataFrameList",
                 function(x, value)
                 {
                   if (is.null(value)) {
                     x@listData <-
                       lapply(x@listData, function(y) {rownames(x) <- NULL; x})
                   } else if (is(value, "CharacterList")){
                     if (length(x) != length(value))
                       stop("replacement value must be the same length as x")
                     for (i in seq_len(length(x)))
                       rownames(x@listData[[i]]) <- value[[i]]
                   } else {
                     stop("replacement value must either be NULL or a CharacterList")
                   }
                   x
                 })

setReplaceMethod("rownames", "CompressedSplitDataFrameList",
                 function(x, value)
                 {
                   if (is.null(value)) {
                     rownames(x@unlistData) <- NULL
                   } else if (is(value, "CharacterList")){
                     if (length(x) != length(value))
                       stop("replacement value must be the same length as x")
                     rownames(x@unlistData) <- unlist(value, use.names=FALSE)
                   } else {
                     stop("replacement value must either be NULL or a CharacterList")
                   }
                   x
                 })

setReplaceMethod("colnames", "SimpleDataFrameList",
                 function(x, value)
                 {
                   if (is.null(value)) {
                     x@listData <-
                       lapply(x@listData, function(y) {colnames(x) <- NULL; x})
                   } else if (is.character(value)) {
                     for (i in seq_len(length(x)))
                       colnames(x@listData[[i]]) <- value
                   } else if (is(value, "CharacterList")){
                     if (length(x) != length(value))
                       stop("replacement value must be the same length as x")
                     for (i in seq_len(length(x)))
                       colnames(x@listData[[i]]) <- value[[i]]
                   } else {
                       stop("replacement value must either be NULL or a CharacterList")
                   }
                   x
                 })

setReplaceMethod("colnames", "CompressedSplitDataFrameList",
                 function(x, value)
                 {
                   if (is.null(value)) {
                     colnames(x@unlistData) <- NULL
                   } else if (is.character(value)) {
                     colnames(x@unlistData) <- value
                   } else if (is(value, "CharacterList")){
                     if (length(x) != length(value))
                       stop("replacement value must be the same length as x")
                     if (length(x) > 0)
                       colnames(x@unlistData) <- unlist(value[[1]])
                   } else {
                     stop("replacement value must either be NULL or a CharacterList")
                   }
                   x
                 })

setReplaceMethod("dimnames", "DataFrameList",
                 function(x, value)
                 {
                   if (!is.list(value))
                     stop("replacement value must be a list")
                   rownames(x) <- value[[1]]
                   colnames(x) <- value[[2]]
                   x
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
  if (length(listData) == 1 && is.list(listData[[1]]) &&
      !is.data.frame(listData[[1]]))
    listData <- listData[[1]]
  newSimpleList("SimpleDataFrameList", lapply(listData, as, "DataFrame"))
}

SplitDataFrameList <- function(..., compress = TRUE)
{
  if (!isTRUEorFALSE(compress))
    stop("'compress' must be TRUE or FALSE")
  listData <- list(...)
  if (length(listData) == 1 && is.list(listData[[1]]) &&
      !is.data.frame(listData[[1]]))
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

setReplaceMethod("[", "SimpleSplitDataFrameList",
                 function(x, i, j,..., value)
                 {
                     if (length(list(...)) > 0)
                         stop("invalid replacement")
                     if (missing(j)) {
                         if (missing(i))
                             x <- callNextMethod(x = x, value = value)
                         else
                             x <- callNextMethod(x = x, i = i, value = value)
                     } else {
                         jInfo <-
                           .bracket.Index(j, colnames(x)[[1]], ncol(x)[[1]])
                         if (!jInfo[["useIdx"]]) {
                             if (missing(i))
                                 x[] <- value
                             else
                                 x[i] <- value
                         } else {
                             j <- jInfo[["idx"]]
                             y <- x[, j, drop=FALSE]
                             if (missing(i))
                                 y[] <- value
                             else
                                 y[i] <- value
                             indices <-
                               structure(seq_len(length(x)), names = names(x))
                             x@listData <-
                               lapply(indices, function(k) {
                                          z <- x@listData[[k]]
                                          z[j] <- y[[k]]
                                          z
                                      })
                         }
                     }
                     x
                 })

setReplaceMethod("[", "CompressedSplitDataFrameList",
                 function(x, i, j,..., value)
                 {
                     if (length(list(...)) > 0)
                         stop("invalid replacement")
                     if (missing(j)) {
                         if (missing(i))
                             x <- callNextMethod(x = x, value = value)
                         else
                             x <- callNextMethod(x = x, i = i, value = value)
                     } else {
                         jInfo <-
                           .bracket.Index(j, colnames(x)[[1]], ncol(x)[[1]])
                         if (!jInfo[["useIdx"]]) {
                             if (missing(i))
                                 x[] <- value
                             else
                                 x[i] <- value
                         } else {
                             j <- jInfo[["idx"]]
                             y <- x[, j, drop=FALSE]
                             if (missing(i)) {
                                 y[] <- value
                             } else {
                                 y[i] <- value
                             }
                             xels <- elementLengths(x)
                             yels <- elementLengths(y)
                             if (any(xels != yels)) {
                                 ends <- cumsum(elementLengths(y))
                                 starts <- c(1L, head(ends, -1) + 1L)
                                 indices <-
                                   unlist(lapply(seq_len(length(y)),
                                                 function(k) {
                                                     rep(starts[k]:ends[k],
                                                         length.out = xels[k])
                                                 }))
                                 y@unlistData <-
                                   y@unlistData[indices, , drop = FALSE]
                             }
                             x@unlistData[, j] <- y@unlistData
                         }
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

setAs("ANY", "SimpleSplitDataFrameList",
      function(from) SplitDataFrameList(from, compress=FALSE))
setAs("ANY", "CompressedSplitDataFrameList",
      function(from) SplitDataFrameList(from, compress=TRUE))

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Show
###

setMethod("show", "SplitDataFrameList", function(object)
          {
            nc <- ncol(object)[[1]]
            lo <- length(object)
            cat(class(object), ": ",
                lo, ifelse(lo == 1, " elements with ", " elements with "),
                nc, ifelse(nc == 1, " column\n", " columns\n"), sep = "")
            if (!is.null(names(object)))
              cat(labeledLine("names", names(object)))
            cat(labeledLine("colnames", colnames(object)[[1]]))
          })
