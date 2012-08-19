### =========================================================================
### DataFrameList objects
### -------------------------------------------------------------------------

setClass("DataFrameList", representation("VIRTUAL"),
         prototype = prototype(elementType = "DataFrame"),
         contains = "List")
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

setMethod("ncol", "SimpleSplitDataFrameList",
          function(x)
          {
            if (length(x) == 0L)
              0L
            else
              structure(rep.int(ncol(x[[1L]]), length(x)),
                        names = names(x))
          })

setMethod("ncol", "CompressedSplitDataFrameList",
          function(x)
          {
            if (length(x) == 0L)
              0L
            else
              structure(rep.int(ncol(x@unlistData), length(x)),
                        names = names(x))
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

setMethod("colnames", "SimpleSplitDataFrameList",
          function(x, do.NULL = TRUE, prefix = "col")
          {
            if (length(x)) {
              nms <- colnames(x[[1]], do.NULL = do.NULL, prefix = prefix)
              CharacterList(rep(list(nms), length(x)))
            } else NULL
          })

setMethod("colnames", "CompressedSplitDataFrameList",
          function(x, do.NULL = TRUE, prefix = "col")
          {
            if (length(x)) {
              nms <- colnames(x@unlistData, do.NULL = do.NULL, prefix = prefix)
              CharacterList(rep(list(nms), length(x)))
            } else NULL
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
                       lapply(x@listData, function(y) {colnames(y) <- NULL; y})
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
                       colnames(x@unlistData) <- unlist(value[[1L]])
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
                   rownames(x) <- value[[1L]]
                   colnames(x) <- value[[2L]]
                   x
                 })

setGeneric("columnMetadata", function(x, ...) standardGeneric("columnMetadata"))

setMethod("columnMetadata", "SimpleSplitDataFrameList", function(x) {
  if (length(x))
    mcols(x[[1]])
  else NULL
})

setMethod("columnMetadata", "CompressedSplitDataFrameList", function(x) {
  mcols(x@unlistData)
})

setGeneric("columnMetadata<-",
           function(x, ..., value) standardGeneric("columnMetadata<-"))

setReplaceMethod("columnMetadata", "SimpleSplitDataFrameList",
                 function(x, value) {
                   x@listData <- lapply(x@listData, function(xi) {
                     mcols(xi) <- value
                     xi
                   })
                   x
                 })

setReplaceMethod("columnMetadata", "CompressedSplitDataFrameList",
                 function(x, value) {
                   mcols(x@unlistData) <- value
                 })

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Validity.
###

.valid.SplitDataFrameList <- function(x) {
  if (length(x) && !is(x, "CompressedList")) {
    firstNames <- colnames(x[[1L]])
    l <- as.list(x, use.names = FALSE)
    if (!all(sapply(l, function(df) identical(firstNames, colnames(df)))))
      return("column counts or names differ across elements")
    firstMetaData <- mcols(x[[1L]]) # could be NULL
    if (!all(sapply(l, function(df) {
      identical(firstMetaData, mcols(df))
    })))
      return("metadata columns must be identical across elements")
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
  if (length(listData) == 1 && is.list(listData[[1L]]) &&
      !is.data.frame(listData[[1L]]))
    listData <- listData[[1L]]
  if (length(listData) > 0 && !is(listData[[1L]], "DataFrame"))
    listData <- lapply(listData, as, "DataFrame")
  newList("SimpleDataFrameList", listData)
}

SplitDataFrameList <- function(..., compress = TRUE, cbindArgs = FALSE)
{
  if (!isTRUEorFALSE(compress))
    stop("'compress' must be TRUE or FALSE")
  listData <- list(...)
  if (length(listData) == 1 && is.list(listData[[1L]]) &&
      !is.data.frame(listData[[1L]]))
    listData <- listData[[1L]]
  if (cbindArgs) {
    if (is.null(names(listData)))
      names(listData) <- paste("X", seq_len(length(listData)), sep = "")
    listData <- do.call(Map, c(list(DataFrame), listData))
  } else if (any(!sapply(listData, is, "DataFrame")))
    listData <- lapply(listData, as, "DataFrame")
  
  if (compress)
    newList("CompressedSplitDataFrameList", listData)
  else
    newList("SimpleSplitDataFrameList", listData)
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
                (length(x@listData) > 0) && (ncol(x@listData[[1L]]) == 1) &&
                (missing(drop) || drop)) {
              uniqueClasses <-
                unique(unlist(lapply(x@listData, function(y) class(y[[1L]]))))
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
              dataClass <- class(x@unlistData[[1L]])
              if (dataClass %in% 
                  c("raw", "logical", "integer", "numeric", "character",
                    "complex", "Rle"))
                x <-
                  CompressedAtomicList(x@unlistData[[1L]],
                                       partitioning = x@partitioning)
              else if (dataClass == "IRanges")
                x <-
                  new2("CompressedIRangesList", unlistData = x@unlistData[[1L]],
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
                           .bracket.Index(j, ncol(x)[[1L]], colnames(x)[[1L]])
                         if (!is.null(jInfo[["msg"]]))
                           stop("subsetting by column: ", jInfo[["msg"]])
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
                             } else if (is.list(i) || is(i, "List")) {
                                 y <- subsetListByList_replace(y, i, value,
                                                               byrow=TRUE)
                             } else {
                                 y[i] <- value
                             }
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
                           .bracket.Index(j, ncol(x)[[1L]], colnames(x)[[1L]])
                         if (!is.null(jInfo[["msg"]]))
                           stop("subsetting by column: ", jInfo[["msg"]])
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
                             } else if (is.list(i) || is(i, "List")) {
                                 y <- subsetListByList_replace(y, i, value,
                                                               byrow=TRUE)
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

## Casting DataFrameList -> DataFrame implies cast to SplitDataFrameList
setAs("DataFrameList", "DataFrame", function(from) {
  v <- .valid.SplitDataFrameList(from)
  if (!is.null(v))
    stop(v)
  unlist(from)
})

setAs("SplitDataFrameList", "DataFrame", function(from) unlist(from))

setMethod("as.data.frame", "DataFrameList",
          function(x, row.names=NULL, optional=FALSE, ...)
          {
            if (!(is.null(row.names) || is.character(row.names)))
              stop("'row.names' must be NULL or a character vector")
            if (!missing(optional) || length(list(...)))
              warning("'optional' and arguments in '...' ignored")
            stacked <- stack(x)
            if (is.null(row.names))
              row.names <- rownames(stacked)
            data.frame(as.data.frame(stacked, row.names = row.names),
                       stringsAsFactors = FALSE)
          })

setAs("ANY", "SimpleSplitDataFrameList",
      function(from) SplitDataFrameList(from, compress=FALSE))
setAs("ANY", "CompressedSplitDataFrameList",
      function(from) SplitDataFrameList(from, compress=TRUE))

## Behaves like as.list() on a vector, while SplitDataFrameList() is like list()
setAs("List", "SimpleSplitDataFrameList",
      function(from) do.call(SplitDataFrameList,
                             c(as.list(from), compress=FALSE)))
setAs("List", "CompressedSplitDataFrameList",
      function(from) do.call(SplitDataFrameList,
                             c(as.list(from), compress=TRUE)))

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Show
###

setMethod("show", "SplitDataFrameList", function(object)
          {
            k <- length(object)
            cumsumN <- cumsum(elementLengths(object))
            N <- tail(cumsumN, 1)
            cat(class(object), " of length ", k, "\n", sep = "")
            if (k == 0L) {
              cat("<0 elements>\n")
            } else if ((k == 1L) || (N <= 20L)) {
              show(as.list(object))
            } else {
              sketch <- function(x) c(head(x, 3), "...", tail(x, 3))
              if (k >= 3 && cumsumN[3L] <= 20)
                showK <- 3
              else if (k >= 2 && cumsumN[2L] <= 20)
                showK <- 2
              else
                showK <- 1
              diffK <- k - showK
              show(as.list(head(object, showK)))
              if (diffK > 0)
                cat("...\n<", k - showK,
                    ifelse(diffK == 1,
                           " more element>\n", " more elements>\n"),
                    sep="")
            }
          })
