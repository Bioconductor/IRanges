### =========================================================================
### DataFrameList objects
### -------------------------------------------------------------------------

setClass("DataFrameList", representation("VIRTUAL"),
         prototype = prototype(elementType = "DataFrame"),
         contains = "List")
setClass("SimpleDataFrameList",
         contains = c("DataFrameList", "SimpleList"))
setClass("CompressedDataFrameList",
         prototype = prototype(unlistData = new("DataFrame")),
         contains = c("DataFrameList", "CompressedList"))

setClass("SplitDataFrameList", representation("VIRTUAL"),
         contains = "DataFrameList")
setClass("SimpleSplitDataFrameList",
         contains = c("SplitDataFrameList", "SimpleDataFrameList"))
setClass("CompressedSplitDataFrameList",
         contains = c("SplitDataFrameList", "CompressedDataFrameList"))

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Accessor methods.
###

setMethod("nrow", "DataFrameList",
          function(x)
          {
            if (length(x) == 0L)
              0L
            else
              elementNROWS(x)
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

setMethod("colnames", "SplitDataFrameList",
          function(x, do.NULL = TRUE, prefix = "col")
          {
            if (length(x)) {
              nms <- colnames(x[[1]], do.NULL = do.NULL, prefix = prefix)
              rep(CharacterList(nms), length(x))
            } else NULL
          })

setMethod("colnames", "CompressedSplitDataFrameList",
          function(x, do.NULL = TRUE, prefix = "col")
          {
            if (length(x)) {
              nms <- colnames(x@unlistData, do.NULL = do.NULL, prefix = prefix)
              rep(CharacterList(nms), length(x))
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
                   if (is.null(value) || is(value, "CharacterList")) {
                     if (is.null(value))
                       value <- list(NULL)
                     else if (length(x) != length(value))
                       stop("replacement value must be the same length as x")
                     x@listData <-
                       mapply(function(y, rn) {rownames(y) <- rn; y},
                              x@listData, value, SIMPLIFY=FALSE)
                   } else {
                     stop("replacement value must be NULL or a CharacterList")
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
                   x
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
  S4Vectors:::new_SimpleList_from_list("SimpleDataFrameList", listData)
}

SplitDataFrameList <- function(..., compress = TRUE, cbindArgs = FALSE)
{
  if (!isTRUEorFALSE(compress))
    stop("'compress' must be TRUE or FALSE")
  listData <- list(...)
  if (length(listData) == 1 &&
      (is.list(listData[[1L]]) || is(listData[[1L]], "List")) &&
      !(is.data.frame(listData[[1L]]) || is(listData[[1L]], "DataFrame")))
    listData <- listData[[1L]]
  if (cbindArgs) {
    if (is.null(names(listData)))
      names(listData) <- paste("X", seq_len(length(listData)), sep = "")
    listData <- do.call(Map, c(list(DataFrame), listData))
  }

  as(listData,
     if (compress) "CompressedSplitDataFrameList"
     else "SimpleSplitDataFrameList")
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Subsetting.
###

setMethod("[", "SimpleSplitDataFrameList",
          function(x, i, j, ..., drop=TRUE)
          {
            if (!missing(j))
              x@listData <- lapply(x@listData, function(y) y[,j,drop=FALSE])
            if (!missing(i))
              x <- callNextMethod(x, i)

            if (((nargs() - !missing(drop)) > 2) &&
                (length(x@listData) > 0) && (ncol(x@listData[[1L]]) == 1) &&
                (missing(drop) || drop)) {
              x <- as(lapply(x@listData, "[[", 1), "List")
            }

            x
          })

setMethod("[", "CompressedSplitDataFrameList",
          function(x, i, j, ..., drop=TRUE)
          {
            if (!missing(j))
              x@unlistData <- x@unlistData[, j, drop=FALSE]
            if (!missing(i))
              x <- callNextMethod(x, i)

            if (((nargs() - !missing(drop)) > 2) &&
                (ncol(x@unlistData) == 1) && (missing(drop) || drop)) {
              x <- relist(x@unlistData[[1L]], x)
            }

            x
          })

setMethod("normalizeSingleBracketReplacementValue", "SplitDataFrameList",
    function(value, x)
    {
        value <- callNextMethod()  # call default method
        rownames(value) <- NULL
        if (length(x) != 0L && ncol(x)[[1L]] == ncol(value)[[1L]])
            colnames(value)[[1L]] <- colnames(x)[[1L]]
        value
    }
)

setReplaceMethod("[", "SplitDataFrameList",
    function(x, i, j,..., value)
    {
        if (length(list(...)) > 0L)
            stop("invalid replacement")
        value <- normalizeSingleBracketReplacementValue(value, x)
        if (missing(j)) {
            if (missing(i))
                ans <- callNextMethod(x=x, value=value)
            else
                ans <- callNextMethod(x=x, i=i, value=value)
            return(ans)
        }
        colind <- setNames(seq_along(commonColnames(x)), commonColnames(x))
        if (missing(i) && is.character(j)) {
            colnames(value) <- j
        }
        j <- normalizeSingleBracketSubscript(j, colind, allow.append=missing(i))
        if (missing(i)) {
            y <- value
        } else {
            y <- x[, j, drop=FALSE]
            if (is.list(i) || (is(i, "List") && !is(i, "Ranges"))) {
                y <- S4Vectors:::lsubset_List_by_List(y, i, value)
            } else {
                y[i] <- value
            }
        }
        if (length(y) < length(x)) {
            y <- rep(y, length.out=length(x))
        }
        if (is(x, "CompressedList")) {
            x_eltNROWS <- elementNROWS(x)
            y_eltNROWS <- elementNROWS(y)
            if (any(x_eltNROWS != y_eltNROWS)) {
                indices <- IRanges(start(y@partitioning), width=y_eltNROWS)
                indices <- rep(indices, x_eltNROWS / y_eltNROWS)
                if (sum(width(indices)) != sum(x_eltNROWS)) {
                    stop("some element lengths of 'x' are not multiples of the ",
                         "corresponding element lengths of 'value'")
                }
                y@unlistData <- y@unlistData[indices, , drop=FALSE]
            }
            x@unlistData[, j] <- y@unlistData
        } else if (is(x, "SimpleList")) {
            indices <- structure(seq_len(length(x)), names = names(x))
            x@listData <- lapply(indices,
                                 function(k) {
                                     z <- x@listData[[k]]
                                     z[j] <- y[[k]]
                                     z
                                 })
        } else {
            stop(class(x), " objects not supported")
        }
        x
    }
)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion
###

## Casting DataFrameList -> DataFrame implies cast to SplitDataFrameList
setAs("DataFrameList", "DataFrame", function(from) {
  as(as(from, "SplitDataFrameList"), "DataFrame")
})

setGeneric("commonColnames", function(x) standardGeneric("commonColnames"))

setMethod("commonColnames", "CompressedSplitDataFrameList",
          function(x) colnames(unlist(x, use.names=FALSE)))

setMethod("commonColnames", "SplitDataFrameList",
          function(x) colnames(head(x, 1L)))

setAs("SplitDataFrameList", "DataFrame",
    function(from) {
      cols <- sapply(commonColnames(from), function(j) from[,j],
                     simplify=FALSE)
      DataFrame(cols, check.names=FALSE)
    }
)

setMethod("as.data.frame", "DataFrameList",
          function(x, row.names = NULL, optional = FALSE, ...)
{
    as.data.frame(as(x, "DataFrame"), row.names=row.names, optional=optional,
                  ...)
})

setAs("ANY", "SplitDataFrameList",
      function(from) as(from, "CompressedSplitDataFrameList"))

setAs("list", "SplitDataFrameList",
      function(from) as(from, "SimpleSplitDataFrameList"))

setAs("SimpleList", "SplitDataFrameList",
      function(from) as(from, "SimpleSplitDataFrameList"))

setAs("DataFrame", "SplitDataFrameList",
      function(from) as(from, "CompressedSplitDataFrameList"))

setAs("ANY", "SimpleSplitDataFrameList",
      function(from) {
        new("SimpleSplitDataFrameList", as(from, "SimpleDataFrameList"))
      })
setAs("ANY", "CompressedSplitDataFrameList",
      function(from) {
        coerceToCompressedList(from, "DataFrame")
      })

setListCoercions("DataFrame")

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Show
###

setMethod("show", "SplitDataFrameList", function(object)
          {
            k <- length(object)
            cumsumN <- cumsum(elementNROWS(object))
            N <- tail(cumsumN, 1)
            cat(classNameForDisplay(object), " of length ", k, "\n",
                sep = "")
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
