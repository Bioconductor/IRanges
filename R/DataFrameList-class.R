### =========================================================================
### DataFrameList objects
### -------------------------------------------------------------------------


setClass("DataFrameList",
    contains="List",
    representation("VIRTUAL"),
    prototype(elementType="DataFrame")
)

setClass("DFrameList",
    contains="DataFrameList",
    representation("VIRTUAL"),
    prototype(elementType="DFrame")
)

setClass("SimpleDataFrameList",
    contains=c("DataFrameList", "SimpleList")
)

setClass("SimpleDFrameList",
    contains=c("DFrameList", "SimpleDataFrameList")
)

setClass("SplitDataFrameList",
    contains="DataFrameList",
    representation("VIRTUAL")
)

setClass("SplitDFrameList",
    contains=c("DFrameList", "SplitDataFrameList"),
    representation("VIRTUAL")
)

setClass("SimpleSplitDataFrameList",
    contains=c("SplitDataFrameList", "SimpleDataFrameList")
)

setClass("SimpleSplitDFrameList",
    contains=c("SplitDFrameList", "SimpleDFrameList",
               "SimpleSplitDataFrameList")
)


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

setReplaceMethod("dimnames", "DataFrameList",
                 function(x, value)
                 {
                   if (!is.list(value))
                     stop("replacement value must be a list")
                   rownames(x) <- value[[1L]]
                   colnames(x) <- value[[2L]]
                   x
                 })

### NROW(x) and ROWNAMES(x) need to retun length(x) and names(x),
### respectively, on a DataFrameList object, but the default methods
### return dim(x)[1L] and rownames(x), which is not what we want.
### So we need to override them.
setMethod("NROW", "DataFrameList", function(x) length(x))
setMethod("ROWNAMES", "DataFrameList", function(x) names(x))

setGeneric("commonColnames", function(x) standardGeneric("commonColnames"))

setMethod("commonColnames", "SimpleSplitDataFrameList",
          function(x) {
            if (length(x)) 
              colnames(x[[1]])
            else NULL
          })

setGeneric("commonColnames<-", function(x, value) standardGeneric("commonColnames<-"))

setReplaceMethod("commonColnames", "SplitDataFrameList", 
                 function(x, value) {
                   colnames(x) <- value
                   x
                 })

setGeneric("columnMetadata", function(x, ...) standardGeneric("columnMetadata"))

setMethod("columnMetadata", "SimpleSplitDataFrameList", function(x) {
  if (length(x))
    mcols(x[[1]], use.names=FALSE)
  else NULL
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

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Validity.
###

.valid.SimpleSplitDataFrameList <- function(x) {
  if (length(x)) {
    firstNames <- colnames(x[[1L]])
    l <- as.list(x, use.names = FALSE)
    if (!all(sapply(l, function(df) identical(firstNames, colnames(df)))))
      return("column counts or names differ across elements")
    firstMetaData <- mcols(x[[1L]], use.names=FALSE) # could be NULL
    if (!all(sapply(l, function(df) {
      identical(firstMetaData, mcols(df, use.names=FALSE))
    })))
      return("metadata columns must be identical across elements")
  }
  NULL
}

setValidity2("SimpleSplitDataFrameList", .valid.SimpleSplitDataFrameList)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Constructor.
###

DataFrameList <- function(...)
{
  listData <- list(...)
  if (length(listData) == 1 && is.list(listData[[1L]]) &&
      !is.data.frame(listData[[1L]]))
    listData <- listData[[1L]]
  if (length(listData) > 0 && !is(listData[[1L]], "DFrame"))
    listData <- lapply(listData, as, "DFrame")
  S4Vectors:::new_SimpleList_from_list("SimpleDFrameList", listData)
}

SplitDataFrameList <- function(..., compress = TRUE, cbindArgs = FALSE)
{
  if (!isTRUEorFALSE(compress))
    stop("'compress' must be TRUE or FALSE")
  listData <- list(...)
  if (length(listData) == 1 &&
      (is.list(listData[[1L]]) || is(listData[[1L]], "List")) &&
      !(is.data.frame(listData[[1L]]) || is(listData[[1L]], "DFrame")))
    listData <- listData[[1L]]
  if (cbindArgs) {
    if (is.null(names(listData)))
      names(listData) <- paste("X", seq_len(length(listData)), sep = "")
    listData <- do.call(Map, c(list(DataFrame), listData))
  }

  as(listData,
     if (compress) "CompressedSplitDFrameList"
     else "SimpleSplitDFrameList")
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

setMethod("normalizeSingleBracketReplacementValue", "SplitDataFrameList",
    function(value, x)
    {
        value <- callNextMethod()  # call default method
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
            if (is.list(i) || (is(i, "List") && !is(i, "IntegerRanges"))) {
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

setAs("ANY", "DataFrameList",
    function(from) as(from, "DFrameList")
)
setAs("ANY", "SimpleDataFrameList",
    function(from) as(from, "SimpleDFrameList")
)
setAs("ANY", "SplitDataFrameList",
    function(from) as(from, "SplitDFrameList")
)
setAs("ANY", "SimpleSplitDataFrameList",
    function(from) as(from, "SimpleSplitDFrameList")
)

## Casting DataFrameList -> DFrame implies cast to SplitDataFrameList
setAs("DataFrameList", "DFrame", function(from) {
  as(as(from, "SplitDFrameList"), "DFrame")
})

setAs("SplitDataFrameList", "DFrame",
    function(from) {
      cols <- sapply(commonColnames(from), function(j) from[,j],
                     simplify=FALSE)
      DataFrame(cols, check.names=FALSE)
    }
)

setAs("list", "SplitDFrameList",
      function(from) as(from, "SimpleSplitDFrameList"))

setAs("SimpleList", "SplitDFrameList",
      function(from) as(from, "SimpleSplitDFrameList"))

setAs("ANY", "SimpleSplitDFrameList",
      function(from) {
        new("SimpleSplitDFrameList", as(from, "SimpleDFrameList"))
      })

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Display
###

setMethod("classNameForDisplay", "SimpleDFrameList",
    function(x) sub("^Simple", "", sub("DFrame", "DataFrame", class(x)))
)

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
