### =========================================================================
### CompressedDataFrameList objects
### -------------------------------------------------------------------------

setClass("CompressedDataFrameList",
         prototype = prototype(unlistData = new("DataFrame")),
         contains = c("DataFrameList", "CompressedList"))

setClass("CompressedSplitDataFrameList",
         contains = c("SplitDataFrameList", "CompressedDataFrameList"))

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Accessor methods.
###

setMethod("ncol", "CompressedSplitDataFrameList",
          function(x)
          {
            if (length(x) == 0L)
              0L
            else
              structure(rep.int(ncol(x@unlistData), length(x)),
                        names = names(x))
          })

setMethod("colnames", "CompressedSplitDataFrameList",
          function(x, do.NULL = TRUE, prefix = "col")
          {
            if (length(x)) {
              nms <- colnames(x@unlistData, do.NULL = do.NULL, prefix = prefix)
              rep(CharacterList(nms), length(x))
            } else NULL
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

setMethod("columnMetadata", "CompressedSplitDataFrameList", function(x) {
  mcols(x@unlistData, use.names=FALSE)
})

setReplaceMethod("columnMetadata", "CompressedSplitDataFrameList",
                 function(x, value) {
                   mcols(x@unlistData) <- value
                   x
                 })

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Subsetting.
###

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

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion
###

setListCoercions("DataFrame")

setMethod("commonColnames", "CompressedSplitDataFrameList",
          function(x) colnames(unlist(x, use.names=FALSE)))

setAs("ANY", "CompressedSplitDataFrameList",
      function(from) {
        coerceToCompressedList(from, "DataFrame")
      })

setAs("ANY", "SplitDataFrameList",
      function(from) as(from, "CompressedSplitDataFrameList"))

setAs("DataFrame", "SplitDataFrameList",
      function(from) as(from, "CompressedSplitDataFrameList"))

