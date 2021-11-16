### =========================================================================
### CompressedDataFrameList objects
### -------------------------------------------------------------------------


setClass("CompressedDataFrameList",
    contains=c("DataFrameList", "CompressedList"),
    representation("VIRTUAL", unlistData="DataFrame"),
    prototype(unlistData=new("DFrame"))
)

setClass("CompressedDFrameList",
    contains=c("DFrameList", "CompressedDataFrameList"),
    representation(unlistData="DFrame")
)

setClass("CompressedSplitDataFrameList",
    contains=c("SplitDataFrameList", "CompressedDataFrameList"),
    representation("VIRTUAL")
)

setClass("CompressedSplitDFrameList",
    contains=c("SplitDFrameList", "CompressedDFrameList",
               "CompressedSplitDataFrameList")
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Accessor methods.
###

### Deprecated.
### IMPORTANT NOTE: We won't be able to go thru the Defunct cycle because
### a lot of code around assumes that ncol() can be called on an arbitrary
### object!
setMethod("ncol", "CompressedSplitDataFrameList",
          function(x)
          {
            msg <- c("The ncol() method for CompressedSplitDataFrameList ",
                     "objects is deprecated. Please use ncols() on these ",
                     "objects instead.")
            .Deprecated(msg=wmsg(msg))
            if (length(x) == 0L)
              0L
            else
              structure(rep.int(ncol(x@unlistData), length(x)),
                        names = names(x))
          })

setMethod("ncols", "CompressedSplitDataFrameList",
    function(x, use.names=TRUE)
    {
        if (!isTRUEorFALSE(use.names))
            stop(wmsg("'use.names' must be TRUE or FALSE"))
        ans_names <- if (use.names) names(x) else NULL
        structure(rep.int(ncol(x@unlistData), length(x)), names=ans_names)
    }
)

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

setMethod("commonColnames", "CompressedSplitDataFrameList",
          function(x) colnames(unlist(x, use.names=FALSE)))

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

setAs("ANY", "CompressedDataFrameList",
    function(from) as(from, "CompressedDFrameList")
)
setAs("ANY", "CompressedSplitDataFrameList",
    function(from) as(from, "CompressedSplitDFrameList")
)

setListCoercions("DFrame")

setAs("ANY", "CompressedSplitDFrameList",
      function(from) {
        coerceToCompressedList(from, "DFrame")
      })

setAs("ANY", "SplitDFrameList",
      function(from) as(from, "CompressedSplitDFrameList"))

setAs("DataFrame", "SplitDFrameList",
      function(from) as(from, "CompressedSplitDFrameList"))

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Display
###

setMethod("classNameForDisplay", "CompressedDFrameList",
    function(x) sub("^Compressed", "", sub("DFrame", "DataFrame", class(x)))
)

