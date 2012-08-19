### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "viewApply", "viewMins", "viewMaxs", and "viewSums" generics and
### methods.
###

setMethod("viewApply", "RleViewsList",
          function(X, FUN, ..., simplify = TRUE)
          newList("SimpleList",
                  lapply(structure(seq_len(length(X)), names = names(X)),
                         function(i) {
                             ans <-
                               aggregate(subject(X[[i]]),
                                         start = structure(start(X[[i]]),
                                            names = names(start(X[[i]]))),
                                         end = end(X[[i]]),
                                         FUN = FUN, ...,
                                         simplify = simplify)
                             if (!simplify) {
                                 ans <- newList("SimpleList", ans,
                                                metadata = metadata(X[[i]]),
                                                mcols = mcols(X[[i]]))
                             }
                             ans
                         }),
                  metadata = metadata(X),
                  mcols = mcols(X)))

.summaryRleViewsList <- function(x, FUN, na.rm = FALSE, outputListType = NULL)
{
    FUN <- match.fun(FUN)
    if (length(x) == 0) {
        outputListType <- "SimpleList"
        listData <- list()
    } else {
        if (is.null(outputListType)) {
            valuesClass <- class(runValue(subject(x[[1L]])))
            if (valuesClass == "integer")
                outputListType <- "SimpleIntegerList"
            else if (valuesClass == "numeric")
                outputListType <- "SimpleNumericList"
            else
                stop("cannot compute numeric summary over a non-numeric Rle")
        }
        listData <-
          lapply(structure(seq_len(length(x)), names = names(x)),
                 function(i) FUN(x[[i]], na.rm = na.rm))
    }
    newList(outputListType, listData, metadata = metadata(x), mcols = mcols(x))
}
setMethod("viewMins", "RleViewsList",
          function(x, na.rm = FALSE)
          .summaryRleViewsList(x, FUN = viewMins, na.rm = na.rm))

setMethod("viewMaxs", "RleViewsList",
          function(x, na.rm = FALSE)
          .summaryRleViewsList(x, FUN = viewMaxs, na.rm = na.rm))

setMethod("viewSums", "RleViewsList",
          function(x, na.rm = FALSE)
          .summaryRleViewsList(x, FUN = viewSums, na.rm = na.rm))

setMethod("viewMeans", "RleViewsList",
          function(x, na.rm = FALSE)
          .summaryRleViewsList(x, FUN = viewMeans, na.rm = na.rm,
                               outputListType = "SimpleNumericList"))

setMethod("viewWhichMins", "RleViewsList",
          function(x, na.rm = FALSE)
          .summaryRleViewsList(x, FUN = viewWhichMins, na.rm = na.rm,
                               outputListType = "SimpleIntegerList"))

setMethod("viewWhichMaxs", "RleViewsList",
          function(x, na.rm = FALSE)
          .summaryRleViewsList(x, FUN = viewWhichMaxs, na.rm = na.rm,
                               outputListType = "SimpleIntegerList"))

setMethod("viewRangeMaxs", "RleViewsList",
          function(x, na.rm = FALSE)
          .summaryRleViewsList(x, FUN = viewRangeMaxs, na.rm = na.rm,
                               outputListType = "SimpleIRangesList"))

setMethod("viewRangeMins", "RleViewsList",
          function(x, na.rm = FALSE)
          .summaryRleViewsList(x, FUN = viewRangeMins, na.rm = na.rm,
                               outputListType = "SimpleIRangesList"))
