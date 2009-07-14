### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "viewApply", "viewMins", "viewMaxs", and "viewSums" generics and
### methods.
###

setMethod("viewApply", "RleViewsList",
          function(X, FUN, ..., simplify = TRUE)
          newSimpleList("SimpleList",
                        lapply(seq_len(length(X)), function(i)
                               aggregate(subject(X[[i]]),
                                         start = start(X[[i]]),
                                         end = end(X[[i]]),
                                         FUN = FUN, ...,
                                         simplify = simplify)),
                        metadata = metadata(X),
                        elementMetadata = elementMetadata(X)))

.summaryRleViewsList <- function(x, FUN, na.rm = FALSE, outputListType = NULL)
{
    FUN <- match.fun(FUN)
    if (length(x) == 0) {
        outputListType <- "SimpleList"
        listData <- list()
    } else {
        if (is.null(outputListType)) {
            valuesClass <- class(runValue(subject(x[[1]])))
            if (valuesClass == "integer")
                outputListType <- "SimpleIntegerList"
            else if (valuesClass == "numeric")
                outputListType <- "SimpleNumericList"
            else
                stop("cannot compute numeric summary over a non-numeric Rle")
        }
        listData <-
          lapply(seq_len(length(x)), function(i) FUN(x[[i]], na.rm = na.rm))
    }
    newSimpleList(outputListType, listData, metadata = metadata(x),
                  elementMetadata = elementMetadata(x))
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
