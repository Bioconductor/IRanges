### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "viewApply", "viewMins", "viewMaxs", and "viewSums" methods.
###

setMethod("viewApply", "RleViews",
          function(X, FUN, ..., simplify = TRUE)
          aggregate(subject(X), start = start(X), end = end(X), FUN = FUN, ...,
                    simplify = simplify))

setMethod("viewMins", "RleViews",
          function(x, na.rm = FALSE)
          .Call("RleViews_viewMins", x, na.rm, PACKAGE="IRanges"))

setMethod("viewMaxs", "RleViews",
          function(x, na.rm = FALSE)
          .Call("RleViews_viewMaxs", x, na.rm, PACKAGE="IRanges"))

setMethod("viewSums", "RleViews",
          function(x, na.rm = FALSE)
          .Call("RleViews_viewSums", x, na.rm, PACKAGE="IRanges"))

setMethod("viewWhichMins", "RleViews",
          function(x, na.rm=FALSE)
          .Call("RleViews_viewWhichMins", x, na.rm, PACKAGE="IRanges"))

setMethod("viewWhichMaxs", "RleViews",
          function(x, na.rm=FALSE)
          .Call("RleViews_viewWhichMaxs", x, na.rm, PACKAGE="IRanges"))

setGeneric("viewRangeMaxs", function(x, ...) standardGeneric("viewRangeMaxs"))
setMethod("viewRangeMaxs", "RleViews",
          function(x) {
            maxs <- viewWhichMaxs(x, TRUE)
            findRange(maxs, subject(x))
          })

setGeneric("viewRangeMins", function(x, ...) standardGeneric("viewRangeMins"))
setMethod("viewRangeMins", "RleViews",
          function(x) {
            mins <- viewWhichMins(x, TRUE)
            findRange(mins, subject(x))
          })
