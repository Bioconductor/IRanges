### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "viewApply", "viewMins", "viewMaxs", and "viewSums" generics and
### methods.
###

setMethod("viewApply", "RleViews",
          function(X, FUN, ..., simplify = TRUE) {
              X <- trim(X)
              ans <-
                aggregate(subject(X), start = structure(start(X), names = names(X)),
                          end = end(X), FUN = FUN, ..., simplify = simplify)
              if (!simplify) {
                  ans <- newSimpleList("SimpleList", ans, metadata = metadata(X),
                                       elementMetadata = elementMetadata(X))
              }
              ans
          })

setGeneric("viewMins", signature="x",
           function(x, na.rm = FALSE) standardGeneric("viewMins"))
setMethod("viewMins", "RleViews",
          function(x, na.rm = FALSE)
          .Call("RleViews_viewMins", trim(x), na.rm, PACKAGE="IRanges"))

setGeneric("viewMaxs", signature="x",
           function(x, na.rm = FALSE) standardGeneric("viewMaxs"))
setMethod("viewMaxs", "RleViews",
          function(x, na.rm = FALSE)
          .Call("RleViews_viewMaxs", trim(x), na.rm, PACKAGE="IRanges"))

setGeneric("viewSums", signature="x",
           function(x, na.rm = FALSE) standardGeneric("viewSums"))
setMethod("viewSums", "RleViews",
          function(x, na.rm = FALSE)
          .Call("RleViews_viewSums", trim(x), na.rm, PACKAGE="IRanges"))

setGeneric("viewWhichMins", signature="x",
           function(x, na.rm = FALSE) standardGeneric("viewWhichMins"))
setMethod("viewWhichMins", "RleViews",
          function(x, na.rm = FALSE)
          .Call("RleViews_viewWhichMins", trim(x), na.rm, PACKAGE="IRanges"))

setGeneric("viewWhichMaxs", signature="x",
           function(x, na.rm = FALSE) standardGeneric("viewWhichMaxs"))
setMethod("viewWhichMaxs", "RleViews",
          function(x, na.rm = FALSE)
          .Call("RleViews_viewWhichMaxs", trim(x), na.rm, PACKAGE="IRanges"))

setGeneric("viewRangeMaxs",
           function(x, na.rm = FALSE) standardGeneric("viewRangeMaxs"))
setMethod("viewRangeMaxs", "RleViews",
          function(x, na.rm = FALSE) {
              maxs <- viewWhichMaxs(trim(x), na.rm = na.rm)
              if (any(is.na(maxs)))
                  stop("missing values present, set 'na.rm = TRUE'")
              findRange(maxs, subject(x))
          })

setGeneric("viewRangeMins",
           function(x, na.rm = FALSE) standardGeneric("viewRangeMins"))
setMethod("viewRangeMins", "RleViews",
          function(x, na.rm = FALSE) {
              mins <- viewWhichMins(trim(x), na.rm = na.rm)
              if (any(is.na(mins)))
                  stop("missing values present, set 'na.rm = TRUE'")
              findRange(mins, subject(x))
          })
