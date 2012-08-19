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
                  ans <- newList("SimpleList", ans, metadata=metadata(X),
                                 mcols=mcols(X))
              }
              ans
          })

setMethod("viewMins", "RleViews",
          function(x, na.rm = FALSE)
          .Call2("RleViews_viewMins", trim(x), na.rm, PACKAGE="IRanges"))

setMethod("viewMaxs", "RleViews",
          function(x, na.rm = FALSE)
          .Call2("RleViews_viewMaxs", trim(x), na.rm, PACKAGE="IRanges"))

setMethod("viewSums", "RleViews",
          function(x, na.rm = FALSE)
          .Call2("RleViews_viewSums", trim(x), na.rm, PACKAGE="IRanges"))

setMethod("viewMeans", "RleViews",
          function(x, na.rm = FALSE)
          .Call2("RleViews_viewMeans", trim(x), na.rm, PACKAGE="IRanges"))

setMethod("viewWhichMins", "RleViews",
          function(x, na.rm = FALSE)
          .Call2("RleViews_viewWhichMins", trim(x), na.rm, PACKAGE="IRanges"))

setMethod("viewWhichMaxs", "RleViews",
          function(x, na.rm = FALSE)
          .Call2("RleViews_viewWhichMaxs", trim(x), na.rm, PACKAGE="IRanges"))

setMethod("viewRangeMaxs", "RleViews",
          function(x, na.rm = FALSE) {
              maxs <- viewWhichMaxs(trim(x), na.rm = na.rm)
              if (anyMissing(maxs))
                  stop("missing values present, set 'na.rm = TRUE'")
              findRange(maxs, subject(x))
          })

setMethod("viewRangeMins", "RleViews",
          function(x, na.rm = FALSE) {
              mins <- viewWhichMins(trim(x), na.rm = na.rm)
              if (anyMissing(mins))
                  stop("missing values present, set 'na.rm = TRUE'")
              findRange(mins, subject(x))
          })
