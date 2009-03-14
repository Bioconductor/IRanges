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
            findRun(maxs, subject(x))
          })

setGeneric("viewRangeMins", function(x, ...) standardGeneric("viewRangeMins"))
setMethod("viewRangeMins", "RleViews",
          function(x) {
            mins <- viewWhichMins(x, TRUE)
            findRun(mins, subject(x))
          })

## can this become a general utility?
findRun <- function(x, rle) {
  starts <- start(rle)
  runs <- match(x, starts) # or findInterval to be more general
  IRanges(starts[runs], width=width(rle)[runs])
}
