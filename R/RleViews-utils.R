### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "viewMins", "viewMaxs", and "viewSums" methods.
###

setMethod("viewMins", "RleViews",
          function(x, na.rm=FALSE)
          .Call("RleViews_viewMins", x, na.rm, PACKAGE="IRanges"))

setMethod("viewMaxs", "RleViews",
          function(x, na.rm=FALSE)
          .Call("RleViews_viewMaxs", x, na.rm, PACKAGE="IRanges"))

setMethod("viewSums", "RleViews",
          function(x, na.rm=FALSE)
          .Call("RleViews_viewSums", x, na.rm, PACKAGE="IRanges"))
