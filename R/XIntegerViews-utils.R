setMethod("viewMins", signature = c(x = "XIntegerViews"),
    function(x, na.rm = FALSE)
    .Call("XIntegerViews_viewMins", x, na.rm, PACKAGE="IRanges")
)

setMethod("viewMaxs", signature = c(x = "XIntegerViews"),
    function(x, na.rm = FALSE)
    .Call("XIntegerViews_viewMaxs", x, na.rm, PACKAGE="IRanges")
)

setMethod("viewSums", signature = c(x = "XIntegerViews"),
    function(x, na.rm = FALSE)
    .Call("XIntegerViews_viewSums", x, na.rm, PACKAGE="IRanges")
)
