### =========================================================================
###
### Note that the generics defined in this file implement concepts that make
### sense only for the kind of Views object where the views have a numeric
### content i.e. when subject(x) itself contains values that are or can be
### interpreted as numbers.
###

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "slice" generic and methods.
###

setGeneric("slice", signature="x",
    function(x, lower=-Inf, upper=Inf, ...)
        standardGeneric("slice")
)

setMethod("slice", "integer",
    function(x, lower=-.Machine$integer.max, upper=.Machine$integer.max)
        slice(as(x, "XInteger"), lower=lower, upper=upper)
)

setMethod("slice", "XInteger",
    function(x, lower=-.Machine$integer.max, upper=.Machine$integer.max)
    {
        if (!isSingleNumber(lower))
            stop("'lower' must be a single integer")
        if (!is.integer(lower))
            lower <- as.integer(lower)
        if (!isSingleNumber(upper))
            stop("'upper' must be a single integer")
        if (!is.integer(upper))
            upper <- as.integer(upper)
        .Call("XIntegerViews_slice", x, lower, upper, PACKAGE="IRanges")
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "viewMins", "viewMaxs", "viewSums", "viewWhichMins", and
### "viewWhichMaxs" generics and methods.
###

setGeneric("viewMins", signature="x",
    function(x, na.rm=FALSE) standardGeneric("viewMins")
)

setMethod("viewMins", "XIntegerViews",
    function(x, na.rm=FALSE)
        .Call("XIntegerViews_viewMins", x, na.rm, PACKAGE="IRanges")
)

setGeneric("viewMaxs", signature="x",
    function(x, na.rm=FALSE) standardGeneric("viewMaxs")
)

setMethod("viewMaxs", "XIntegerViews",
    function(x, na.rm=FALSE)
        .Call("XIntegerViews_viewMaxs", x, na.rm, PACKAGE="IRanges")
)

setGeneric("viewSums", signature="x",
    function(x, na.rm=FALSE) standardGeneric("viewSums")
)

setMethod("viewSums", "XIntegerViews",
    function(x, na.rm=FALSE)
        .Call("XIntegerViews_viewSums", x, na.rm, PACKAGE="IRanges")
)

setGeneric("viewWhichMins", signature="x",
    function(x, na.rm=FALSE) standardGeneric("viewWhichMins")
)

setMethod("viewWhichMins", "XIntegerViews",
    function(x, na.rm=FALSE)
        .Call("XIntegerViews_viewWhichMins", x, na.rm, PACKAGE="IRanges")
)

setGeneric("viewWhichMaxs", signature="x",
    function(x, na.rm=FALSE) standardGeneric("viewWhichMaxs")
)

setMethod("viewWhichMaxs", "XIntegerViews",
    function(x, na.rm=FALSE)
        .Call("XIntegerViews_viewWhichMaxs", x, na.rm, PACKAGE="IRanges")
)
