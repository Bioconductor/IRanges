### =========================================================================
###
### Note that the generics defined in this file implement concepts that make
### sense only for the kind of Views object where the views have a numeric
### content i.e. when subject(x) itself contains values that are or can be
### interpreted as numbers.
###

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "slice" method.
###

setMethod("slice", "XRleInteger",
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
        .Call("XRleIntegerViews_slice", x, lower, upper, PACKAGE="IRanges")
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "viewMins", "viewMaxs", "viewSums", "viewWhichMins", and
### "viewWhichMaxs" methods.
###

setMethod("viewMins", "XRleIntegerViews",
    function(x, na.rm=FALSE)
        .Call("XRleIntegerViews_viewMins", x, na.rm, PACKAGE="IRanges")
)

setMethod("viewMaxs", "XRleIntegerViews",
    function(x, na.rm=FALSE)
        .Call("XRleIntegerViews_viewMaxs", x, na.rm, PACKAGE="IRanges")
)

setMethod("viewSums", "XRleIntegerViews",
    function(x, na.rm=FALSE)
        .Call("XRleIntegerViews_viewSums", x, na.rm, PACKAGE="IRanges")
)

setMethod("viewWhichMins", "XRleIntegerViews",
    function(x, na.rm=FALSE)
       .Call("XRleIntegerViews_viewWhichMins", x, na.rm, PACKAGE="IRanges")
)

setMethod("viewWhichMaxs", "XRleIntegerViews",
    function(x, na.rm=FALSE)
       .Call("XRleIntegerViews_viewWhichMaxs", x, na.rm, PACKAGE="IRanges")
)
