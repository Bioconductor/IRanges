### =========================================================================
###
### Note that the generics defined in this file implement concepts that make
### sense only for the kind of Views object where the views have a numeric
### content i.e. when subject(x) itself contains values that are or can be
### interpreted as numbers.
###

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "slice" methods.
###

setMethod("slice", "numeric",
    function(x, lower=-Inf, upper=Inf,
             includeLower=TRUE, includeUpper=TRUE, rangesOnly=FALSE)
        slice(as(x, "XDouble"), lower=lower, upper=upper,
              includeLower=includeLower, includeUpper=includeUpper,
              rangesOnly=rangesOnly)
)

setMethod("slice", "XDouble",
    function(x, lower=-.Machine$double.xmax, upper=.Machine$double.xmax,
             includeLower=TRUE, includeUpper=TRUE, rangesOnly=FALSE)
    {
        if (!isSingleNumber(lower))
            stop("'lower' must be a single integer")
        if (!is.numeric(lower))
            lower <- as.numeric(lower)
        if (!isSingleNumber(upper))
            stop("'upper' must be a single integer")
        if (!is.numeric(upper))
            upper <- as.numeric(upper)
        if (!isTRUEorFALSE(includeLower))
            stop("'includeLower' must be TRUE or FALSE")
        if (!isTRUEorFALSE(includeUpper))
            stop("'includeUpper' must be TRUE or FALSE")
        if (!isTRUEorFALSE(rangesOnly))
            stop("'rangesOnly' must be TRUE or FALSE")
        
        ranges <- .Call2("XDouble_slice", x, lower, upper, includeLower,
                        includeUpper, PACKAGE="IRanges")
        if (rangesOnly) {
            ranges
        } else {
            Views(x, ranges)
        }
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "viewMins", "viewMaxs", "viewSums", "viewWhichMins", and
### "viewWhichMaxs" methods.
###

setMethod("viewMins", "XDoubleViews",
    function(x, na.rm=FALSE)
        .Call2("XDoubleViews_summary1", x, na.rm, "viewMins",
              PACKAGE="IRanges")
)

setMethod("viewMaxs", "XDoubleViews",
    function(x, na.rm=FALSE)
        .Call2("XDoubleViews_summary1", x, na.rm, "viewMaxs",
              PACKAGE="IRanges")
)

setMethod("viewSums", "XDoubleViews",
    function(x, na.rm=FALSE)
        .Call2("XDoubleViews_summary1", x, na.rm, "viewSums",
              PACKAGE="IRanges")
)

setMethod("viewMeans", "XDoubleViews",
    function(x, na.rm=FALSE) {
        if (!isTRUEorFALSE(na.rm))
            stop("'na.rm' must be TRUE or FALSE")
        if (na.rm) {
            n <-
              viewSums(Views(!is.na(Rle(as.numeric(subject(x)))), as(x, "Rle")))
        } else {
            n <- width(x)
        }
        viewSums(x, na.rm = na.rm) / n
    }
)

setMethod("viewWhichMins", "XDoubleViews",
    function(x, na.rm=FALSE)
        .Call2("XDoubleViews_summary2", x, na.rm, "viewWhichMins",
              PACKAGE="IRanges")
)

setMethod("viewWhichMaxs", "XDoubleViews",
    function(x, na.rm=FALSE)
        .Call2("XDoubleViews_summary2", x, na.rm, "viewWhichMaxs",
              PACKAGE="IRanges")
)

