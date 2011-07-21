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
        ranges <- .Call2("XInteger_slice", x, lower, upper,
                        PACKAGE="IRanges")
        Views(x, ranges)
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "viewMins", "viewMaxs", "viewSums", "viewWhichMins", and
### "viewWhichMaxs" methods.
###

setMethod("viewMins", "XIntegerViews",
    function(x, na.rm=FALSE)
        .Call2("XIntegerViews_summary1", x, na.rm, "viewMins",
              PACKAGE="IRanges")
)

setMethod("viewMaxs", "XIntegerViews",
    function(x, na.rm=FALSE)
        .Call2("XIntegerViews_summary1", x, na.rm, "viewMaxs",
              PACKAGE="IRanges")
)

setMethod("viewSums", "XIntegerViews",
    function(x, na.rm=FALSE)
        .Call2("XIntegerViews_summary1", x, na.rm, "viewSums",
              PACKAGE="IRanges")
)

setMethod("viewMeans", "XIntegerViews",
    function(x, na.rm=FALSE) {
        if (!isTRUEorFALSE(na.rm))
            stop("'na.rm' must be TRUE or FALSE")
        if (na.rm) {
            n <-
              viewSums(Views(!is.na(Rle(as.integer(subject(x)))), as(x, "Rle")))
        } else {
            n <- width(x)
        }
        viewSums(x, na.rm = na.rm) / n
    }
)

setMethod("viewWhichMins", "XIntegerViews",
    function(x, na.rm=FALSE)
        .Call2("XIntegerViews_summary2", x, na.rm, "viewWhichMins",
              PACKAGE="IRanges")
)

setMethod("viewWhichMaxs", "XIntegerViews",
    function(x, na.rm=FALSE)
        .Call2("XIntegerViews_summary2", x, na.rm, "viewWhichMaxs",
              PACKAGE="IRanges")
)

