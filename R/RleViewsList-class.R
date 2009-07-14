### =========================================================================
### RleViewsList objects
### -------------------------------------------------------------------------

setClass("RleViewsList", representation("VIRTUAL"),
         prototype = prototype(elementType = "RleViews"),
         contains = "RangesList")
setClass("SimpleRleViewsList",
         prototype = prototype(elementType = "RleViews"),
         contains = c("RleViewsList", "SimpleRangesList"))

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Constructor.
###

RleViewsList <- function(..., rleList, rangesList, universe = NULL)
{
    if (!is.null(universe) && !isSingleString(universe))
        stop("'universe' must be a single string or NULL")
    views <- list(...)
    if (!missing(rleList) && !missing(rangesList)) {
        if (length(views) > 0)
            stop("'...' must be empty when 'rleList' and 'rangesList' are specified")
        if (!is(rleList, "RleList"))
            stop("'rleList' must be a RleList object")
        if (!is(rangesList, "RangesList"))
            stop("'rangesList' must be a RangesList object")
        views <- Map(Views, rleList, rangesList)
    } else if ((length(views) > 0) &&
            (!missing(rleList) || !missing(rangesList))) {
        stop("cannot specify 'rleList' or 'rangesList' when specifying '...'")
    } else {
        if (length(views) == 1 && is.list(views[[1]]))
            views <- views[[1]]
        if (!all(sapply(views, is, "RleViews")))
            stop("all elements in '...' must be RleViews objects")
    }
    ans <- newSimpleList("SimpleRleViewsList", views)
    universe(ans) <- universe
    ans
}
