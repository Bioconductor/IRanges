### =========================================================================
### RleViewsList objects
### -------------------------------------------------------------------------

setClass("RleViewsList", representation("VIRTUAL"),
         prototype = prototype(elementType = "RleViews"),
         contains = "ViewsList")
setClass("SimpleRleViewsList",
         prototype = prototype(elementType = "RleViews"),
         contains = c("RleViewsList", "SimpleViewsList"))

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Accessor.
###

setMethod("subject", "SimpleRleViewsList",
    function(x)
        S4Vectors:::new_SimpleList_from_list("SimpleRleList",
                                             lapply(x, slot, "subject"))
)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Constructor.
###

setMethod("Views", "RleList",
          function(subject, start=NULL, end=NULL, width=NULL, names=NULL)
              RleViewsList(rleList = subject, rangesList = start))

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
        if (!is(rangesList, "RangesList")) {
            rangesList <- try(IRangesList(rangesList), silent = TRUE)
            if (inherits(rangesList, "try-error"))
                stop("'rangesList' must be a RangesList object")
        }
        views <- Map(Views, rleList, rangesList)
    } else if ((length(views) > 0) &&
            (!missing(rleList) || !missing(rangesList))) {
        stop("cannot specify 'rleList' or 'rangesList' when specifying '...'")
    } else {
        if (length(views) == 1 && is.list(views[[1L]]))
            views <- views[[1L]]
        if (!all(sapply(views, is, "RleViews")))
            stop("all elements in '...' must be RleViews objects")
    }
    ans <- S4Vectors:::new_SimpleList_from_list("SimpleRleViewsList", views)
    universe(ans) <- universe
    ans
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion.
###

setAs("RleViewsList", "IRangesList", function(from)
      IRangesList(lapply(from, as, "IRanges")))

setAs("RleViewsList", "CompressedIRangesList", function(from)
      IRangesList(lapply(from, as, "IRanges"), compress=TRUE))

setAs("RleViewsList", "SimpleIRangesList", function(from)
      IRangesList(lapply(from, as, "IRanges"), compress=FALSE))
