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
    if (!is.null(universe)) {
         msg <- wmsg("The 'universe' argument of the RleViewsList() ",
                     "constructor function is deprecated.")
        .Deprecated(msg=msg)
        if (!isSingleString(universe))
            stop(wmsg("'universe' must be a single string or NULL"))
    }
    views <- list(...)
    if (!missing(rleList) && !missing(rangesList)) {
        if (length(views) > 0)
            stop(wmsg("'...' must be empty when 'rleList' and 'rangesList' ",
                      "are specified"))
        if (!is(rleList, "RleList"))
            stop(wmsg("'rleList' must be a RleList object"))
        if (!is(rangesList, "RangesList")) {
            rangesList <- try(IRangesList(rangesList), silent = TRUE)
            if (inherits(rangesList, "try-error"))
                stop(wmsg("'rangesList' must be a RangesList object"))
        }
        if (length(rleList) != length(rangesList))
            stop("'rleList' and 'rangesList' must have the same length")
        rleList_names <- names(rleList)
        rangesList_names <- names(rangesList)
        if (!(is.null(rleList_names) ||
              is.null(rangesList_names) ||
              identical(rleList_names, rangesList_names))) {
            if (anyDuplicated(rleList_names,) ||
                anyDuplicated(rangesList_names))
                stop(wmsg("when both 'rleList' and 'rangesList' have names, ",
                          "the names on each object cannot have duplicates"))
            if (!setequal(rleList_names, rangesList_names))
                stop(wmsg("when both 'rleList' and 'rangesList' have names, ",
                          "the set of names must be the same on each object"))
            warning(wmsg("'rleList' was reordered so that its names ",
                         "match the names on 'rangesList'"))
            rleList <- rleList[rangesList_names]
        }
        views <- Map(Views, rleList, rangesList)
    } else if ((length(views) > 0) &&
            (!missing(rleList) || !missing(rangesList))) {
        stop(wmsg("cannot specify 'rleList' or 'rangesList' ",
                  "when specifying '...'"))
    } else {
        if (length(views) == 1 && is.list(views[[1L]]))
            views <- views[[1L]]
        if (!all(sapply(views, is, "RleViews")))
            stop(wmsg("all elements in '...' must be RleViews objects"))
    }
    ans <- S4Vectors:::new_SimpleList_from_list("SimpleRleViewsList", views)
    if (!is.null(universe))
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
