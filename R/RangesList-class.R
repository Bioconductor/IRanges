### =========================================================================
### RangesList objects
### -------------------------------------------------------------------------


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### RangesList
###
### Accepts any type of Ranges object as an element.
###

setClass("RangesList", representation("VIRTUAL"),
         prototype = prototype(elementType = "Ranges"),
         contains = "List")

setClass("SimpleRangesList",
         prototype = prototype(elementType = "Ranges"),
         contains = c("RangesList", "SimpleList"))


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### IRangesList
###

setClass("IRangesList", representation("VIRTUAL"),
         prototype = prototype(elementType = "IRanges"),
         contains = "RangesList")

setClass("CompressedIRangesList",
         prototype = prototype(elementType = "IRanges",
                               unlistData = new("IRanges")),
         contains = c("IRangesList", "CompressedList"))

setClass("SimpleIRangesList",
         prototype = prototype(elementType = "IRanges"),
         contains = c("IRangesList", "SimpleRangesList"))


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### NormalIRangesList
###

setClass("NormalIRangesList", representation("VIRTUAL"),
         prototype = prototype(elementType = "NormalIRanges"),
         contains = "IRangesList")

### CompressedNormalIRangesList cannot hold NormalIRanges as its elements,
### due to the compression combining everything into a single
### NormalIRanges (which could easily become non-normal). So just have it
### hold IRanges, instead.
setClass("CompressedNormalIRangesList",
         prototype = prototype(elementType = "IRanges",
                               unlistData = new("IRanges")),
         contains = c("NormalIRangesList", "CompressedIRangesList"))

setClass("SimpleNormalIRangesList",
         prototype = prototype(elementType = "NormalIRanges"),
         contains = c("NormalIRangesList", "SimpleIRangesList"))


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Validity.
###

.valid.NormalIRangesList <- function(x)
{
  if (!all(isNormal(x)))
    return("at least one element of object is not normal")
  NULL
}

setValidity2("NormalIRangesList", .valid.NormalIRangesList)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Accessor methods.
###

setMethod("start", "RangesList",
          function(x) S4Vectors:::new_SimpleList_from_list("SimpleIntegerList",
                                                           lapply(x, start)))
setMethod("end", "RangesList",
          function(x) S4Vectors:::new_SimpleList_from_list("SimpleIntegerList",
                                                           lapply(x, end)))
setMethod("width", "RangesList",
          function(x) S4Vectors:::new_SimpleList_from_list("SimpleIntegerList",
                                                           lapply(x, width)))

setGeneric(".replaceSEW", signature="x",  # not exported
           function(x, FUN, ..., value) standardGeneric(".replaceSEW"))
setMethod(".replaceSEW", "RangesList",
    function(x, FUN, ..., value)
    {
        if (extends(class(value), "IntegerList")) {
            if (!identical(lapply(x, names), lapply(value, names)) &&
                !all(elementNROWS(x) == elementNROWS(value)))
                stop("'value' must have same length and names as current 'ranges'")
        } else if (is.numeric(value)) {
            lelts <- sum(elementNROWS(x))
            if (lelts != length(value))
                value <- rep(value, length.out = lelts)
            if (!is.integer(value))
                value <- as.integer(value)
            value <- split(value, factor(space(x), names(x)))
        } else {
            stop("'value' must extend class IntegerList or integer")
        }
        FUN <- match.fun(FUN)
        for (i in seq_len(length(x)))
            x[[i]] <- FUN(x[[i]], ..., value = value[[i]])
        x
    }
)
setReplaceMethod("start", "RangesList",
    function(x, ..., value) .replaceSEW(x, "start<-", ..., value=value)
)
setReplaceMethod("end", "RangesList",
    function(x, ..., value) .replaceSEW(x, "end<-", ..., value=value)
)
setReplaceMethod("width", "RangesList",
    function(x, ..., value) .replaceSEW(x, "width<-", ..., value=value)
)

setMethod("start", "CompressedIRangesList",
          function(x)
          new2("CompressedIntegerList",
               unlistData = start(unlist(x, use.names=FALSE)),
               partitioning = x@partitioning, check=FALSE))
setMethod("end", "CompressedIRangesList",
          function(x)
          new2("CompressedIntegerList",
               unlistData = end(unlist(x, use.names=FALSE)),
               partitioning = x@partitioning, check=FALSE))
setMethod("width", "CompressedIRangesList",
          function(x)
          new2("CompressedIntegerList",
               unlistData = width(unlist(x, use.names=FALSE)),
               partitioning = x@partitioning, check=FALSE))

setMethod(".replaceSEW", "CompressedIRangesList",
    function(x, FUN, ..., value)
    {
        if (extends(class(value), "IntegerList")) {
            if (!identical(lapply(x, names), lapply(value, names)) &&
                !all(elementNROWS(x) == elementNROWS(value)))
                stop("'value' must have same length and names as current 'ranges'")
            value <- unlist(value)
        } else if (is.numeric(value)) {
            lelts <- sum(elementNROWS(x))
            if (lelts != length(value))
                value <- rep(value, length.out = lelts)
            if (!is.integer(value))
                value <- as.integer(value)
        } else {
            stop("'value' must extend class IntegerList or integer")
        }
        FUN <- match.fun(FUN)
        slot(x, "unlistData", check=FALSE) <-
                        FUN(x@unlistData, ..., value = value)
        x
    }
)

setMethod("space", "RangesList",
          function(x)
          {
            space <- names(x)
            if (!is.null(space))
              space <- factor(rep.int(space, elementNROWS(x)), unique(space))
            space
          })

### TODO: Why not define this at the List level? Or even at the Vector level?
setGeneric("universe", function(x) standardGeneric("universe"))
setMethod("universe", "RangesList",
          function(x)
          {
            .Deprecated(msg="The universe() getter is deprecated.")
            metadata(x)$universe
          })

setGeneric("universe<-", function(x, value) standardGeneric("universe<-"))
setReplaceMethod("universe", "RangesList",
                 function(x, value)
                 {
                   .Deprecated(msg="The universe() setter is deprecated.")
                   if (!is.null(value) && !isSingleString(value))
                     stop("'value' must be a single string or NULL")
                   metadata(x)$universe <- value
                   x
                 })

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### isNormal()
###
### Test the list elements of a RangesList object 'x' individually and return
### a vector of TRUE's or FALSE's parallel to 'x'. More precisely, is
### equivalent to 'sapply(x, FUN)', when FUN is 'isNormal'.
###

setMethod("isNormal", "RangesList",
    function(x, use.names=FALSE)
        vapply(x, isNormal, logical(1), USE.NAMES=use.names)
)

setMethod("isNormal", "SimpleIRangesList",
    function(x, use.names=FALSE)
        .Call2("SimpleIRangesList_isNormal", x, use.names,
               PACKAGE="IRanges")
)

setMethod("isNormal", "CompressedIRangesList",
    function(x, use.names=FALSE)
        .Call2("CompressedIRangesList_isNormal", x, use.names,
               PACKAGE="IRanges")
)

setMethod("whichFirstNotNormal", "RangesList",
          function(x) unlist(lapply(x, whichFirstNotNormal)))


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion from list-like object to IRangesList object
###

### Try to turn an arbitrary list-like object into an ordinary list of
### IRanges objects.
.as_list_of_IRanges <- function(from)
{
    if (is(from, "Ranges")) {
        if (!is(from, "IRanges"))
            from <- as(from, "IRanges", strict=FALSE)
        along_idx <- setNames(seq_along(from), names(from))
        names(from) <- NULL
        mcols(from) <- NULL
        lapply(along_idx, function(i) from[i])
    } else {
        lapply(from, as, "IRanges", strict=FALSE)
    }
}

### From ordinary list to IRangesList

.from_list_to_CompressedIRangesList <- function(from)
{
    from <- .as_list_of_IRanges(from)
    new_CompressedList_from_list("CompressedIRangesList", from)
}

.from_list_to_SimpleIRangesList <- function(from)
{
    from <- .as_list_of_IRanges(from)
    S4Vectors:::new_SimpleList_from_list("SimpleIRangesList", from)
}

setAs("list", "CompressedIRangesList", .from_list_to_CompressedIRangesList)
setAs("list", "SimpleIRangesList", .from_list_to_SimpleIRangesList)
setAs("list", "IRangesList", .from_list_to_SimpleIRangesList)

### From List to IRangesList

.from_List_to_CompressedIRangesList <- function(from)
{
    new_CompressedList_from_list("CompressedIRangesList",
                                 .as_list_of_IRanges(from),
                                 metadata=metadata(from),
                                 mcols=mcols(from))
}

### Ranges objects are List objects so this case is already covered by the
### .from_List_to_CompressedIRangesList() helper above. However, we can
### implement it much more efficiently.
.from_Ranges_to_CompressedIRangesList <- function(from)
{
    if (!is(from, "IRanges"))
        from <- as(from, "IRanges", strict=FALSE)
    ans_partitioning <- PartitioningByEnd(seq_along(from), names=names(from))
    names(from) <- NULL
    ans_mcols <- mcols(from)
    mcols(from) <- NULL
    ans <- relist(from, ans_partitioning)
    mcols(ans) <- ans_mcols
    ans
}

.from_List_to_SimpleIRangesList <- function(from)
{
    S4Vectors:::new_SimpleList_from_list("SimpleIRangesList",
                                 .as_list_of_IRanges(from),
                                 metadata=metadata(from),
                                 mcols=mcols(from))
}

setAs("List", "CompressedIRangesList", .from_List_to_CompressedIRangesList)
setAs("Ranges", "CompressedIRangesList", .from_Ranges_to_CompressedIRangesList)
setAs("List", "SimpleIRangesList", .from_List_to_SimpleIRangesList)

### Automatic coercion method from RangesList to SimpleIRangesList silently
### returns a broken object (unfortunately these dummy automatic coercion
### methods don't bother to validate the object they return). So we overwrite
### it.
setAs("RangesList", "SimpleIRangesList", .from_List_to_SimpleIRangesList)
setAs("SimpleRangesList", "SimpleIRangesList", .from_List_to_SimpleIRangesList)

setAs("List", "IRangesList",
    function(from)
    {
        if (is(from, "CompressedList") || is(from, "Ranges"))
            as(from, "CompressedIRangesList")
        else
            as(from, "SimpleIRangesList")
    }
)

### This case is already covered by the List-to-CompressedIRangesList coercion
### above. However, we can implement it much more efficiently.
setAs("CompressedRleList", "CompressedIRangesList",
      function(from)
      {
        if ((length(from) > 0) &&
            (!is.logical(runValue(from[[1L]])) ||
             S4Vectors:::anyMissing(runValue(from[[1L]]))))
          stop("cannot coerce a non-logical 'RleList' or a logical 'RleList' ",
               "with NAs to a CompressedIRangesList object")
        ranges <- as(unlist(from, use.names = FALSE), "IRanges")
        to <- diceRangesByList(ranges, from)
        metadata(to) <- metadata(from)
        mcols(to) <- mcols(from)
        to
      })


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Constructor.
###

RangesList <- function(..., universe = NULL)
{
  if (!is.null(universe)) {
     msg <- wmsg("The 'universe' argument of the RangesList() ",
                 "constructor function is deprecated.")
    .Deprecated(msg=msg)
    if (!isSingleString(universe))
       stop("'universe' must be a single string or NULL")
  }
  ranges <- list(...)
  if (length(ranges) == 1 && is.list(ranges[[1L]]))
    ranges <- ranges[[1L]]
  if (!all(sapply(ranges, is, "Ranges")))
    stop("all elements in '...' must be Ranges objects")
  ans <- S4Vectors:::new_SimpleList_from_list("SimpleRangesList", ranges)
  if (!is.null(universe))
    universe(ans) <- universe
  ans
}

IRangesList <- function(..., universe=NULL, compress=TRUE)
{
    if (!isTRUEorFALSE(compress))
        stop("'compress' must be TRUE or FALSE")
    if (!is.null(universe)) {
        msg <- wmsg("The 'universe' argument of the IRangesList() ",
                    "constructor function is deprecated.")
        .Deprecated(msg=msg)
        if (!isSingleString(universe))
            stop("'universe' must be a single string or NULL")
    }
    args <- list(...)
    if (length(args) == 2L &&
        setequal(names(args), c("start", "end")) &&
        !is(args[[1L]], "Ranges") && !is(args[[2L]], "Ranges"))
    {
        if (!compress)
            stop(wmsg("'compress' must be TRUE when passing the 'start' ",
                      "and 'end' arguments"))
        ans_start <- IntegerList(args[["start"]], compress=TRUE)
        ans_end <- IntegerList(args[["end"]], compress=TRUE)
        ans_partitioning <- PartitioningByEnd(ans_start)
        if (!identical(ans_partitioning, PartitioningByEnd(ans_end)))
            stop("'start' and 'end' are not compatible")
        unlisted_start <- unlist(ans_start, use.names=FALSE)
        unlisted_end <- unlist(ans_end, use.names=FALSE)
        unlisted_ans <- IRanges(start=unlisted_start, end=unlisted_end)
        ans <- relist(unlisted_ans, ans_partitioning)
    } else {
        if (length(args) == 1L) {
            x1 <- args[[1L]]
            if (is.list(x1) || (is(x1, "List") && !is(x1, "Ranges")))
                args <- x1
        }
        if (compress)
            ans <- as(args, "CompressedIRangesList")
        else
            ans <- as(args, "SimpleIRangesList")
    }
    if (!is.null(universe))
        universe(ans) <- universe
    ans
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Subsetting.
###

setMethod("getListElement", "CompressedNormalIRangesList",
    function(x, i, exact=TRUE) newNormalIRangesFromIRanges(callNextMethod())
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "show" method.
###

showRangesList <- function(x, with.header=TRUE)
{
    x_len <- length(x)
    if (with.header)
        cat(classNameForDisplay(x), " of length ", x_len, "\n",
            sep = "")
    if (x_len == 0L)
        return(invisible(NULL))
    cumsumN <- end(PartitioningByEnd(x))
    N <- tail(cumsumN, 1)
    if (x_len <= 5L && N <= 20L) {
        show(as.list(x))
        return(invisible(NULL))
    }
    if (x_len >= 3L && cumsumN[3L] <= 20L) {
        showK <- 3L
    } else if (x_len >= 2L && cumsumN[2L] <= 20L) {
        showK <- 2L
    } else {
        showK <- 1L
    }
    show(as.list(x[seq_len(showK)]))
    diffK <- x_len - showK
    if (diffK > 0L)
        cat("...\n<", x_len - showK,
            ifelse(diffK == 1L,
                   " more element>\n", " more elements>\n"),
            sep="")
}

setMethod("show", "RangesList", function(object) showRangesList(object))

setMethod("showAsCell", "RangesList",
          function(object)
          {
            unlist(lapply(object, function(x) {
                            if (length(x) <= 3)
                              paste(showAsCell(x), collapse = " ")
                            else
                              paste(c(showAsCell(head(x, 3)), "..."),
                                    collapse = " ")
                          }), use.names = FALSE)
          })


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### More coercions
###

.as.list.CompressedNormalIRangesList <- function(x, use.names=TRUE)
{
    if (!isTRUEorFALSE(use.names))
        stop("'use.names' must be TRUE or FALSE")
    ans <- lapply_CompressedList(x, newNormalIRangesFromIRanges)
    if (use.names)
        names(ans) <- names(x)
    ans
}
setMethod("as.list", "CompressedNormalIRangesList",
    .as.list.CompressedNormalIRangesList)

setMethod("unlist", "SimpleNormalIRangesList",
          function(x, recursive = TRUE, use.names = TRUE)
          {
            x <- S4Vectors:::new_SimpleList_from_list("SimpleIRangesList",
                                                      lapply(x, as, "IRanges"))
            callGeneric()
          })

setAs("list", "RangesList", function(from) {
  S4Vectors:::coerceToSimpleList(from, "Ranges")
})

setAs("Ranges", "RangesList", function(from) as(from, "IRangesList"))

setAs("RangesList", "SimpleRangesList",
      function(from)
      S4Vectors:::new_SimpleList_from_list("SimpleRangesList",
                                           lapply(from, as, "Ranges"),
                                           metadata = metadata(from),
                                           mcols = mcols(from)))

### Coercion from RangesList to NormalIRangesList.

.from_RangesList_to_SimpleNormalIRangesList <- function(from)
{
    S4Vectors:::new_SimpleList_from_list("SimpleNormalIRangesList",
        lapply(from, as, "NormalIRanges"),
        mcols=mcols(from),
        metadata=metadata(from))
}

setAs("RangesList", "SimpleNormalIRangesList",
    .from_RangesList_to_SimpleNormalIRangesList
)

setAs("SimpleIRangesList", "SimpleNormalIRangesList",
    .from_RangesList_to_SimpleNormalIRangesList
)

setAs("NormalIRangesList", "CompressedNormalIRangesList",
    function(from)
    {
        ans <- as(from, "CompressedIRangesList", strict=FALSE)
        class(ans) <- "CompressedNormalIRangesList"
        ans
    }
)

setAs("CompressedIRangesList", "CompressedNormalIRangesList",
    function(from)
    {
        if (!all(isNormal(from)))
            from <- reduce(from, drop.empty.ranges=TRUE)
        class(from) <- "CompressedNormalIRangesList"
        from
    }
)

setAs("RangesList", "CompressedNormalIRangesList",
    function(from)
    {
        as(as(from, "CompressedIRangesList", strict=FALSE),
           "CompressedNormalIRangesList")
    }
)

setAs("RangesList", "NormalIRangesList",
    function(from)
    {
        if (is(from, "SimpleRangesList"))
            as(from, "SimpleNormalIRangesList")
        else
            as(from, "CompressedNormalIRangesList")
    }
)

### Coercion from LogicalList to NormalIRangesList.

setAs("LogicalList", "NormalIRangesList",
      function(from)
      {
        if (is(from, "CompressedList"))
          as(from, "CompressedNormalIRangesList")
        else
          as(from, "SimpleNormalIRangesList")
      })

setAs("LogicalList", "CompressedNormalIRangesList",
      function(from)
      new_CompressedList_from_list("CompressedNormalIRangesList",
                                   lapply(from, as, "NormalIRanges"),
                                   metadata = metadata(from),
                                   mcols = mcols(from)))

setAs("LogicalList", "SimpleNormalIRangesList",
      function(from)
      S4Vectors:::new_SimpleList_from_list("SimpleNormalIRangesList",
                                           lapply(from, as, "NormalIRanges"),
                                           metadata = metadata(from),
                                           mcols = mcols(from)))

### Coercion from RleList to NormalIRangesList.

setAs("RleList", "NormalIRangesList",
        function(from)
        {
            if (is(from, "CompressedList"))
                as(from, "CompressedNormalIRangesList")
            else
                as(from, "SimpleNormalIRangesList")
        })

setAs("RleList", "CompressedNormalIRangesList",
      function(from)
      {
        if ((length(from) > 0) &&
            (!is.logical(runValue(from[[1L]])) ||
             S4Vectors:::anyMissing(runValue(from[[1L]]))))
          stop("cannot coerce a non-logical 'RleList' or a logical 'RleList' ",
               "with NAs to a CompressedNormalIRangesList object")
        new_CompressedList_from_list("CompressedNormalIRangesList",
                                     lapply(from, as, "NormalIRanges"),
                                     metadata = metadata(from),
                                     mcols = mcols(from))
      })

setAs("RleList", "SimpleNormalIRangesList",
      function(from)
      {
        if ((length(from) > 0) &&
            (!is.logical(runValue(from[[1L]])) ||
             S4Vectors:::anyMissing(runValue(from[[1L]]))))
          stop("cannot coerce a non-logical 'RleList' or a logical 'RleList' ",
               "with NAs to a SimpleNormalIRangesList object")
        S4Vectors:::new_SimpleList_from_list("SimpleNormalIRangesList",
                                             lapply(from, as, "NormalIRanges"),
                                             metadata = metadata(from),
                                             mcols = mcols(from))
      })


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### merge()
###

### Merges various RangesList objects into a single RangesList object.
### The merging is either by name (if all the RangesList objects have names),
### or by position (if any RangesList object is missing names).
### When merging by name, and in case of duplicated names within a given
### RangesList, the elements corresponding to the duplicated names are ignored.
### When merging by position, all the RangesList objects must have the same
### length.
### Note that the "range" method for RangesList objects expects "merge" to
### behave like this.  
.RangesList.merge <- function(...)
{
    args <- unname(list(...))
    if (length(args) == 0L)
        stop("nothing to merge")
    x <- args[[1L]]
    spaceList <- lapply(args, names)
    names <- spaces <- unique(do.call(c, spaceList))
    if (any(sapply(spaceList, is.null))) {
        ## Merging by position.
        if (!all(unlist(lapply(args, length)) == length(x)))
            stop("if any RangesList objects to merge are missing names, ",
                 "all must have same length")
        names <- NULL
        spaces <- seq_len(length(x))
    }
    ranges <- lapply(spaces,
                     function(space) {
                       r <- lapply(args, `[[`, space)
                       do.call(c, r[!sapply(r, is.null)])
                     })
    names(ranges) <- names
    if (is(x, "CompressedList"))
      ans <- new_CompressedList_from_list(class(x), ranges)
    else
      ans <- S4Vectors:::new_SimpleList_from_list(class(x), ranges)
    ans
}

setMethod("merge", c("RangesList", "missing"),
    function(x, y, ...) .RangesList.merge(x, ...)
)

setMethod("merge", c("missing", "RangesList"),
    function(x, y, ...) .RangesList.merge(y, ...)
)

setMethod("merge", c("RangesList", "RangesList"),
    function(x, y, ...) .RangesList.merge(x, y, ...)
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "max" and "min" methods for NormalIRangesList objects.
###

CompressedNormalIRangesList.max <- function(x, use.names)
{
    if (!is(x, "CompressedNormalIRangesList"))
        stop("'x' must be a CompressedNormalIRangesList object")
    use.names <- S4Vectors:::normargUseNames(use.names)
    .Call2("CompressedNormalIRangesList_max", x, use.names, PACKAGE="IRanges")
}

setMethod("max", "CompressedNormalIRangesList",
          function(x, ..., na.rm) CompressedNormalIRangesList.max(x, TRUE))

setMethod("max", "SimpleNormalIRangesList",
          function(x, ..., na.rm)
          .Call2("SimpleNormalIRangesList_max", x, PACKAGE="IRanges"))

CompressedNormalIRangesList.min <- function(x, use.names)
{
    if (!is(x, "CompressedNormalIRangesList"))
        stop("'x' must be a CompressedNormalIRangesList object")
    use.names <- S4Vectors:::normargUseNames(use.names)
    .Call2("CompressedNormalIRangesList_min", x, use.names, PACKAGE="IRanges")
}

setMethod("min", "CompressedNormalIRangesList",
          function(x, ..., na.rm) CompressedNormalIRangesList.min(x, TRUE))

setMethod("min", "SimpleNormalIRangesList",
          function(x, ..., na.rm)
          .Call2("SimpleNormalIRangesList_min", x, PACKAGE="IRanges"))


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "summary" method.
###

setMethod("summary", "CompressedIRangesList",
          function(object)
          .Call2("CompressedIRangesList_summary", object, PACKAGE="IRanges"))

