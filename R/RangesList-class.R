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
          function(x) newList("SimpleIntegerList", lapply(x, start)))
setMethod("end", "RangesList",
          function(x) newList("SimpleIntegerList", lapply(x, end)))
setMethod("width", "RangesList",
          function(x) newList("SimpleIntegerList", lapply(x, width)))
setGeneric(".SEW<-", signature="x",
           function(x, FUN, check=TRUE, value) standardGeneric(".SEW<-"))
setReplaceMethod(".SEW", "RangesList",
                 function(x, FUN, check=TRUE, value)
                 {
                   if (!isTRUEorFALSE(check))
                     stop("'check' must be TRUE or FALSE")
                   if (extends(class(value), "IntegerList")) {
                     if (!identical(lapply(x, names), lapply(value, names)) &&
                         !all(elementLengths(x) == elementLengths(value)))
                       stop("'value' must have same length and names as current 'ranges'")
                   } else if (is.numeric(value)) {
                     lelts <- sum(elementLengths(x))
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
                     x[[i]] <- FUN(x[[i]], check = check, value = value[[i]])
                   x
                 })
setReplaceMethod("start", "RangesList",
                 function(x, check=TRUE, value)
                 {
                   if (!isTRUEorFALSE(check))
                     stop("'check' must be TRUE or FALSE")
                   .SEW(x, FUN = "start<-", check = check) <- value
                   x
                 })
setReplaceMethod("end", "RangesList",
                 function(x, check=TRUE, value)
                 {
                   if (!isTRUEorFALSE(check))
                     stop("'check' must be TRUE or FALSE")
                   .SEW(x, FUN = "end<-", check = check) <- value
                   x
                 })
setReplaceMethod("width", "RangesList",
                 function(x, check=TRUE, value)
                 {
                   if (!isTRUEorFALSE(check))
                     stop("'check' must be TRUE or FALSE")
                   .SEW(x, FUN = "width<-", check = check) <- value
                   x
                 })

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
setReplaceMethod(".SEW", "CompressedIRangesList",
                 function(x, FUN, check=TRUE, value)
                 {
                   if (!isTRUEorFALSE(check))
                     stop("'check' must be TRUE or FALSE")
                   if (extends(class(value), "IntegerList")) {
                     if (!identical(lapply(x, names), lapply(value, names)) &&
                         !all(elementLengths(x) == elementLengths(value)))
                       stop("'value' must have same length and names as current 'ranges'")
                     value <- unlist(value)
                   } else if (is.numeric(value)) {
                     lelts <- sum(elementLengths(x))
                     if (lelts != length(value))
                       value <- rep(value, length.out = lelts)
                     if (!is.integer(value))
                       value <- as.integer(value)
                   } else {
                     stop("'value' must extend class IntegerList or integer")
                   }
                   FUN <- match.fun(FUN)
                   slot(x, "unlistData", check=FALSE) <-
                     FUN(x@unlistData, check = check, value = value)
                   x
                 })

setGeneric("space", function(x, ...) standardGeneric("space"))
setMethod("space", "RangesList",
          function(x)
          {
            space <- names(x)
            if (!is.null(space))
              space <- factor(rep.int(space, elementLengths(x)), space)
            space
          })

### TODO: Why not define this at the List level? Or even at the Vector level?
setGeneric("universe", function(x) standardGeneric("universe"))
setMethod("universe", "RangesList",
          function(x)
          {
            metadata(x)$universe
          })

setGeneric("universe<-", function(x, value) standardGeneric("universe<-"))
setReplaceMethod("universe", "RangesList",
                 function(x, value)
                 {
                   if (!is.null(value) && !isSingleString(value))
                     stop("'value' must be a single string or NULL")
                   metadata(x)$universe <- value
                   x
                 })

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Testing a RangesList object.
###

setMethod("isNormal", "RangesList",
          function(x) unlist(lapply(x, isNormal)))

setMethod("isNormal", "CompressedIRangesList",
          function(x)
          .Call2("CompressedIRangesList_isNormal", x, TRUE, PACKAGE = "IRanges"))

setMethod("isNormal", "SimpleIRangesList",
          function(x)
          .Call2("SimpleIRangesList_isNormal", x, PACKAGE = "IRanges"))

setMethod("whichFirstNotNormal", "RangesList",
          function(x) unlist(lapply(x, whichFirstNotNormal)))

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Constructor.
###

RangesList <- function(..., universe = NULL)
{
  if (!is.null(universe) && !isSingleString(universe))
    stop("'universe' must be a single string or NULL")
  ranges <- list(...)
  if (length(ranges) == 1 && is.list(ranges[[1L]]))
    ranges <- ranges[[1L]]
  if (!all(sapply(ranges, is, "Ranges")))
    stop("all elements in '...' must be Ranges objects")
  ans <- newList("SimpleRangesList", ranges)
  universe(ans) <- universe
  ans
}

IRangesList <- function(..., universe = NULL, compress = TRUE)
{
  if (!isTRUEorFALSE(compress))
    stop("'compress' must be TRUE or FALSE")
  if (!is.null(universe) && !isSingleString(universe))
    stop("'universe' must be a single string or NULL")
  ranges <- list(...)
  if (length(ranges) == 1 &&
      (is(ranges[[1L]], "LogicalList") || is(ranges[[1L]], "RleList"))) {
    if (compress)
      ans <- as(ranges[[1L]], "CompressedIRangesList")
    else
      ans <- as(ranges[[1L]], "SimpleIRangesList")
  } else if (length(ranges) == 2 &&
             setequal(names(ranges), c("start", "end")) &&
             !is(ranges[[1L]], "Ranges") && !is(ranges[[2L]], "Ranges")) {
    if (!compress)
      stop("'compress' must be TRUE when passing the 'start' and 'end' arguments")
    ans_start <- IntegerList(ranges[["start"]], compress = TRUE)
    ans_end <- IntegerList(ranges[["end"]], compress = TRUE)
    if (!identical(ans_start@partitioning@end, ans_end@partitioning@end))
      stop("'start' and 'end' are not compatible")
    ans_partitioning <- ans_start@partitioning
    ans_unlistData <- IRanges(start=ans_start@unlistData, end=ans_end@unlistData)
    ans <- new2("CompressedIRangesList",
                partitioning=ans_partitioning,
                unlistData=ans_unlistData,
                check=FALSE)
  } else {
    if (length(ranges) == 1 && is.list(ranges[[1L]]))
      ranges <- ranges[[1L]]
    if (!all(sapply(ranges, is, "IRanges")))
      stop("all elements in '...' must be IRanges objects")
    if (compress)
      ans <- newList("CompressedIRangesList", ranges)
    else
      ans <- newList("SimpleIRangesList", ranges)
  }
  universe(ans) <- universe
  ans
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Subsetting.
###

### Support subsetting by another RangesList
rangesListSingleSquareBracket <- function(x, i, j, ..., drop)
{
  if (!missing(j) || length(list(...)) > 0)
    stop("invalid subsetting")
  if (missing(i))
    return(x)
  if (is(i, "RangesList"))
    stop("'[' subsetting by RangesList is defunct.\n",
         "Use 'subsetByOverlaps' instead.")
  callNextMethod(x, i)
}
setMethod("[", "SimpleRangesList", rangesListSingleSquareBracket)
setMethod("[", "CompressedIRangesList", rangesListSingleSquareBracket)
setMethod("[", "SimpleIRangesList", rangesListSingleSquareBracket)

setMethod("[[", "CompressedNormalIRangesList",
          function(x, i, j, ...) newNormalIRangesFromIRanges(callNextMethod()))


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
    if (!all(sapply(sapply(args, universe), identical, universe(x))))
        stop("all RangesList objects to merge must have the same universe")
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
    newList(class(x), ranges)
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
### The "show" method.
###

.RangesList_show <- function(x)
{
    x_len <- length(x)
    cat(class(x), " of length ", x_len, "\n", sep = "")
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

setMethod("show", "RangesList", function(object) .RangesList_show(object))

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
### Coercion.
###

setMethod("as.data.frame", "RangesList",
          function(x, row.names=NULL, optional=FALSE, ...)
          {
            if (!(is.null(row.names) || is.character(row.names)))
              stop("'row.names'  must be NULL or a character vector")
            x <- as(x, "CompressedIRangesList")
            spaceLevels <- seq_len(length(x))
            if (length(names(x)) > 0) {
              spaceLabels <- names(x)
            } else {
              spaceLabels <- as.character(spaceLevels)
            }
            data.frame(space =
                       factor(rep.int(seq_len(length(x)), elementLengths(x)),
                              level = spaceLevels,
                              labels = spaceLabels),
                       as.data.frame(unlist(x, use.names = FALSE)),
                       row.names = row.names,
                       stringsAsFactors = FALSE)
          })

setMethod("as.list", "CompressedNormalIRangesList",
          function(x, use.names = TRUE)
          .CompressedList.list.subscript(X = x,
                                         INDEX = seq_len(length(x)),
                                         USE.NAMES = use.names,
                                         FUN = newNormalIRangesFromIRanges,
                                         COMPRESS = FALSE))

setMethod("unlist", "SimpleNormalIRangesList",
          function(x, recursive = TRUE, use.names = TRUE)
          {
            x <- newList("SimpleIRangesList", lapply(x, as, "IRanges"))
            callGeneric()
          })

setAs("RangesList", "IRangesList",
      function(from)
      {
        if (is(from, "CompressedList"))
          as(from, "CompressedIRangesList")
        else
          as(from, "SimpleIRangesList")
      })

setAs("RangesList", "CompressedIRangesList",
      function(from)
      newList("CompressedIRangesList",
              lapply(from, as, "IRanges"),
              metadata = metadata(from),
              mcols = mcols(from)))

setAs("RangesList", "SimpleIRangesList",
      function(from)
      newList("SimpleIRangesList",
              lapply(from, as, "IRanges"),
              metadata = metadata(from),
              mcols = mcols(from)))

setAs("RangesList", "NormalIRangesList",
      function(from)
      {
        if (is(from, "CompressedList"))
          as(from, "CompressedNormalIRangesList")
        else
          as(from, "SimpleNormalIRangesList")
      })

setAs("RangesList", "CompressedNormalIRangesList",
      function(from)
      newList("CompressedNormalIRangesList",
              lapply(from, as, "NormalIRanges"),
              metadata = metadata(from),
              mcols = mcols(from)))

setAs("RangesList", "SimpleNormalIRangesList",
      function(from)
      newList("SimpleNormalIRangesList",
              lapply(from, as, "NormalIRanges"),
              metadata = metadata(from),
              mcols = mcols(from)))

setAs("CompressedIRangesList", "CompressedNormalIRangesList",
      function(from)
      {
        if (!all(isNormal(from)))
          from <- reduce(from, drop.empty.ranges=TRUE)
        new2("CompressedNormalIRangesList", from, check=FALSE)
      })

setAs("SimpleIRangesList", "SimpleNormalIRangesList",
      function(from)
      {
        if (!all(isNormal(from))) 
          from <- reduce(from, drop.empty.ranges=TRUE)
        new2("SimpleNormalIRangesList",
             listData = lapply(from@listData, newNormalIRangesFromIRanges,
               check = FALSE),
             metadata = from@metadata,
             elementMetadata = from@elementMetadata, check=FALSE)
      })

setAs("LogicalList", "IRangesList",
      function(from)
      {
        if (is(from, "CompressedList"))
          as(from, "CompressedIRangesList")
        else
          as(from, "SimpleIRangesList")
      })

setAs("LogicalList", "CompressedIRangesList",
      function(from)
      newList("CompressedIRangesList",
              lapply(from, as, "IRanges"),
              metadata = metadata(from),
              mcols = mcols(from)))

setAs("LogicalList", "SimpleIRangesList",
      function(from)
      newList("SimpleIRangesList",
              lapply(from, as, "IRanges"),
              metadata = metadata(from),
              mcols = mcols(from)))

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
      newList("CompressedNormalIRangesList",
              lapply(from, as, "NormalIRanges"),
              metadata = metadata(from),
              mcols = mcols(from)))

setAs("LogicalList", "SimpleNormalIRangesList",
      function(from)
      newList("SimpleNormalIRangesList",
              lapply(from, as, "NormalIRanges"),
              metadata = metadata(from),
              mcols = mcols(from)))

setAs("RleList", "IRangesList",
      function(from)
      {
        if (is(from, "CompressedList"))
          as(from, "CompressedIRangesList")
        else
          as(from, "SimpleIRangesList")
      })

setAs("RleList", "CompressedIRangesList",
      function(from)
      {
        if ((length(from) > 0) &&
            (!is.logical(runValue(from[[1L]])) ||
             anyMissing(runValue(from[[1L]]))))
          stop("cannot coerce a non-logical 'RleList' or a logical 'RleList' ",
               "with NAs to a CompressedIRangesList object")
        newList("CompressedIRangesList",
                lapply(from, as, "IRanges"),
                metadata = metadata(from),
                mcols = mcols(from))
      })

setAs("CompressedRleList", "CompressedIRangesList",
      function(from)
      {
        if ((length(from) > 0) &&
            (!is.logical(runValue(from[[1L]])) ||
             anyMissing(runValue(from[[1L]]))))
          stop("cannot coerce a non-logical 'RleList' or a logical 'RleList' ",
               "with NAs to a CompressedIRangesList object")
        ranges <- as(unlist(from, use.names = FALSE), "IRanges")
        to <- diceRangesByList(ranges, from)
        metadata(to) <- metadata(from)
        mcols(to) <- mcols(from)
        to
      })

  
setAs("RleList", "SimpleIRangesList",
      function(from)
      {
        if ((length(from) > 0) &&
            (!is.logical(runValue(from[[1L]])) ||
             anyMissing(runValue(from[[1L]]))))
          stop("cannot coerce a non-logical 'RleList' or a logical 'RleList' ",
               "with NAs to a SimpleIRangesList object")
        newList("SimpleIRangesList",
                lapply(from, as, "IRanges"),
                metadata = metadata(from),
                mcols = mcols(from))
      })


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
             anyMissing(runValue(from[[1L]]))))
          stop("cannot coerce a non-logical 'RleList' or a logical 'RleList' ",
               "with NAs to a CompressedNormalIRangesList object")
        newList("CompressedNormalIRangesList",
                lapply(from, as, "NormalIRanges"),
                metadata = metadata(from),
                mcols = mcols(from))
      })

setAs("RleList", "SimpleNormalIRangesList",
      function(from)
      {
        if ((length(from) > 0) &&
            (!is.logical(runValue(from[[1L]])) ||
             anyMissing(runValue(from[[1L]]))))
          stop("cannot coerce a non-logical 'RleList' or a logical 'RleList' ",
               "with NAs to a SimpleNormalIRangesList object")
        newList("SimpleNormalIRangesList",
                lapply(from, as, "NormalIRanges"),
                metadata = metadata(from),
                mcols = mcols(from))
      })


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "max" and "min" methods for NormalIRangesList objects.
###

CompressedNormalIRangesList.max <- function(x, use.names)
{
    if (!is(x, "CompressedNormalIRangesList"))
        stop("'x' must be a CompressedNormalIRangesList object")
    use.names <- normargUseNames(use.names)
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
    use.names <- normargUseNames(use.names)
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


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Splitting.
###

### Seems broken to me. We probably don't need this anyway!
#setMethod("split", "Ranges",
#          function(x, f, drop = FALSE, ...)
#          do.call(RangesList, callNextMethod()))

