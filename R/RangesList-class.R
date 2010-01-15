### =========================================================================
### RangesList objects
### -------------------------------------------------------------------------

## Accepts any type of Ranges object as an element

setClass("RangesList", representation("VIRTUAL"),
         prototype = prototype(elementType = "Ranges"),
         contains = "Sequence")
setClass("SimpleRangesList",
         prototype = prototype(elementType = "Ranges"),
         contains = c("RangesList", "SimpleList"))

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

setClass("NormalIRangesList", representation("VIRTUAL"),
         prototype = prototype(elementType = "NormalIRanges"),
         contains = "IRangesList")
setClass("CompressedNormalIRangesList",
         prototype = prototype(elementType = "NormalIRanges",
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
          function(x) newSimpleList("SimpleIntegerList", lapply(x, start)))
setMethod("end", "RangesList",
          function(x) newSimpleList("SimpleIntegerList", lapply(x, end)))
setMethod("width", "RangesList",
          function(x) newSimpleList("SimpleIntegerList", lapply(x, width)))
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
              space <- rep(space, elementLengths(x))
            space
          })

setGeneric("universe", function(x) standardGeneric("universe"))
setMethod("universe", "RangesList",
          function(x)
          {
            ### FIXME: for compatibility with older versions, eventually emit warning
            if (is.null(metadata(x)) || is.character(metadata(x)))
              metadata(x)
            else
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

setMethod("isDisjoint", "RangesList",
          function(x) unlist(lapply(x, isDisjoint)))

setMethod("isNormal", "RangesList",
          function(x) unlist(lapply(x, isNormal)))

setMethod("isNormal", "CompressedIRangesList",
          function(x)
          .Call("CompressedIRangesList_isNormal", x, TRUE, PACKAGE = "IRanges"))

setMethod("isNormal", "SimpleIRangesList",
          function(x)
          .Call("SimpleIRangesList_isNormal", x, PACKAGE = "IRanges"))

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
  ans <- newSimpleList("SimpleRangesList", ranges)
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
      ans <- newCompressedList("CompressedIRangesList", ranges)
    else
      ans <- newSimpleList("SimpleIRangesList", ranges)
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
  if (is(i, "RangesList")) {
    ol <- findOverlaps(x, i, multiple = FALSE)
    els <- as.list(x)
    for (j in seq_len(length(x))) {
      els[[j]] <- els[[j]][!is.na(ol[[j]])]
    }
    if (is(x, "CompressedList"))
      ans <- newCompressedList(class(x), els)
    else
      ans <- newSimpleList(class(x), els)
  } else {
    ans <- callNextMethod(x, i)
  }
  ans
}
setMethod("[", "SimpleRangesList", rangesListSingleSquareBracket)
setMethod("[", "CompressedIRangesList", rangesListSingleSquareBracket)
setMethod("[", "SimpleIRangesList", rangesListSingleSquareBracket)

setMethod("[[", "CompressedNormalIRangesList",
          function(x, i, j, ...) newNormalIRangesFromIRanges(callNextMethod()))

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Some useful endomorphisms: "shift", "restrict", "narrow", "resize",
### "flank", "disjoin", "reflect", "reduce", "gaps" and "range".
###

setMethod("shift", "RangesList",
          function(x, shift, use.names = TRUE)
          endoapply(x, "shift", shift = shift, use.names = use.names))

setMethod("shift", "CompressedIRangesList",
          function(x, shift, use.names = TRUE)
          {
            slot(x, "unlistData", check=FALSE) <-
              shift(x@unlistData, shift = shift, use.names = use.names)
            x
          })

setMethod("restrict", "RangesList",
          function(x, start = NA, end = NA, keep.all.ranges = FALSE, use.names = TRUE)
          endoapply(x, restrict, start = start, end = end,
                    keep.all.ranges = keep.all.ranges, use.names = use.names))

setMethod("restrict", "CompressedIRangesList",
          function(x, start = NA, end = NA, keep.all.ranges = FALSE, use.names = TRUE)
          {
            if (!isTRUEorFALSE(keep.all.ranges))
              stop("'keep.all.ranges' must be TRUE or FALSE")
            if (keep.all.ranges)
              slot(x, "unlistData", check=FALSE) <-
                restrict(x@unlistData, start = start, end = end,
                         keep.all.ranges = keep.all.ranges,
                         use.names = use.names)
            else
              x <- callNextMethod()
            x
          })

setMethod("narrow", "RangesList",
          function(x, start = NA, end = NA, width = NA, use.names = TRUE)
          endoapply(x, narrow, start = start, end = end, width = width,
                    use.names = use.names))

setMethod("narrow", "CompressedIRangesList",
          function(x, start = NA, end = NA, width = NA, use.names = TRUE)
          {
            slot(x, "unlistData", check=FALSE) <-
              narrow(x@unlistData, start = start, end = end, width = width,
                     use.names = use.names)
            x
          })

setMethod("resize", "RangesList",
          function(x, width, start = TRUE, use.names = TRUE, symmetric = FALSE)
          endoapply(x, resize, width = width, start = start,
                    use.names = use.names, symmetric = symmetric))

setMethod("resize", "CompressedIRangesList",
          function(x, width, start = TRUE, use.names = TRUE, symmetric = FALSE)
          {
            slot(x, "unlistData", check=FALSE) <-
              resize(x@unlistData, width = width, start = start,
                     use.names = use.names, symmetric = symmetric)
            x
          })

setMethod("flank", "RangesList",
          function(x, width, start = TRUE, both = FALSE, use.names = TRUE)
          endoapply(x, flank, width = width, start = start, both = both,
                    use.names = use.names))

setMethod("flank", "CompressedIRangesList",
          function(x, width, start = TRUE, both = FALSE, use.names = TRUE)
          {
            slot(x, "unlistData", check=FALSE) <-
              flank(x@unlistData, width = width, start = start, both = both,
                    use.names = use.names)
            x
          })

setMethod("disjoin", "RangesList", function(x) endoapply(x, disjoin))

setMethod("reduce", "RangesList",
          function(x, drop.empty.ranges=FALSE, min.gapwidth=1L,
                   with.inframe.attrib=FALSE)
          endoapply(x, reduce, drop.empty.ranges=drop.empty.ranges,
                    min.gapwidth=min.gapwidth,
                    with.inframe.attrib = with.inframe.attrib))

### 'with.inframe.attrib' is ignored for now.
### TODO: Support 'with.inframe.attrib=TRUE'.
setMethod("reduce", "CompressedIRangesList",
    function(x, drop.empty.ranges=FALSE, min.gapwidth=1L,
             with.inframe.attrib=FALSE)
    {
        if (!isTRUEorFALSE(drop.empty.ranges))
            stop("'drop.empty.ranges' must be TRUE or FALSE")
        if (!isSingleNumber(min.gapwidth))
            stop("'min.gapwidth' must be a single integer")
        if (!is.integer(min.gapwidth))
            min.gapwidth <- as.integer(min.gapwidth)
        if (min.gapwidth < 0L)
            stop("'min.gapwidth' must be non-negative")
        if (!identical(with.inframe.attrib, FALSE))
            stop("'with.inframe.attrib' argument not yet supported ",
                 "when reducing a CompressedIRangesList object")
        .Call("CompressedIRangesList_reduce",
              x, drop.empty.ranges, min.gapwidth,
              PACKAGE="IRanges")
    }
)

setMethod("gaps", "RangesList",
          function(x, start=NA, end=NA) {
            ## need to coerce due to limitation of S4 and '...' dispatch
            if (!is(start, "Sequence"))
              start <- IntegerList(as.list(recycleVector(start, length(x))))
            if (!is(end, "Sequence"))
              end <- IntegerList(as.list(recycleVector(end, length(x))))
            mendoapply(gaps, x, start = start, end = end)
          })

setMethod("gaps", "CompressedIRangesList",
    function(x, start=NA, end=NA)
    {
        start <- normargSingleStartOrNA(start)
        end <- normargSingleEndOrNA(end)
        .Call("CompressedIRangesList_gaps", x, start, end, PACKAGE="IRanges")
    }
)

setMethod("range", "RangesList",
          function(x, ..., na.rm)
          {
            args <- unname(list(x, ...))
            if (!all(sapply(sapply(args, universe), identical, universe(x))))
              stop("All args in '...' must have the same universe as 'x'")
            spaceList <- lapply(args, names)
            names <- spaces <- unique(do.call(c, spaceList))
            if (any(sapply(spaceList, is.null))) {
              if (!all(unlist(lapply(args, length)) == length(x)))
                stop("If any args are missing names, all must have same length")
              spaces <- seq_len(length(x))
            }
            ranges <- lapply(spaces, function(space) {
              r <- lapply(args, `[[`, space)
              do.call(range, r[!sapply(r, is.null)])
            })
            names(ranges) <- names
            if (is(x, "CompressedList"))
              newCompressedList(class(x), ranges)
            else
              newSimpleList(class(x), ranges)
          })

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### findOverlaps()
###

setMethod("findOverlaps", c("RangesList", "RangesList"),
          function(query, subject, maxgap = 0, multiple = TRUE,
                   type = c("any", "start", "end", "within", "equal"),
                   drop = FALSE)
          {
            query <- as.list(query)
            subject <- as.list(subject)
            origSubject <- subject
            if (!is.null(names(subject)) && !is.null(names(query))) {
              subject <- subject[names(query)]
              names(subject) <- names(query) # get rid of NA's in names
            }
            else subject <- subject[seq_along(query)]
            ## NULL's are introduced where they do not match
            ## We replace those with empty IRanges
            subject[sapply(subject, is.null)] <- IRanges()
            ans <- lapply(seq_len(length(subject)), function(i) {
              findOverlaps(query[[i]], subject[[i]], maxgap = maxgap,
                           multiple = multiple, type = type)
            })
            names(ans) <- names(subject)
            if (multiple)
              ans <- RangesMatchingList(ans, origSubject)
            else if (drop) {
              off <- head(c(0, cumsum(sapply(origSubject, length))), -1)
              names(off) <- names(origSubject)
              if (is.null(names(ans)))
                off <- off[seq_along(ans)]
              else off <- off[names(ans)]
              ans <- unlist(ans, use.names=FALSE) +
                rep(unname(off), sapply(ans, length))
            } else
              ans <- IntegerList(ans)
            ans
          })

setMethod("%in%", c("RangesList", "RangesList"),
          function(x, table)
          LogicalList(lapply(findOverlaps(x, table, multiple = FALSE),
                             function(y) !is.na(y))))

setMethod("match", c("RangesList", "RangesList"),
          function(x, table, nomatch = NA_integer_, incomparables = NULL)
          {
            if (length(nomatch) != 1)
              stop("'nomatch' must be of length 1") 
            ans <- findOverlaps(x, table, multiple=FALSE, drop=TRUE)
            if (!is.na(nomatch))
              ans[is.na(ans)] <- as.integer(nomatch)
            ans
          })
  
### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Set Operations
###

setMethod("union", c("RangesList", "RangesList"),
          function(x, y) mendoapply(union, x, y))

setMethod("intersect", c("RangesList", "RangesList"),
          function(x, y) mendoapply(intersect, x, y))

setMethod("setdiff", c("RangesList", "RangesList"),
          function(x, y) mendoapply(setdiff, x, y))

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Arithmetic Operations
###

setMethod("Ops", c("RangesList", "ANY"),
          function(e1, e2)
          {
            for (i in seq_len(length(e1)))
              e1[[i]] <- callGeneric(e1[[i]], e2)
            e1
          })

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "show" method.
###

.RangesList_show <- function(object)
{
  k <- length(object)
  cumsumN <- cumsum(elementLengths(object))
  N <- tail(cumsumN, 1)
  cat(class(object), " of length ", k, "\n", sep = "")
  if (k == 0L) {
    return(NULL)
  } else if ((k == 1L) || (N <= 20L)) {
    show(as.list(object))
  } else {
    sketch <- function(x) c(head(x, 3), "...", tail(x, 3))
    if (k >= 3 && cumsumN[3L] <= 20)
      showK <- 3
    else if (k >= 2 && cumsumN[2L] <= 20)
      showK <- 2
    else
      showK <- 1
    diffK <- k - showK
    show(as.list(object[seq_len(showK)]))
    if (diffK > 0)
      cat("...\n<", k - showK,
          ifelse(diffK == 1,
                 " more element>\n", " more elements>\n"),
          sep="")
  }
}
setMethod("show", "RangesList", .RangesList_show)
setMethod("show", "IRangesList", .RangesList_show)

setMethod("showAsCell", "RangesList",
          function(object)
          {
            unlist(lapply(object, function(x) {
                            rng2str <- function(y) 
                              paste("[", start(y), ", ", end(y), "]", sep = "")
                            if (length(x) <= 3)
                              paste(rng2str(x), collapse = " ")
                            else
                              paste(c(head(rng2str(x), 3), "..."),
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
                       factor(rep(seq_len(length(x)), elementLengths(x)),
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
            x <- newSimpleList("SimpleIRangesList", lapply(x, as, "IRanges"))
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
      newCompressedList("CompressedIRangesList", lapply(from, as, "IRanges"),
                        metadata = metadata(from),
                        elementMetadata = elementMetadata(from)))

setAs("RangesList", "SimpleIRangesList",
      function(from)
      newSimpleList("SimpleIRangesList", lapply(from, as, "IRanges"),
                    metadata = metadata(from),
                    elementMetadata = elementMetadata(from)))

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
      newCompressedList("CompressedNormalIRangesList",
                        lapply(from, as, "NormalIRanges"),
                        metadata = metadata(from),
                        elementMetadata = elementMetadata(from)))

setAs("RangesList", "SimpleNormalIRangesList",
      function(from)
      newSimpleList("SimpleNormalIRangesList",
                    lapply(from, as, "NormalIRanges"),
                    metadata = metadata(from),
                    elementMetadata = elementMetadata(from)))

setAs("CompressedIRangesList", "CompressedNormalIRangesList",
      function(from)
      {
        if (!all(isNormal(from)))
          from <- reduce(from, drop.empty.ranges=TRUE)
        new2("CompressedNormalIRangesList", unlistData = from@unlistData,
             partitioning = from@partitioning, metadata = from@metadata,
             elementMetadata = from@elementMetadata, check=FALSE)
      })

setAs("SimpleIRangesList", "SimpleNormalIRangesList",
      function(from)
      {
        if (!all(isNormal(from))) 
          from <- reduce(from, drop.empty.ranges=TRUE)
        new2("SimpleNormalIRangesList", listData = from@listData,
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
      newCompressedList("CompressedIRangesList",
                        lapply(from, as, "IRanges"),
                        metadata = metadata(from),
                        elementMetadata = elementMetadata(from)))

setAs("LogicalList", "SimpleIRangesList",
      function(from)
      newSimpleList("SimpleIRangesList",
                    lapply(from, as, "IRanges"),
                    metadata = metadata(from),
                    elementMetadata = elementMetadata(from)))

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
      newCompressedList("CompressedNormalIRangesList",
                        lapply(from, as, "NormalIRanges"),
                        metadata = metadata(from),
                        elementMetadata = elementMetadata(from)))

setAs("LogicalList", "SimpleNormalIRangesList",
      function(from)
      newSimpleList("SimpleNormalIRangesList",
                    lapply(from, as, "NormalIRanges"),
                    metadata = metadata(from),
                    elementMetadata = elementMetadata(from)))

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
             any(is.na(runValue(from[[1L]])))))
          stop("cannot coerce a non-logical 'RleList' or a logical 'RleList' ",
               "with NAs to a CompressedIRangesList object")
        newCompressedList("CompressedIRangesList",
                          lapply(from, as, "IRanges"),
                          metadata = metadata(from),
                          elementMetadata = elementMetadata(from))
      })

setAs("RleList", "SimpleIRangesList",
      function(from)
      {
        if ((length(from) > 0) &&
            (!is.logical(runValue(from[[1L]])) ||
             any(is.na(runValue(from[[1L]])))))
          stop("cannot coerce a non-logical 'RleList' or a logical 'RleList' ",
               "with NAs to a SimpleIRangesList object")
        newSimpleList("SimpleIRangesList",
                      lapply(from, as, "IRanges"),
                      metadata = metadata(from),
                      elementMetadata = elementMetadata(from))
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
             any(is.na(runValue(from[[1L]])))))
          stop("cannot coerce a non-logical 'RleList' or a logical 'RleList' ",
               "with NAs to a CompressedNormalIRangesList object")
        newCompressedList("CompressedNormalIRangesList",
                          lapply(from, as, "NormalIRanges"),
                          metadata = metadata(from),
                          elementMetadata = elementMetadata(from))
      })

setAs("RleList", "SimpleNormalIRangesList",
      function(from)
      {
        if ((length(from) > 0) &&
            (!is.logical(runValue(from[[1L]])) ||
             any(is.na(runValue(from[[1L]])))))
          stop("cannot coerce a non-logical 'RleList' or a logical 'RleList' ",
               "with NAs to a SimpleNormalIRangesList object")
        newSimpleList("SimpleNormalIRangesList",
                      lapply(from, as, "NormalIRanges"),
                      metadata = metadata(from),
                      elementMetadata = elementMetadata(from))
      })


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "max" and "min" methods for NormalIRangesList objects.
###

CompressedNormalIRangesList.max <- function(x, use.names)
{
    if (!is(x, "CompressedNormalIRangesList"))
        stop("'x' must be a CompressedNormalIRangesList object")
    use.names <- normargUseNames(use.names)
    .Call("CompressedNormalIRangesList_max", x, use.names, PACKAGE="IRanges")
}

setMethod("max", "CompressedNormalIRangesList",
          function(x, ..., na.rm) CompressedNormalIRangesList.max(x, TRUE))

setMethod("max", "SimpleNormalIRangesList",
          function(x, ..., na.rm)
          .Call("SimpleNormalIRangesList_max", x, PACKAGE="IRanges"))

CompressedNormalIRangesList.min <- function(x, use.names)
{
    if (!is(x, "CompressedNormalIRangesList"))
        stop("'x' must be a CompressedNormalIRangesList object")
    use.names <- normargUseNames(use.names)
    .Call("CompressedNormalIRangesList_min", x, use.names, PACKAGE="IRanges")
}

setMethod("min", "CompressedNormalIRangesList",
          function(x, ..., na.rm) CompressedNormalIRangesList.min(x, TRUE))

setMethod("min", "SimpleNormalIRangesList",
          function(x, ..., na.rm)
          .Call("SimpleNormalIRangesList_min", x, PACKAGE="IRanges"))


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "summary" method.
###

setMethod("summary", "CompressedIRangesList",
          function(object)
          .Call("CompressedIRangesList_summary", object, PACKAGE="IRanges"))


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Splitting.
###

setMethod("split", "Ranges",
          function(x, f, drop = FALSE, ...)
          do.call(RangesList, callNextMethod()))

setMethod("split", "IRanges",
          function(x, f, drop = FALSE, ...)
          newCompressedList("CompressedIRangesList", x,
                            splitFactor = f, drop = drop))
