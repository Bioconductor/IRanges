### =========================================================================
### IntegerRangesList objects
### -------------------------------------------------------------------------


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### IntegerRangesList
###
### Accepts any type of IntegerRanges object as an element.
###

setClass("IntegerRangesList",
    contains="RangesList",
    representation("VIRTUAL"),
    prototype(elementType="IntegerRanges")
)

setClass("SimpleIntegerRangesList",
    contains=c("IntegerRangesList", "SimpleRangesList"),
    representation("VIRTUAL")
)

setClass("IntegerPosList",
    contains=c("PosList", "IntegerRangesList"),
    representation("VIRTUAL"),
    prototype(elementType="IntegerPos")
)

setClass("SimpleIntegerPosList",
    contains=c("IntegerPosList", "SimpleIntegerRangesList"),
    representation("VIRTUAL")
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### IRangesList
###

setClass("IRangesList",
    contains="IntegerRangesList",
    representation("VIRTUAL"),
    prototype(elementType="IRanges")
)

setClass("SimpleIRangesList",
    contains=c("IRangesList", "SimpleIntegerRangesList")
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### NormalIRangesList
###

setClass("NormalIRangesList",
    contains="IRangesList",
    representation("VIRTUAL"),
    prototype(elementType="NormalIRanges")
)

setClass("SimpleNormalIRangesList",
     contains=c("NormalIRangesList", "SimpleIRangesList")
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### IPosList
###

setClass("IPosList",
    contains="IntegerPosList",
    representation("VIRTUAL"),
    prototype(elementType="IPos")
)

setClass("SimpleIPosList",
    contains=c("IPosList", "SimpleIntegerPosList")
)


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

setGeneric(".replaceSEW", signature="x",  # not exported
    function(x, FUN, ..., value) standardGeneric(".replaceSEW")
)

setMethod(".replaceSEW", "IntegerRangesList",
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

setReplaceMethod("start", "IntegerRangesList",
    function(x, ..., value) .replaceSEW(x, "start<-", ..., value=value)
)

setReplaceMethod("end", "IntegerRangesList",
    function(x, ..., value) .replaceSEW(x, "end<-", ..., value=value)
)

setReplaceMethod("width", "IntegerRangesList",
    function(x, ..., value) .replaceSEW(x, "width<-", ..., value=value)
)

setMethod("space", "IntegerRangesList",
          function(x)
          {
            space <- names(x)
            if (!is.null(space))
              space <- factor(rep.int(space, elementNROWS(x)), unique(space))
            space
          })

### TODO: Why not define this at the List level? Or even at the Vector level?
setGeneric("universe", function(x) standardGeneric("universe"))
setMethod("universe", "IntegerRangesList",
          function(x)
          {
            .Defunct(msg="The universe() getter is defunct.")
          })

setGeneric("universe<-", function(x, value) standardGeneric("universe<-"))
setReplaceMethod("universe", "IntegerRangesList",
                 function(x, value)
                 {
                   .Defunct(msg="The universe() setter is defunct.")
                 })

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### isNormal()
###
### Test the list elements of an IntegerRangesList object 'x' individually and
### return a vector of TRUE's or FALSE's parallel to 'x'. More precisely, is
### equivalent to 'sapply(x, FUN)', when FUN is 'isNormal'.
###

setMethod("isNormal", "IntegerRangesList",
    function(x, use.names=FALSE)
        vapply(x, isNormal, logical(1), USE.NAMES=use.names)
)

setMethod("isNormal", "SimpleIRangesList",
    function(x, use.names=FALSE)
        .Call2("SimpleIRangesList_isNormal", x, use.names,
               PACKAGE="IRanges")
)

setMethod("whichFirstNotNormal", "IntegerRangesList",
    function(x) unlist(lapply(x, whichFirstNotNormal))
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion from list-like object to IRangesList object
###

### Try to turn an arbitrary list-like object into an ordinary list of
### IRanges objects.
.as_list_of_IRanges <- function(from)
{
    if (is(from, "IntegerRanges")) {
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

.from_list_to_SimpleIRangesList <- function(from)
{
    from <- .as_list_of_IRanges(from)
    S4Vectors:::new_SimpleList_from_list("SimpleIRangesList", from)
}

setAs("list", "SimpleIRangesList", .from_list_to_SimpleIRangesList)
setAs("list", "IRangesList", .from_list_to_SimpleIRangesList)

### From List to IRangesList

.from_List_to_SimpleIRangesList <- function(from)
{
    S4Vectors:::new_SimpleList_from_list("SimpleIRangesList",
                                 .as_list_of_IRanges(from),
                                 metadata=metadata(from),
                                 mcols=mcols(from))
}

setAs("List", "SimpleIRangesList", .from_List_to_SimpleIRangesList)

### Automatic coercion methods from SimpleList, IntegerRangesList, or
### SimpleIntegerRangesList to SimpleIRangesList silently return a broken
### object (unfortunately these dummy automatic coercion methods don't bother
### to validate the object they return). So we overwrite them.
setAs("SimpleList", "SimpleIRangesList", .from_List_to_SimpleIRangesList)
setAs("IntegerRangesList", "SimpleIRangesList", .from_List_to_SimpleIRangesList)
setAs("SimpleIntegerRangesList", "SimpleIRangesList", .from_List_to_SimpleIRangesList)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion from list-like object to IntegerRangesList object
###

### From ordinary list to IntegerRangesList

.from_list_to_SimpleIntegerRangesList <- function(from)
{
    from <- .as_list_of_IRanges(from)
    S4Vectors:::new_SimpleList_from_list("SimpleIntegerRangesList", from)
}

setAs("list", "SimpleIntegerRangesList", .from_list_to_SimpleIntegerRangesList)
setAs("list", "IntegerRangesList", .from_list_to_SimpleIntegerRangesList)

### From List to IntegerRangesList

.from_List_to_SimpleIntegerRangesList <- function(from)
{
    S4Vectors:::new_SimpleList_from_list("SimpleIntegerRangesList",
                                 .as_list_of_IRanges(from),
                                 metadata=metadata(from),
                                 mcols=mcols(from))
}

setAs("List", "SimpleIntegerRangesList", .from_List_to_SimpleIntegerRangesList)

### You'd think that defining the above method would take care of the
### SimpleList-to-SimpleIntegerRangesList coercion but no such luck.
### Another dummy automatic coercion method is in the way, again. And it
### silently returns a broken object, again. So we overwrite it (again).
setAs("SimpleList", "SimpleIntegerRangesList",
    .from_List_to_SimpleIntegerRangesList
)

setAs("List", "IntegerRangesList",
    function(from)
    {
        if (!is(from, "IRangesList") && is(from, "SimpleList"))
            as(from, "SimpleIntegerRangesList")
        else
            as(from, "IRangesList")
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Constructor.
###

RangesList <- function(..., universe = NULL)
{
  msg <- c("The RangesList() constructor is deprecated. ",
           "Please coerce to IRangesList instead e.g. do ",
           "'as(list(x1, x2), \"IRangesList\")' instead of ",
           "'RangesList(x1, x2)'. Alternatively, you can use ",
           "the IRangesList() constructor e.g. ",
           "'IRangesList(x1, x2, compress=FALSE)'. See '?IRangesList' ",
           "for more information.")
  .Deprecated(msg=wmsg(msg))
  if (!is.null(universe)) {
     msg <- wmsg("The 'universe' argument of the RangesList() ",
                 "constructor function is defunct.")
    .Defunct(msg=msg)
  }
  ranges <- list(...)
  if (length(ranges) == 1 && is.list(ranges[[1L]]))
    ranges <- ranges[[1L]]
  if (!all(sapply(ranges, is, "IntegerRanges")))
    stop("all elements in '...' must be IntegerRanges objects")
  ans <- S4Vectors:::new_SimpleList_from_list("SimpleIntegerRangesList", ranges)
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
                    "constructor function is defunct.")
        .Defunct(msg=msg)
    }
    args <- list(...)
    if (length(args) == 2L &&
        setequal(names(args), c("start", "end")) &&
        !is(args[[1L]], "IntegerRanges") && !is(args[[2L]], "IntegerRanges"))
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
            if (is.list(x1) || (is(x1, "List") && !is(x1, "IntegerRanges")))
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

setMethod("show", "IntegerRangesList", function(object) showRangesList(object))

setMethod("showAsCell", "IntegerRangesList",
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

setMethod("unlist", "SimpleNormalIRangesList",
          function(x, recursive = TRUE, use.names = TRUE)
          {
            x <- S4Vectors:::new_SimpleList_from_list("SimpleIRangesList",
                                                      lapply(x, as, "IRanges"))
            callGeneric()
          })

### Coercion from IntegerRangesList to NormalIRangesList.

.from_RangesList_to_SimpleNormalIRangesList <- function(from)
{
    S4Vectors:::new_SimpleList_from_list("SimpleNormalIRangesList",
        lapply(from, as, "NormalIRanges"),
        mcols=mcols(from),
        metadata=metadata(from))
}

setAs("IntegerRangesList", "SimpleNormalIRangesList",
    .from_RangesList_to_SimpleNormalIRangesList
)

setAs("SimpleIRangesList", "SimpleNormalIRangesList",
    .from_RangesList_to_SimpleNormalIRangesList
)

setAs("LogicalList", "SimpleNormalIRangesList",
      function(from)
      S4Vectors:::new_SimpleList_from_list("SimpleNormalIRangesList",
                                           lapply(from, as, "NormalIRanges"),
                                           metadata = metadata(from),
                                           mcols = mcols(from)))

### Coercion from RleList to NormalIRangesList.

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

### Merges various IntegerRangesList objects into a single IntegerRangesList
### object. The merging is either by name (if all the IntegerRangesList
### objects have names), or by position (if any IntegerRangesList object is
### missing names). When merging by name, and in case of duplicated names
### within a given IntegerRangesList, the elements corresponding to the
### duplicated names are ignored.
### When merging by position, all the IntegerRangesList objects must have the
### same length.
### Note that the "range" method for IntegerRangesList objects expects "merge"
### to behave like this.  
.merge_IntegerRangesList <- function(...)
{
    args <- unname(list(...))
    if (length(args) == 0L)
        stop("nothing to merge")
    x <- args[[1L]]
    spaceList <- lapply(args, names)
    names <- spaces <- unique(do.call(c, spaceList))
    if (any(S4Vectors:::sapply_isNULL(spaceList))) {
        ## Merging by position.
        if (!all(unlist(lapply(args, length)) == length(x)))
            stop("if any IntegerRangesList objects to merge are missing ",
                 "names, all must have same length")
        names <- NULL
        spaces <- seq_len(length(x))
    }
    ranges <- lapply(spaces,
                     function(space) {
                       r <- lapply(args, `[[`, space)
                       do.call(c, S4Vectors:::delete_NULLs(r))
                     })
    names(ranges) <- names
    if (is(x, "CompressedList"))
      ans <- new_CompressedList_from_list(class(x), ranges)
    else
      ans <- S4Vectors:::new_SimpleList_from_list(class(x), ranges)
    ans
}

setMethod("merge", c("IntegerRangesList", "missing"),
    function(x, y, ...) .merge_IntegerRangesList(x, ...)
)

setMethod("merge", c("missing", "IntegerRangesList"),
    function(x, y, ...) .merge_IntegerRangesList(y, ...)
)

setMethod("merge", c("IntegerRangesList", "IntegerRangesList"),
    function(x, y, ...) .merge_IntegerRangesList(x, y, ...)
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "max" and "min" methods for NormalIRangesList objects.
###

setMethod("max", "SimpleNormalIRangesList",
          function(x, ..., na.rm)
          .Call2("SimpleNormalIRangesList_max", x, PACKAGE="IRanges"))

setMethod("min", "SimpleNormalIRangesList",
          function(x, ..., na.rm)
          .Call2("SimpleNormalIRangesList_min", x, PACKAGE="IRanges"))

