### =========================================================================
### CompressedRangesList objects
### -------------------------------------------------------------------------
###

setClass("CompressedRangesList",
    contains=c("RangesList", "CompressedList"),
    representation("VIRTUAL")
)

setClass("CompressedPosList",
    contains=c("PosList", "CompressedRangesList"),
    representation("VIRTUAL")
)

setClass("CompressedFWRangesList",
    contains=c("FWRangesList", "CompressedRangesList"),
    representation("VIRTUAL")
)

setClass("CompressedIntegerRangesList",
    contains=c("IntegerRangesList", "CompressedRangesList"),
    representation("VIRTUAL")
)

setClass("CompressedIRangesList",
    contains=c("IRangesList", "CompressedIntegerRangesList"),
    prototype=prototype(unlistData=new("IRanges"))
)

### CompressedNormalIRangesList cannot hold NormalIRanges as its elements,
### due to the compression concatenating everything into a single
### NormalIRanges (which could easily become non-normal). So just have it
### hold IRanges, instead.
setClass("CompressedNormalIRangesList",
    contains=c("NormalIRangesList", "CompressedIRangesList"),
    prototype=prototype(
        elementType="IRanges",
        unlistData=new("IRanges")
    )
)

setClass("CompressedIPosList",
    contains=c("IPosList", "CompressedPosList", "CompressedIntegerRangesList"),
    prototype=prototype(unlistData=new("IPos"))
)

setClass("CompressedIFWRangesList",
    contains=c("IFWRangesList", "CompressedFWRangesList", "CompressedIntegerRangesList"),
    prototype=prototype(unlistData=new("IFWRanges"))
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Methods for CompressedRangesList objects
###

setMethod("start", "CompressedRangesList",
    function(x) relist(start(unlist(x, use.names=FALSE)), x)
)

setMethod("end", "CompressedRangesList",
    function(x) relist(end(unlist(x, use.names=FALSE)), x)
)

setMethod("width", "CompressedRangesList",
    function(x) relist(width(unlist(x, use.names=FALSE)), x)
)

setMethod("pos", "CompressedPosList",
    function(x) relist(pos(unlist(x, use.names=FALSE)), x)
)

setMethod("fixed_width", "CompressedFWRangesList",
          function(x) fixed_width(unlist(x, use.names=FALSE))
)

setMethod(".replaceSEW", "CompressedRangesList",
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


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Methods for CompressedIRangesList objects
###

setMethod("isNormal", "CompressedIRangesList",
    function(x, use.names=FALSE)
        .Call2("CompressedIRangesList_isNormal", x, use.names,
               PACKAGE="IRanges")
)

setMethod("summary", "CompressedIRangesList",
    function(object)
        .Call2("CompressedIRangesList_summary", object,
               PACKAGE="IRanges")
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion from list-like object to IRangesList object
###

.from_list_to_CompressedIRangesList <- function(from)
{
    from <- .as_list_of_IRanges(from)
    new_CompressedList_from_list("CompressedIRangesList", from)
}

setAs("list", "CompressedIRangesList", .from_list_to_CompressedIRangesList)

### From List to IRangesList

.from_List_to_CompressedIRangesList <- function(from)
{
    new_CompressedList_from_list("CompressedIRangesList",
                                 .as_list_of_IRanges(from),
                                 metadata=metadata(from),
                                 mcols=mcols(from, use.names=FALSE))
}

### IntegerRanges objects are List objects so this case is already covered
### by the .from_List_to_CompressedIRangesList() helper above. However, we
### can implement it much more efficiently.
.from_IntegerRanges_to_CompressedIRangesList <- function(from)
{
    if (!is(from, "IRanges"))
        from <- as(from, "IRanges", strict=FALSE)
    ans_partitioning <- PartitioningByEnd(seq_along(from), names=names(from))
    names(from) <- NULL
    ans_mcols <- mcols(from, use.names=FALSE)
    mcols(from) <- NULL
    ans <- relist(from, ans_partitioning)
    mcols(ans) <- ans_mcols
    ans
}

setAs("List", "CompressedIRangesList", .from_List_to_CompressedIRangesList)
setAs("IntegerRanges", "CompressedIRangesList",
    .from_IntegerRanges_to_CompressedIRangesList
)

setAs("List", "IRangesList",
    function(from)
    {
        if (is(from, "CompressedList") || is(from, "IntegerRanges"))
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
        mcols(to) <- mcols(from, use.names=FALSE)
        to
      })


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Methods for CompressedNormalIRangesList objects
###

setMethod("getListElement", "CompressedNormalIRangesList",
    function(x, i, exact=TRUE) newNormalIRangesFromIRanges(callNextMethod())
)

.max_CompressedNormalIRangesList <- function(x, use.names)
{
    if (!is(x, "CompressedNormalIRangesList"))
        stop("'x' must be a CompressedNormalIRangesList object")
    use.names <- S4Vectors:::normargUseNames(use.names)
    .Call2("CompressedNormalIRangesList_max", x, use.names, PACKAGE="IRanges")
}

setMethod("max", "CompressedNormalIRangesList",
          function(x, ..., na.rm) .max_CompressedNormalIRangesList(x, TRUE))

.min_CompressedNormalIRangesList <- function(x, use.names)
{
    if (!is(x, "CompressedNormalIRangesList"))
        stop("'x' must be a CompressedNormalIRangesList object")
    use.names <- S4Vectors:::normargUseNames(use.names)
    .Call2("CompressedNormalIRangesList_min", x, use.names, PACKAGE="IRanges")
}

setMethod("min", "CompressedNormalIRangesList",
          function(x, ..., na.rm) .min_CompressedNormalIRangesList(x, TRUE))

### Coercion from list to CompressedNormalIRangesList.
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

### Coercion from IntegerRangesList to NormalIRangesList.

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

setAs("IntegerRangesList", "CompressedNormalIRangesList",
    function(from)
    {
        as(as(from, "CompressedIRangesList", strict=FALSE),
           "CompressedNormalIRangesList")
    }
)

setAs("IntegerRangesList", "NormalIRangesList",
    function(from)
    {
        if (is(from, "SimpleIntegerRangesList"))
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
                                   mcols = mcols(from, use.names=FALSE)))

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
                                     mcols = mcols(from, use.names=FALSE))
      })

### Coercion from IntegerRanges to IPosList.

.from_IntegerRanges_to_CompressedIPosList <- function(from)
{
    from <- as(from, "IRanges")
    ans <- relist(IPos(from), from)
    metadata(ans) <- metadata(from)
    mcols(ans) <- mcols(from, use.names=FALSE)
    ans
}

setAs("IntegerRanges", "CompressedIPosList",
    .from_IntegerRanges_to_CompressedIPosList
)

setAs("IntegerRanges", "IPosList",
    .from_IntegerRanges_to_CompressedIPosList
)

setAs("IRanges", "IPosList",
    .from_IntegerRanges_to_CompressedIPosList
)

### Coercion from IntegerRanges to IFWRangesList.

.from_IntegerRanges_to_CompressedIFWRangesList <- function(from)
{
  from <- as(from, "IRanges")
  ans <- relist(IFWRanges(from), from)
  metadata(ans) <- metadata(from)
  mcols(ans) <- mcols(from, use.names=FALSE)
  ans
}

setAs("IntegerRanges", "CompressedIFWRangesList",
      .from_IntegerRanges_to_CompressedIFWRangesList
)

setAs("IntegerRanges", "IFWRangesList",
      .from_IntegerRanges_to_CompressedIFWRangesList
)

setAs("IRanges", "IFWRangesList",
      .from_IntegerRanges_to_CompressedIFWRangesList
)

