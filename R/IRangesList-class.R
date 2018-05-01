### =========================================================================
### IRangesList objects
### -------------------------------------------------------------------------


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
### isNormal()
###

setMethod("isNormal", "SimpleIRangesList",
    function(x, use.names=FALSE)
        .Call2("SimpleIRangesList_isNormal", x, use.names,
               PACKAGE="IRanges")
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Constructor.
###

IRangesList <- function(..., compress=TRUE)
{
    if (!isTRUEorFALSE(compress))
        stop("'compress' must be TRUE or FALSE")
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
    ans
}


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

.from_IntegerRangesList_to_SimpleNormalIRangesList <- function(from)
{
    S4Vectors:::new_SimpleList_from_list("SimpleNormalIRangesList",
        lapply(from, as, "NormalIRanges"),
        mcols=mcols(from),
        metadata=metadata(from))
}

setAs("IntegerRangesList", "SimpleNormalIRangesList",
    .from_IntegerRangesList_to_SimpleNormalIRangesList
)

setAs("SimpleIRangesList", "SimpleNormalIRangesList",
    .from_IntegerRangesList_to_SimpleNormalIRangesList
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
### The "max" and "min" methods for NormalIRangesList objects.
###

setMethod("max", "SimpleNormalIRangesList",
          function(x, ..., na.rm)
          .Call2("SimpleNormalIRangesList_max", x, PACKAGE="IRanges"))

setMethod("min", "SimpleNormalIRangesList",
          function(x, ..., na.rm)
          .Call2("SimpleNormalIRangesList_min", x, PACKAGE="IRanges"))

