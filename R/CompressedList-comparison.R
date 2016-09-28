### =========================================================================
### Comparing and ordering CompressedList objects
### -------------------------------------------------------------------------
###
### Overwrite methods defined in S4Vectors/R/List-comparison.R for List
### objects with optimized methods for CompressedList objects.
###


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Element-wise (aka "parallel") comparison of 2 List objects.
###
### TODO: Add optimized "==" and "<=" methods for CompressedList objects.
###

setMethod("!", "CompressedList",
    function(x) relist(!unlist(x, use.names=FALSE), x)
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### match()
###

### The first match method catches CompressedList,list; 'table' is atomic
setMethod("match", c("CompressedList", "vector"),
          function(x, table, nomatch = NA_integer_, incomparables = NULL, ...)
          {
            m <- match(x@unlistData, table, nomatch=nomatch,
                       incomparables=incomparables, ...)
            relist(m, x)
          })


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### duplicated() & unique()
###

.duplicated.CompressedList <- function(x, incomparables=FALSE,
                                       fromLast=FALSE, nmax=NA)
{
    if (!identical(incomparables, FALSE))
        stop("\"duplicated\" method for CompressedList objects ",
             "does not support the 'incomparables' argument")
    x_unlistData <- x@unlistData
    sm <- match(x_unlistData, x_unlistData)  # doesn't work on an Rle
    x_group <- rep.int(seq_along(x), elementNROWS(x))
    ans_unlistData <- duplicatedIntegerPairs(x_group, sm, fromLast=fromLast)
    relist(ans_unlistData, x)
}
setMethod("duplicated", "CompressedList", .duplicated.CompressedList)

.unique.CompressedList <- function(x, ...)
{
    is_dup <- duplicated(x, ...)
    x_unlistData <- x@unlistData
    keep_idx <- which(!is_dup@unlistData)
    ans_unlistData <- x_unlistData[keep_idx]
    x_group <- rep.int(seq_along(x), elementNROWS(x))
    ans_group <- x_group[keep_idx]
    ans_partitioning <- PartitioningByEnd(ans_group, NG=length(x),
                                          names=names(x))
    relist(ans_unlistData, ans_partitioning)
}
setMethod("unique", "CompressedList", .unique.CompressedList)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### %in%
###

### The "%in%" method for Vector objects calls is.na() internally.
setMethod("is.na", "CompressedList",
    function(x) relist(is.na(unlist(x, use.names=FALSE)), x)
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### order() and related methods.
###
### TODO: Add optimized methods for CompressedList objects.
###

