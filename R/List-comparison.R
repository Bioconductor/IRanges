### =========================================================================
### Comparing and ordering List objects
### -------------------------------------------------------------------------
###


### Method signatures for binary comparison operators.
.OP2_SIGNATURES <- list(
    c("List", "List"),
    c("List", "list"),
    c("list", "List")
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### compareRecursively()
###
### NOT exported!
###
### By default, List objects compare recursively. Exceptions to the rule
### (e.g. Ranges, XString, etc...) must define a "compareRecursively" method
### that returns FALSE.
###

setGeneric("compareRecursively",
    function(x) standardGeneric("compareRecursively")
)

setMethod("compareRecursively", "List", function(x) TRUE)
setMethod("compareRecursively", "list", function(x) TRUE)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### .op1_apply() and .op2_apply() internal helpers
###

### Apply a unary operator.
.op1_apply <- function(OP1, x, ..., ANS_CONSTRUCTOR)
{
    comp_rec_x <- compareRecursively(x)
    if (!comp_rec_x) {
        OP1_Vector_method <- selectMethod(OP1, "Vector")
        return(OP1_Vector_method(x, ...))
    }
    compress_ans <- !is(x, "SimpleList")
    ANS_CONSTRUCTOR(lapply(x, OP1, ...), compress=compress_ans)
}

### Apply a binary operator.
.op2_apply <- function(OP2, x, y, ..., ANS_CONSTRUCTOR)
{
    comp_rec_x <- compareRecursively(x)
    comp_rec_y <- compareRecursively(y)
    if (!(comp_rec_x || comp_rec_y)) {
        OP2_Vector_method <- selectMethod(OP2, c("Vector", "Vector"))
        return(OP2_Vector_method(x, y, ...))
    }
    if (!comp_rec_x)
        x <- list(x)
    if (!comp_rec_y)
        y <- list(y)
    compress_ans <- !((is(x, "SimpleList") || is.list(x)) &&
                      (is(y, "SimpleList") || is.list(y)))
    x_len <- length(x)
    y_len <- length(y)
    if (x_len == 0L || y_len == 0L) {
        ans <- ANS_CONSTRUCTOR(compress=compress_ans)
    } else {
        ans <- ANS_CONSTRUCTOR(mapply(OP2, x, y, MoreArgs=list(...),
                                      SIMPLIFY=FALSE, USE.NAMES=FALSE),
                               compress=compress_ans)
    }
    ## 'ans' is guaranteed to have the length of 'x' or 'y'.
    x_names <- names(x)
    y_names <- names(y)
    if (!(is.null(x_names) && is.null(y_names))) {
        ans_len <- length(ans)
        if (x_len != y_len) {
            if (x_len == ans_len) {
                ans_names <- x_names
            } else {
                ans_names <- y_names
            }
        } else {
            if (is.null(x_names)) {
                ans_names <- y_names
            } else {
                ans_names <- x_names
            }
        }
        names(ans) <- ans_names
    }
    ans
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Element-wise (aka "parallel") comparison of 2 List objects.
###

setMethods("compare", .OP2_SIGNATURES,
    function(x, y) .op2_apply(compare, x, y, ANS_CONSTRUCTOR=IntegerList)
)

### TODO: Add fast "==" and "<=" methods for CompressedList objects.
setMethods("==", .OP2_SIGNATURES,
    function(e1, e2) .op2_apply(`==`, e1, e2, ANS_CONSTRUCTOR=LogicalList)
)

setMethods("<=", .OP2_SIGNATURES,
    function(e1, e2) .op2_apply(`<=`, e1, e2, ANS_CONSTRUCTOR=LogicalList)
)

### The remaining comparison binary operators (!=, >=, <, >) will work
### out-of-the-box on List objects thanks to the "!" methods below and to the
### methods for Vector objects.
setMethod("!", "List",
    function(x)
    {
        if (is(x, "RleList")) {
            ANS_CONSTRUCTOR <- RleList
        } else {
            ANS_CONSTRUCTOR <- LogicalList
        }
        .op1_apply(`!`, x, ANS_CONSTRUCTOR=ANS_CONSTRUCTOR)
    }
)

setMethod("!", "CompressedList",
    function(x) relist(!unlist(x, use.names=FALSE), x)
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### match()
###

setMethods("match", c(.OP2_SIGNATURES, list(c("CompressedList", "list"))),
    function(x, table, nomatch=NA_integer_, incomparables=NULL, ...)
    {
        if (is(x, "RleList")) {
            ANS_CONSTRUCTOR <- RleList
        } else {
            ANS_CONSTRUCTOR <- IntegerList
        }
        .op2_apply(match, x, table,
                   nomatch=nomatch, incomparables=incomparables, ...,
                   ANS_CONSTRUCTOR=ANS_CONSTRUCTOR)
    }
)

### 2 of the 3 "match" methods defined above have signatures List,list and
### List,List and therefore are more specific than the 2 methods below.
### So in the methods below 'table' is guaranteed to be a vector that is not
### a list or a Vector that is not a List.
setMethods("match", list(c("List", "vector"), c("List", "Vector")),
    function(x, table, nomatch=NA_integer_, incomparables=NULL, ...)
    {
        match(x, list(table),
              nomatch=nomatch, incomparables=incomparables, ...)
    }
)

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

### S3/S4 combo for duplicated.List
duplicated.List <- function(x, incomparables=FALSE,
                               fromLast=FALSE, ...)
{
    .op1_apply(duplicated, x,
               incomparables=incomparables, fromLast=fromLast, ...,
               ANS_CONSTRUCTOR=LogicalList)
}
setMethod("duplicated", "List", duplicated.List)

### S3/S4 combo for duplicated.CompressedList
.duplicated.CompressedList <- function(x, incomparables=FALSE,
                                          fromLast=FALSE)
{
    if (!identical(incomparables, FALSE))
        stop("\"duplicated\" method for CompressedList objects ",
             "does not support the 'incomparables' argument")
    x_unlistData <- x@unlistData
    sm <- match(x_unlistData, x_unlistData)  # doesn't work on an Rle
    x_group <- rep.int(seq_along(x), elementLengths(x))
    ans_unlistData <- S4Vectors:::duplicatedIntegerPairs(x_group, sm,
                                                         fromLast=fromLast)
    relist(ans_unlistData, x)
}
duplicated.CompressedList <- function(x, incomparables=FALSE,
                                         fromLast=FALSE, ...)
    .duplicated.CompressedList(x, incomparables=incomparables,
                                  fromLast=fromLast, ...)
setMethod("duplicated", "CompressedList", duplicated.CompressedList)

### S3/S4 combo for unique.List
unique.List <- function(x, incomparables=FALSE, ...)
{
    if (!compareRecursively(x))
        return(unique.Vector(x, incomparables=incomparables, ...))
    i <- !duplicated(x, incomparables=incomparables, ...)  # LogicalList
    x[i]
}
setMethod("unique", "List", unique.List)

### S3/S4 combo for unique.SimpleList
unique.SimpleList <- function(x, incomparables=FALSE, ...) {
    as(lapply(x, unique, incomparables=incomparables, ...), class(x))
}
setMethod("unique", "SimpleList", unique.SimpleList)

### S3/S4 combo for unique.CompressedList
.unique.CompressedList <- function(x, incomparables=FALSE,
                                      fromLast=FALSE)
{
    if (!identical(incomparables, FALSE))
        stop("\"unique\" method for CompressedList objects ",
             "does not support the 'incomparables' argument")
    is_dup <- duplicated(x, incomparables=incomparables, fromLast=fromLast)
    x_unlistData <- x@unlistData
    keep_idx <- which(!is_dup@unlistData)
    ans_unlistData <- x_unlistData[keep_idx]
    x_group <- rep.int(seq_along(x), elementLengths(x))
    ans_group <- x_group[keep_idx]
    ans_partitioning <- PartitioningByEnd(ans_group, NG=length(x),
                                          names=names(x))
    relist(ans_unlistData, ans_partitioning)
}
unique.CompressedList <- function(x, incomparables=FALSE, ...)
    .unique.CompressedList(x, incomparables=incomparables, ...)
setMethod("unique", "CompressedList", unique.CompressedList)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### %in%()
###
### %in% will work out-of-the-box on List objects thanks to the "is.na"
### methods below and to the method for Vector objects.
###

setMethod("is.na", "List",
    function(x)
    {
        if (is(x, "RleList")) {
            ANS_CONSTRUCTOR <- RleList
        } else {
            ANS_CONSTRUCTOR <- LogicalList
        }
        .op1_apply(is.na, x, ANS_CONSTRUCTOR=ANS_CONSTRUCTOR)
    }
)

setMethod("is.na", "CompressedList",
    function(x) relist(is.na(unlist(x, use.names=FALSE)), x)
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### order() and related methods.
###

setMethod("order", "List",
    function(..., na.last=TRUE, decreasing=FALSE)
    {
        args <- list(...)
        if (length(args) != 1L)
            stop("\"order\" method for List objects ",
                 "can only take one input object")
        .op1_apply(order, args[[1L]],
                   na.last=na.last, decreasing=decreasing,
                   ANS_CONSTRUCTOR=IntegerList)
    }
)

### S3/S4 combo for sort.List
.sort.List <- function(x, decreasing=FALSE, na.last=NA)
{
    if (!compareRecursively(x))
        return(sort.Vector(x, decreasing=decreasing, na.last=na.last))
    i <- order(x, na.last=na.last, decreasing=decreasing)  # IntegerList
    x[i]
}
sort.List <- function(x, decreasing=FALSE, ...)
    .sort.List(x, decreasing=decreasing, ...)
setMethod("sort", "List", sort.List)

setMethod("rank", "List",
    function(x, na.last=TRUE,
             ties.method=c("average", "first", "random", "max", "min"))
    {
        .op1_apply(rank, x,
                   na.last=na.last, ties.method=ties.method,
                   ANS_CONSTRUCTOR=IntegerList)
    }
)

