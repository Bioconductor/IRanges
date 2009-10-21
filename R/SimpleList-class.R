### =========================================================================
### SimpleList objects
### -------------------------------------------------------------------------

setClass("SimpleList",
         contains="Sequence",
         representation(
                        listData="list"
                        )
         )

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Accessor methods.
###

### A SimpleList object is considered empty iff all its elements are empty.
setMethod("isEmpty", "SimpleList", function(x) all(elementLengths(x) == 0L))

setMethod("length", "SimpleList", function(x) length(as.list(x)))

setMethod("names", "SimpleList", function(x) names(as.list(x)))

setReplaceMethod("names", "SimpleList",
                 function(x, value) {
                     names(x@listData) <- value
                     x
                 })

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Constructor.
###

SimpleList <- function(...) {
    new("SimpleList", listData = list(...))
}

newSimpleList <- function(listClass, listData, ...) {
    if (!is.list(listData))
        stop("'listData' must be a list object")
    if (!extends(listClass, "SimpleList"))
        stop("cannot create a ", listClass, " as a 'SimpleList'")
    elementTypeData <- elementType(new(listClass))
    if (!all(sapply(listData,
                    function(x) extends(class(x), elementTypeData))))
        stop("all elements in 'listData' must be ", elementTypeData, " objects")
    new2(listClass, listData = listData, ..., check=FALSE)
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Validity.
###

.valid.SimpleList.listData <- function(x)
{
    elementTypeX <- elementType(x)
    if (!all(sapply(as.list(x),
                    function(xi) extends(class(xi), elementTypeX))))
        return(paste("the 'listData' slot must be a list containing",
                     elementTypeX, "objects"))
    NULL
}
.valid.SimpleList <- function(x)
{
    c(.valid.SimpleList.listData(x))
}
setValidity2("SimpleList", .valid.SimpleList)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Subsetting.
###

setMethod("[[", "SimpleList",
          function(x, i, j, ...) {
              dotArgs <- list(...)
              if (length(dotArgs) > 0)
                  dotArgs <- dotArgs[names(dotArgs) != "exact"]
              if (!missing(j) || length(dotArgs) > 0)
                  stop("incorrect number of subscripts")
              index <-
                try(checkAndTranslateDbleBracketSubscript(x, i), silent = TRUE)
              if (inherits(index, "try-error")) {
                  if (length(i) == 1 && (is.na(i) || is.character(i)))
                      ans <- NULL
                  else
                      stop(index)
              } else {
                  ans <- as.list(x)[[index]]
              }
              ans
          })

setReplaceMethod("[[", "SimpleList",
                 function(x, i, j,..., value)
                 {
                     if (!missing(j) || length(list(...)) > 0)
                         stop("invalid replacement")
                     x@listData[[i]] <- value
                     x
                 })

setReplaceMethod("$", "SimpleList",
                 function(x, name, value) {
                     x[[name]] <- value
                     x
                 })

### Supported 'i' types: numeric, character, logical, NULL and missing.
setMethod("[", "SimpleList",
          function(x, i, j, ..., drop)
          {
              if (!missing(j) || length(list(...)) > 0)
                  stop("invalid subsetting")
              if (!missing(i)) {
                  if (is(i, "RangesList") || is(i, "RleList") ||
                      is(i, "LogicalList") || is(i, "IntegerList")) {
                      x <- seqselect(x, i)
                  } else {
                      slot(x, "listData", check=FALSE) <- as.list(x)[i]
                      x <- .bracket.Sequence(x, i)
                  }
              }
              x
          })

setMethod("seqselect", "SimpleList",
          function(x, start=NULL, end=NULL, width=NULL)
          {
              if (!is.null(start) && is.null(end) && is.null(width) &&
                  (length(x) > 0)) {
                  if (length(x) != length(start))
                      stop("'length(start)' must equal 'length(x)' when ",
                           "'end' and 'width' are NULL")
                  if (is.list(start)) {
                      if (is.logical(start[[1]]))
                          start <- LogicalList(start)
                      else if (is.numeric(start[[1]]))
                          start <- IntegerList(start)
                  } else if (is(start, "RleList")) {
                      start <- IRangesList(start)
                  }
                  indices <- structure(seq_len(length(x)), names = names(x))
                  if (is(start, "RangesList")) {
                      listData <-
                        lapply(indices, function(i)
                               seqselect(x@listData[[i]], start[[i]]))
                  } else if (is(start, "LogicalList") ||
                             is(start, "IntegerList")) {
                      if (length(dim(x@listData[[1]])) < 2)
                          listData <-
                            lapply(indices,
                                   function(i) x@listData[[i]][start[[i]]])
                      else
                          listData <-
                            lapply(indices, function(i)
                                   x@listData[[i]][start[[i]], , drop = FALSE])
                  } else {
                      stop("unrecognized 'start' type")
                  }
                  slot(x, "listData", check=FALSE) <- listData
              } else {
                  x <- callNextMethod()
              }
              x
          })

setReplaceMethod("seqselect", "SimpleList",
                 function(x, start = NULL, end = NULL, width = NULL, value)
                 {
                     if (!is.null(start) && is.null(end) && is.null(width) &&
                         (length(x) > 0)) {
                         if (length(x) != length(start))
                             stop("'length(start)' must equal 'length(x)' when ",
                                  "'end' and 'width' are NULL")
                         if (is.list(start)) {
                             if (is.logical(start[[1]]))
                                 start <- LogicalList(start)
                             else if (is.numeric(start[[1]]))
                                 start <- IntegerList(start)
                         } else if (is(start, "RleList")) {
                             start <- IRangesList(start)
                         } else if (is(start, "IntegerList")) {
                             newstart <-
                               LogicalList(lapply(elementLengths(x), rep,
                                                  x = FALSE))
                             for (i in seq_len(length(newstart)))
                                 newstart[[i]][start[[i]]] <- TRUE
                             start <- newstart
                         }
                         indices <-
                           structure(seq_len(length(x)), names = names(x))
                         if (is(start, "RangesList") ||
                             is(start, "LogicalList")) {
                             if (!is(value, "SimpleList") &&
                                 !is(value, "CompressedList") &&
                                 !is.list(value))
                                 value <- list(value)
                             li <- length(indices)
                             lv <- length(value)
                             if (li != lv) {
                                 if ((li == 0) || (li %% lv != 0))
                                     stop(paste(lv, "elements in value to replace",
                                                li, "elements"))
                                 else
                                     value <- rep(value, length.out = li)
                             }
                             x@listData <-
                               lapply(indices, function(i) {
                                          y <- x@listData[[i]]
                                          seqselect(y, start[[i]]) <- value[[i]]
                                          y
                                      })
                         } else {
                             stop("unrecognized 'start' type")
                         }
                     } else {
                         x <- callNextMethod()
                     }
                     x
                 })

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Combining and splitting.
###

## NOTE: while the 'c' function does not have an 'x', the generic does
## c() is a primitive, so 'x' can be missing; dispatch is by position,
## although sometimes this does not work so well, so it's best to keep
## names off the parameters whenever feasible.

setMethod("c", "SimpleList",
          function(x, ..., recursive = FALSE) {
              slot(x, "listData") <-
                do.call("c", lapply(list(x, ...), as.list))
              .c.Sequence(x, ...)
          })

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Looping.
###

setMethod("lapply", "SimpleList",
          function(X, FUN, ...)
              lapply(as.list(X), FUN = FUN, ...))

setMethod("aggregate", "SimpleList",
          function(x, by, FUN, start = NULL, end = NULL, width = NULL,
                   frequency = NULL, delta = NULL, ..., simplify = TRUE)
          {
              if (!missing(by) && is(by, "RangesList")) {
                  if (length(x) != length(by))
                      stop("for Ranges 'by', 'length(x) != length(by)'")
                  ans <-
                    newSimpleList("SimpleList",
                                  lapply(structure(seq_len(length(x)),
                                                   names = names(x)),
                                         function(i)
                                         aggregate(x[[i]], by = by[[i]],
                                                   FUN = FUN,
                                                   frequency = frequency,
                                                   delta = delta, ...,
                                                   simplify = simplify)),
                                  metadata = metadata(x),
                                  elementMetadata = elementMetadata(x))
              } else {
                  ans <- callNextMethod()
              }
              ans
          })

setMethod("endoapply", "SimpleList",
          function(X, FUN, ...) {
              listData <- lapply(X, FUN = FUN, ...)
              elementTypeX <- elementType(X)
              if (!all(sapply(listData,
                              function(Xi) extends(class(Xi), elementTypeX))))
                  stop("all results must be of class '", elementTypeX, "'")
              slot(X, "listData", check=FALSE) <- listData
              X
          })

setMethod("mendoapply", "SimpleList",
          function(FUN, ..., MoreArgs = NULL) {
              X <- list(...)[[1]]
              elementTypeX <- elementType(X)
              listData <- mapply(FUN = FUN, ..., MoreArgs = MoreArgs)
              if (!all(sapply(listData,
                              function(Xi) extends(class(Xi), elementTypeX))))
                  stop("all results must be of class '", elementTypeX, "'")
              slot(X, "listData", check=FALSE) <- listData
              X
          })

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion.
###

setMethod("as.list", "SimpleList",
          function(x, use.names = TRUE) {
              ans <- x@listData
              if (!use.names)
                  names(ans) <- NULL
              ans
          })

setMethod("unlist", "SimpleList",
          function(x, recursive = TRUE, use.names = TRUE) {
              if (!missing(recursive))
                  warning("'recursive' argument currently ignored")
              if (length(x) == 0)
                  ans <- NULL
              else {
                  ans <- .compress.list(as.list(x))
                  if (length(dim(ans)) < 2 && use.names) {
                      nms <- rep(names(x), elementLengths(x))
                      if (!is.null(nms) && !is.null(names(ans)))
                          nms <- paste(nms, names(ans), sep = ".")
                      else if (is.null(nms))
                          nms <- names(ans)
                      names(ans) <- nms
                  } else {
                      if (!use.names)
                          rownames(ans) <- NULL
                  }
              }
              ans
          })

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "show" method.
###

setMethod("show", "SimpleList",
          function(object)
          {
              lo <- length(object)
              cat(class(object), " of length ", lo, "\n", sep = "")
              if (!is.null(names(object)))
                  cat(labeledLine("names", names(object)))
          })
