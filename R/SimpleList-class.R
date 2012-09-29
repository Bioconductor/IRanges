### =========================================================================
### SimpleList objects
### -------------------------------------------------------------------------

setClass("SimpleList",
         contains="List",
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
    list <- list(...)
    if (length(list) == 1 && is.list(list[[1L]]))
        list <- list[[1L]]
    new("SimpleList", listData = list)
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
                      lx <- length(x)
                      if (is.numeric(i)) {
                          if (!is.integer(i))
                              i <- as.integer(i)
                          if (anyMissingOrOutside(i, upper = lx))
                              stop("subscript contains NAs or out of bounds indices")
                          if (anyMissingOrOutside(i, 0L, lx)) {
                              if (anyMissingOrOutside(i, upper = 0L))
                                  stop("negative and positive indices cannot be mixed")
                              i <- seq_len(lx)[i]
                          }
                      } else if (is.logical(i)) {
                          if (anyMissing(i))
                              stop("subscript contains NAs")
                          li <- length(i)
                          if (li > lx)
                              stop("subscript out of bounds")
                          if (li < lx)
                              i <- rep(i, length.out = lx)
                          i <- which(i)
                      } else if (is.character(i) || is.factor(i)) {
                          nms <- names(x)
                          if (is.null(nms))
                              stop("cannot subset by character when names are NULL")
                          i <- match(i, nms)
                          if (anyMissing(i))
                              stop("mismatching names")
                      } else if (is.null(i)) {
                          i <- integer(0)
                      } else {
                          stop("invalid subscript type")
                      }
                      x <-
                        initialize(x,
                                   elementMetadata =
                                   seqselect(x@elementMetadata, i),
                                   listData = as.list(x)[i])
                  }
              }
              x
          })

setReplaceMethod("[", "SimpleList",
    function(x, i, j, ..., value)
    {
        if (!missing(j) || length(list(...)) > 0L)
            stop("invalid subsetting")
        if (missing(i))
            i <- seq_len(length(x))
        else if (is.list(i) || is(i, "List")) 
            return(subsetListByList_replace(x, i, value))
        else
            i <- normalizeSingleBracketSubscript(i, x)
        li <- length(i)
        if (li == 0L) {
            ## Surprisingly, in that case, `[<-` on standard vectors does not
            ## even look at 'value'. So neither do we...
            return(x)
        }
        lv <- length(value)
        if (lv == 0L)
            stop("replacement has length zero")
        if (!is(value, class(x)))
            value <- mk_singleBracketReplacementValue(x, value)
        if (li != lv) {
            if (li %% lv != 0L)
                warning("number of items to replace is not a multiple ",
                        "of replacement length")
            ## Assuming that rep() works on 'value' and also replicates its
            ## names.
            value <- rep(value, length.out = li)
        }
        x@listData[i] <- value@listData
        x
    }
)

setMethod("seqselect", "SimpleList",
          function(x, start=NULL, end=NULL, width=NULL)
          {
              lx <- length(x)
              if ((lx > 0) && is.null(end) && is.null(width) &&
                  !is.null(start) && !is(start, "Ranges")) {
                  if (lx != length(start))
                      stop("'length(start)' must equal 'length(x)' when ",
                           "'end' and 'width' are NULL")
                  if (is.list(start)) {
                      if (is.logical(start[[1L]]))
                          start <- LogicalList(start)
                      else if (is.numeric(start[[1L]]))
                          start <- IntegerList(start)
                  } else if (is(start, "RleList")) {
                      start <- IRangesList(start)
                  }
                  indices <- structure(seq_len(lx), names = names(x))
                  if (is(start, "RangesList")) {
                      listData <-
                        lapply(indices, function(i)
                               seqselect(x@listData[[i]], start[[i]]))
                  } else if (is(start, "LogicalList") ||
                             is(start, "IntegerList")) {
                      listData <-
                        lapply(indices, function(i)
                               extractROWS(x@listData[[i]], start[[i]]))
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
                     lx <- length(x)
                     if ((lx > 0) && is.null(end) && is.null(width) &&
                         !is.null(start) && !is(start, "Ranges")) {
                         if (lx != length(start))
                             stop("'length(start)' must equal 'length(x)' when ",
                                  "'end' and 'width' are NULL")
                         if (is.list(start)) {
                             if (is.logical(start[[1L]]))
                                 start <- LogicalList(start)
                             else if (is.numeric(start[[1L]]))
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
                         indices <- structure(seq_len(lx), names = names(x))
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
setMethod("[[", "SimpleList",
          function(x, i, j, ...) {
              dotArgs <- list(...)
              if (length(dotArgs) > 0)
                  dotArgs <- dotArgs[names(dotArgs) != "exact"]
              if (!missing(j) || length(dotArgs) > 0)
                  stop("incorrect number of subscripts")
              ## H.P.: Do we really need to support subsetting by NA? Other
              ## "[[" methods for other List subtypes don't support it.
              if (is.vector(i) && length(i) == 1L && is.na(i))
                  return(NULL)
              index <- checkAndTranslateDbleBracketSubscript(x, i,
                           error.if.nomatch=FALSE)
              if (is.na(index))
                  return(NULL)
              as.list(x)[[index]]
          })

setReplaceMethod("[[", "SimpleList",
                 function(x, i, j, ..., value)
                 {
                     if (!missing(j) || length(list(...)) > 0)
                         stop("invalid replacement")
                     origLen <- length(x)
                     x@listData[[i]] <- value
                     if (origLen < length(x))
                       x <- rbindRowOfNAsToMetadatacols(x)
                     x
                 })

setReplaceMethod("$", "SimpleList",
                 function(x, name, value) {
                     x[[name]] <- value
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
                do.call(c, lapply(unname(list(x, ...)), as.list))
              .c.Vector(x, ...)
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
                  result <-
                    lapply(structure(seq_len(length(x)), names = names(x)),
                           function(i)
                               aggregate(x[[i]], by = by[[i]], FUN = FUN,
                                         frequency = frequency, delta = delta,
                                         ..., simplify = simplify))
                  ans <- try(SimpleAtomicList(result), silent = TRUE)
                  if (inherits(ans, "try-error"))
                      ans <- newList("SimpleList", result)
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
              X <- list(...)[[1L]]
              elementTypeX <- elementType(X)
              listData <-
                mapply(FUN = FUN, ..., MoreArgs = MoreArgs, SIMPLIFY = FALSE)
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

setAs("ANY", "SimpleList", function(from) {
  coerceToSimpleList(from)
})

coerceToSimpleList <- function(from, element.type, ...) {
  if (missing(element.type)) {
    if (is(from, "List"))
      element.type <- from@elementType
    else if (is.list(from))
      element.type <- NULL
    else element.type <- class(from)
  }
  SimpleListClass <- listClassName("Simple", element.type)
  if (!is(from, SimpleListClass)) {
    listData <- as.list(from)
    if (!is.null(element.type))
      listData <- lapply(listData, coercerToClass(element.type), ...)
    newList(SimpleListClass, listData)
  } else {
    from
  }
}
