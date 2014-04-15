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

setMethod("extractROWS", "SimpleList",
    function(x, i)
    {
        if (missing(i) || !is(i, "Ranges"))
            i <- normalizeSingleBracketSubscript(i, x)
        initialize(x, listData=extractROWS(x@listData, i),
                      elementMetadata=extractROWS(x@elementMetadata, i))
    }
)

setMethod("replaceROWS", "SimpleList",
    function(x, i, value)
    {
        if (missing(i) || !is(i, "Ranges"))
            i <- normalizeSingleBracketSubscript(i, x)
        initialize(x, listData=replaceROWS(x@listData, i, value@listData))
    }
)

setMethod("getListElement", "SimpleList",
    function(x, i, exact=TRUE)
    {
        i <- normalizeDoubleBracketSubscript(i, x, exact=exact,
                                             error.if.nomatch=FALSE)
        x@listData[[i]]
    }
)

setMethod("setListElement", "SimpleList",
    function(x, i, value)
    {
        x@listData[[i]] <- value
        x
    }
)


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
              FUN <- match.fun(FUN)
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
              FUN <- match.fun(FUN)
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

.as.list.SimpleList <- function(x, use.names=TRUE)
{
    if (!isTRUEorFALSE(use.names))
        stop("'use.names' must be TRUE or FALSE")
    ans <- x@listData
    if (!use.names)
        names(ans) <- NULL
    ans
}
### S3/S4 combo for as.list.SimpleList
as.list.SimpleList <- function(x, ...) .as.list.SimpleList(x, ...)
setMethod("as.list", "SimpleList", as.list.SimpleList)

setAs("ANY", "SimpleList", function(from) {
  coerceToSimpleList(from)
})

setAs("list", "List", function(from) {
  coerceToSimpleList(from)
})

listElementType <- function(x) {
  cl <- lapply(x, class)
  clnames <- unique(unlist(cl, use.names=FALSE))
  if (length(clnames == 1L)) {
    clnames
  } else {
    contains <- lapply(cl, function(x) getClass(x, TRUE)@contains)
    clnames <- c(clnames,
                 unlist(lapply(contains, names), use.names=FALSE))
    cltab <- table(factor(clnames, unique(clnames)))
    clnames <- names(cltab)[cltab == length(x)]
    if (length(clnames) > 0L) {
      clnames[1]
    } else {
      NULL
    }
  }
}

coerceToSimpleList <- function(from, element.type, ...) {
  if (missing(element.type)) {
    if (is(from, "List"))
      element.type <- from@elementType
    else if (is.list(from))
      element.type <- listElementType(from)
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
