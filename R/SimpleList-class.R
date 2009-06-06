### =========================================================================
### SimpleList objects
### -------------------------------------------------------------------------

setClass("SimpleList",
         contains=c("Sequence"),
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
                     listData <- as.list(x)
                     names(listData) <- value
                     slot(x, "listData") <- listData
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
    if (!all(unlist(lapply(listData, is, elementTypeData))))
        stop("all elements in 'listData' must be ", elementTypeData, " objects")
    new(listClass, listData = listData, ...)
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
                     listData <- as.list(x)
                     listData[[i]] <- value
                     slot(x, "listData") <- listData
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
                  slot(x, "listData", check=FALSE) <- as.list(x)[i]
              }
              .bracket.Sequence(x, i)
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
            cat("  A ", class(object), " instance of length ", lo, "\n", sep="")
### TODO: show (some of the) elements here
          })
