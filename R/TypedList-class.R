### =========================================================================
### TypedList objects
### -------------------------------------------------------------------------

## Wrapper around a list that ensures all elements extend from a certain type

setClass("TypedList",
         representation(
                        elements="list",   # a list of R objects
                        NAMES="characterORNULL"  # R doesn't like @names !!
                        ),
         prototype(
                   elements=list(),
                   NAMES=NULL
                   ),
         contains = "VIRTUAL"
         )

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Accessor methods.
###


setGeneric("elements", function(x) standardGeneric("elements"))
setMethod("elements", "TypedList", function(x) x@elements)

setMethod("length", "TypedList", function(x) length(elements(x)))

setMethod("names", "TypedList", function(x) x@NAMES)

### The only replacement method for TypedList objects!
setReplaceMethod("names", "TypedList",
                 function(x, value)
                 {
                   if (is.null(value)) {
                     x@NAMES <- NULL
                     return(x)
                   }
                   value <- as.character(value)
                   if (length(value) > length(x))
                     stop("number of names ",
                          length(value), " greater than number of elements",
                          length(x))
                   if (length(value) < length(x))
                     value <- c(value, rep(NA, length(x) - length(value)))
                   x@NAMES <- value
                   x
                 }
                 )


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Validity.
###

## allow subclasses to provide the required class of elements in the list

setGeneric("elementClass", function(x, ...) standardGeneric("elementClass"))

.valid.TypedList.elements <- function(x)
{
  ### FIXME: currently this test seems to be broken, because 'x' is
  ### dropped to TypedList from its original class
  if (!is.list(elements(x))
      || !all(sapply(elements(x), is, elementClass(x))))
    return(paste("the 'elements' slot must contain a list of",
                 elementClass(x), "objects"))
  NULL
}

.valid.TypedList.names <- function(x)
{
  if (!is(x@NAMES, "characterORNULL"))
    return("the 'NAMES' slot must be NULL or contain a character vector")
  if (is.null(names(x)))
    return(NULL)
  if (length(names(x)) != length(x))
    return("number of names and number of elements differ")
  NULL
}

.valid.TypedList <- function(x)
{
  c(.valid.TypedList.elements(x),
    .valid.TypedList.names(x))
}

setValidity2("TypedList", .valid.TypedList)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Subsetting.
###

### Extract the i-th element of a TypedList object.
### Supported 'i' types: numeric vector of length 1.
setMethod("[[", "TypedList",
          function(x, i, j, ...)
          {
            if (!missing(j) || length(list(...)) > 0)
              stop("invalid subsetting")
            if (missing(i))
              stop("subscript is missing")
            if (!is.character(i) && !is.numeric(i))
              stop("invalid subscript type")
            if (length(i) < 1L)
              stop("attempt to select less than one element")
            if (length(i) > 1L)
              stop("attempt to select more than one element")
            else if (!is.character(i) && !is.na(i) && (i<1L || i>length(x)))
              stop("subscript out of bounds")

            els <- elements(x)
            names(els) <- names(x)
            els[[i]]
          }
          )

setReplaceMethod("[[", "TypedList",
                 function(x, i, j,..., value)
                 {
                   if (!missing(j) || length(list(...)) > 0)
                     warning("arguments beyond 'i' ignored")
                   if (missing(i))
                     stop("subscript is missing")
                   if (!is.character(i) && !is.numeric(i))
                     stop("invalid subscript type")
                   if (length(i) < 1L)
                     stop("attempt to select less than one element")
                   if (length(i) > 1L)
                     stop("attempt to select more than one element")
                   if (is.numeric(i) && (i < 1L || i > length(x)+1))
                     stop("subscript out of bounds")
                   if (!is.null(value) && !canCoerce(value, elementClass(x)))
                     stop("cannot coerce 'value' to required class")
                   els <- x@elements
                   names(els) <- names(x)
                   if (!is.null(value))
                     value <- as(value, elementClass(x))
                   els[[i]] <- value 
                   x@elements <- els
                   names(x) <- names(els)
                   names(x@elements) <- NULL
                   x
                 })

### Supported 'i' types: numeric vector, logical vector, NULL and missing.
setMethod("[", "TypedList",
          function(x, i, j, ..., drop)
          {
            if (!missing(j) || length(list(...)) > 0)
              stop("invalid subsetting")
            if (missing(i))
              return(x)
            if (!is.atomic(i))
              stop("invalid subscript type")
            lx <- length(x)
            if (is.numeric(i)) {
              if (any(is.na(i)))
                stop("subscript contains NAs")
              if (any(i < -lx) || any(i > lx))
                stop("subscript out of bounds")
              if (any(i < 0) && any(i > 0))
                stop("negative and positive indices cannot be mixed")
            } else if (is.logical(i)) {
              if (any(is.na(i)))
                stop("subscript contains NAs")
              if (length(i) > lx)
                stop("subscript out of bounds")
            } else if (is.character(i)) {
              if (is.null(names(x)))
                stop("cannot subscript by character when names are NULL")
              i <- match(i, names(x))
            } else if (!is.null(i)) {
              stop("invalid subscript type")
            }
            slot(x, "elements", check=FALSE) <- elements(x)[i]
            if (!is.null(names(x)))
              slot(x, "NAMES", check=FALSE) <- names(x)[i]
            x
          }
          )

setReplaceMethod("[", "TypedList",
                 function(x, i, j,..., value)
                 stop("attempt to modify the value of a ", class(x), " instance")
                 )


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "append" method.
###

setMethod("append", c("TypedList", "TypedList"),
          function(x, values, after=length(x))
          {
            if (!isSingleNumber(after))
              stop("'after' must be a single number")
            ans_elements <- append(elements(x), elements(values),
                                   after=after)
            nm1 <- names(x)
            nm2 <- names(values)
            if (is.null(nm1) && is.null(nm2)) {
              ans_NAMES <- NULL
            } else {
              if (is.null(nm1))
                nm1 <- rep.int("", length(x))
              if (is.null(nm2))
                nm2 <- rep.int("", length(values))
              ans_NAMES <- append(nm1, nm2, after=after)
            }
            ## This transformation must be atomic.
            x@elements <- ans_elements
            x@NAMES <- ans_NAMES
            x
          }
          )

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Methods that are vectorized over the elements
###

setGeneric("width", function(x) standardGeneric("width"))

setMethod("width", "TypedList", function(x) {
  sapply(elements(x), length)
})

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion.
###

.TypedList_asList <- function(from) {
  ### NOTE: we don't just get the elements slot, because that's internal.
  ### What is actually visible to the public may be different.
  to <- lapply(seq_len(length(from)), function(i) from[[i]])
  names(to) <- names(from)
  to
}

### From an TypedList object to a normal R list.
setAs("TypedList", "list",
      function(from) {
        .TypedList_asList(from)
      })

setMethod("as.data.frame", "TypedList",
          function(x, row.names=NULL, optional=FALSE, ...)
          {
            if (!(is.null(row.names) || is.character(row.names)))
              stop("'row.names'  must be NULL or a character vector")
            as.data.frame(as(x, "list"), row.names = row.names,
                          optional = optional, ...)
          })
            
### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "show" method.
###

setMethod("show", "TypedList",
          function(object)
          {
            lo <- length(object)
            cat("  A ", class(object), " instance of length ", lo, "\n", sep="")
### TODO: show (some of the) elements here
          }
          )
