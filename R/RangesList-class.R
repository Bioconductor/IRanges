### =========================================================================
### RangesList objects
### -------------------------------------------------------------------------

## Accepts any type of Ranges instance as an element

setClass("RangesList", prototype = prototype(elementClass = "Ranges"),
         contains = "TypedList")

setClass("IRangesList", prototype = prototype(elementClass = "IRanges"),
         contains = "RangesList")


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Accessor methods.
###

setMethod("start", "RangesList", function(x) start(unlist(x)))
setMethod("end", "RangesList", function(x) end(unlist(x)))
setMethod("width", "RangesList", function(x) width(unlist(x)))

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Constructor.
###

RangesList <- function(...)
{
  ranges <- list(...)
  NAMES <- names(ranges)
  names(ranges) <- NULL
  new("RangesList", elements=ranges, NAMES=NAMES)
}

IRangesList <- function(...)
{
    ranges <- list(...)
    NAMES <- names(ranges)
    names(ranges) <- NULL
    new("IRangesList", elements=ranges, NAMES=NAMES)
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Methods that are vectorized over the ranges
###

setMethod("isEmpty", "RangesList",
          function(x)
          {
            if (length(x) == 0)
              return(logical(0))
            sapply(elements(x), isEmpty)
          }
          )

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Some useful endomorphisms: "reduce" and "gaps".
###

### 'with.inframe.attrib' is ignored.
setMethod("reduce", "RangesList",
          function(x, with.inframe.attrib=FALSE)
          {
            elements <- elements(x)
            if (length(elements) == 0) {
              nir1 <- new("NormalIRanges")
            } else {
              start1 <- unlist(lapply(elements, start))
              width1 <- unlist(lapply(elements, width))
              ranges <- new2("IRanges", start=start1, width=width1, check=FALSE)
              nir1 <- asNormalIRanges(ranges, force=TRUE)
            }
            ## This transformation must be atomic.
            x@elements <- list(nir1)
            x@NAMES <- NULL
            x
          }
          )

### 'start' and 'end' are ignored.
setMethod("gaps", "RangesList",
          function(x, start=NA, end=NA)
          {
            x@elements <- lapply(x, function(r) gaps(r, start=start, end=end))
            x@NAMES <- NULL
            x
          }
          )


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion.
###

setMethod("as.data.frame", "RangesList",
          function(x, row.names=NULL, optional=FALSE, ...)
          {
            if (!(is.null(row.names) || is.character(row.names)))
              stop("'row.names'  must be NULL or a character vector")
            if (!missing(optional) || length(list(...)))
              warning("'optional' and arguments in '...' ignored")
            as.data.frame(unlist(x), row.names = row.names)
          })

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "show" method.
###

setMethod("show", "RangesList",
          function(object)
          {
            lo <- length(object)
            cat("  A ", class(object), " instance of length ", lo, "\n", sep="")
            ### TODO: show (some of the) ranges here
          }
          )


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "summary" method.
###

setMethod("summary", "RangesList",
          function(object)
          {
              if (all(unlist(lapply(object, is, "IRanges"))))
                  .Call("summary_IRangesList", object, PACKAGE="IRanges")
              else
                  stop("all elements must be of class 'IRanges' ")
          })


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion.
###

### From an IRangesList object to a NormalIRanges object.
setAs("IRangesList", "NormalIRanges",
      function(from) reduce(from)[[1]]
      )

