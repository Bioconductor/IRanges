### =========================================================================
### RangesList objects
### -------------------------------------------------------------------------

## Accepts any type of Ranges instance as an element

setClass("RangesList", contains = "TypedList")

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Accessor methods.
###

setMethod("elementClass", "RangesList", function(x) "Ranges")

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


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "isEmpty" methods.
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
### The "max" and "min" methods.
###

setMethod("max", "RangesList",
          function(x, ..., na.rm)
          {
            if (length(x) == 0)
              return(integer(0))
            sapply(elements(x), max)
          }
          )

setMethod("min", "RangesList",
          function(x, ..., na.rm)
          {
            if (length(x) == 0)
              return(integer(0))
            sapply(elements(x), min)
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
            x@elements <- lapply(elements(x),
                                 function(r) gaps(r, start=start, end=end))
            x@NAMES <- NULL
            x
          }
          )


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion.
###

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

