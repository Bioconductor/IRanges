### =========================================================================
### RangesList objects
### -------------------------------------------------------------------------

## Accepts any type of Ranges instance as an element

setClass("RangesList", contains = "TypedList")

setClass("IRangesList", contains = "RangesList")


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Accessor methods.
###

setMethod("elementClass", "RangesList", function(x) "RangesORXRanges")

## coerced to internal ranges
setGeneric("ranges", function(object, ...) standardGeneric("ranges"))
setMethod("ranges", "RangesList",
          function(object, asRanges = TRUE) {
            if (!isTRUEorFALSE(asRanges))
              stop("'asRanges' should be TRUE or FALSE")
            els <- elements(object)
            if (asRanges)
              els <- lapply(els, as, "Ranges")
            els
          })

setMethod("elementClass", "IRangesList", function(x) "IRanges")

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
            x@elements <- lapply(elements(x),
                                 function(r) gaps(r, start=start, end=end))
            x@NAMES <- NULL
            x
          }
          )


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion.
###

setMethod("unlist", "RangesList",
          function(x, recursive = TRUE, use.names = TRUE) {
            if (!missing(recursive) || !missing(use.names))
              warning("'recursive' and 'use.names' arguments ignored")
            do.call("rbind", lapply(ranges(x), as.matrix))
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
              if (all(unlist(lapply(elements(object), is, "IRanges"))))
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

