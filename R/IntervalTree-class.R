### =========================================================================
### IntervalTree objects
### -------------------------------------------------------------------------

setClass("IntervalTree",
         representation(ptr = "externalptr", mode = "character"),
         contains = "Ranges")

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Accessors
###

setMethod("length", "IntervalTree", function(x) IntervalTreeCall(x, "length"))

setMethod("start", "IntervalTree", function(x) IntervalTreeCall(x, "start"))
setMethod("end", "IntervalTree", function(x) IntervalTreeCall(x, "end"))

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Constructor
###

IntervalTree <- function(ranges) {
  as(ranges, "IntervalTree")
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion
###

setAs("IntervalTree", "IRanges", function(from) {
  IntervalTreeCall(from, "asIRanges")
})

setAs("IRanges", "IntervalTree", function(from) {
  msg <- c("IntervalTree objects and the \"intervaltree\" algorithm used ",
           "in findOverlaps() and family are deprecated. Please use the ",
           "\"nclist\" algorithm instead. See the 'algorithm' argument ",
           "in ?findOverlaps for more information.")
  .Deprecated(msg=wmsg(msg))

  validObject(from)
  ptr <- .Call2("IntegerIntervalTree_new", from, PACKAGE="IRanges")
  new2("IntervalTree", ptr = ptr, mode = "integer", check=FALSE)
})

setAs("Ranges", "IntervalTree", function(from) {
  as(as(from, "IRanges"), "IntervalTree")
})

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Low-level utilities
###

IntervalTreeCall <- function(object, fun, ...) {
  #validObject(object)  # causes an infinite recursion, because calls
                        # .valid.Vector.length(), which calls length(),
                        # which calls IntervalTreeCall(), which calls
                        # validObject(), etc...
  fun <- paste("IntervalTree", fun, sep = "_")
  if (object@mode == "integer") {
    fun <- paste("Integer", fun, sep = "")
    .Call2(fun, object@ptr, ..., PACKAGE="IRanges")
  } else stop("unknown interval tree mode: ", object@mode)
}
