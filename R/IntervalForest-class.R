### =========================================================================
### IntervalForest objects
### -------------------------------------------------------------------------

setClass("IntervalForest", 
         representation(ptr="externalptr", mode="character",
                        partitioning="PartitioningByEnd"),
         contains = c("RangesList"))

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Validity
###
.valid.IntervalForest.partitioning <- function(x)
{
    dataLength <- .IntervalForestCall(x,"nobj")
    if (nobj(x@partitioning) != dataLength)
        "improper partitioning"
    else NULL
}

.valid.IntervalForest.mode <- function(x) {
  if (x@mode != "integer") 
    return("mode is not 'integer'")
  NULL
}

.valid.IntervalForest <- function(x) {
  c(.valid.IntervalForest.partitioning(x),
    .valid.IntervalForest.mode(x))
}

setValidity2("IntervalForest", .valid.IntervalForest)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Constructor
###

IntervalForest <- function(x) {
  if (!is(x, "IRangesList")) {
    stop("'x' must be an 'IRangesList' object")
  }
  if (elementType(x) != "IRanges") {
    stop("'elementType(x)' must be of class 'IRanges'")
  }
  as(x, "IntervalForest")
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion
###
setAs("IntervalForest", "CompressedIRangesList",
  function(from) {
    new2("CompressedIRangesList",
         unlistData=.IntervalForestCall(from, "asIRanges"),
         partitioning=from@partitioning,
         elementType="IRanges",
         check=FALSE)
})

setAs("IntervalForest", "IRanges",
      function(from) .IntervalForestCall(from, "asIRanges"))

setAs("CompressedIRangesList", "IntervalForest", function(from) {
  validObject(from)
    
  npartitions <- length(from@partitioning)
  partitionLengths <- elementLengths(from)
  
  ptr <- .Call2("IntegerIntervalForest_new", from@unlistData, partitionLengths,
                npartitions, PACKAGE="IRanges")
  new2("IntervalForest", 
       ptr = ptr, 
       mode="integer", 
       partitioning=from@partitioning, 
       check=FALSE)
})

setAs("RangesList", "IntervalForest",
      function(from) as(as(from, "CompressedIRangesList"), "IntervalForest"))

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Accessors
###

setMethod("start", "IntervalForest",
          function(x)
          new2("CompressedIntegerList",
               unlistData = .IntervalForestCall(x, "start"),
               partitioning = x@partitioning, check=FALSE))
setMethod("end", "IntervalForest",
          function(x)
          new2("CompressedIntegerList",
               unlistData = .IntervalForestCall(x, "end"),
               partitioning = x@partitioning, check=FALSE))
setMethod("width", "IntervalForest",
          function(x)
          new2("CompressedIntegerList",
               unlistData = .IntervalForestCall(x, "end") -
                 .IntervalForestCall(x, "start") + 1L,
               partitioning = x@partitioning, check=FALSE))

setMethod("elementLengths", "IntervalForest",
    function(x)
    {
        ans <- elementLengths(x@partitioning)
        names(ans) <- names(x)
        ans
    }
)

setMethod("length", "IntervalForest", function(x) length(x@partitioning))
setMethod("names", "IntervalForest", function(x) names(x@partitioning))

### - - - - 
### Subsetting
###

setMethod("[", "IntervalForest",
          function(x, i, j, ..., drop=TRUE) {
            if (!missing(j) || length(list(...)) > 0L)
              stop("invalid subsetting")
            rl <- split(callGeneric(as(x, "IRanges"), i = i, ...),
                        callGeneric(space(x), i = i, ...))
            as(rl, "IntervalForest")
          }
)


### - - - -
### show
### - - - -

setMethod("show", "IntervalForest", 
          function(object) {
            newobj <- as(object, "CompressedIRangesList")
            cat("IntervalForest of length ", length(newobj), "\n", sep="")
            showRangesList(newobj, with.header=FALSE)
          }
)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Low-level utilities
###

.IntervalForestCall <- function(object, fun, ...) {
  # validObject(object)
  fun <- paste("IntervalForest", fun, sep = "_")
  if (object@mode == "integer") {
    fun <- paste("Integer", fun, sep = "")
    .Call2(fun, object@ptr, ..., PACKAGE="IRanges")
  } else stop("unknown interval forest mode: ", object@mode)
}

## not for exporting, just a debugging utility
IntervalForestDump <- function(object) {
  cat("IntervalForest, levels: ", levels(object), "\n")
  .IntervalForestCall(object, "dump")
}
