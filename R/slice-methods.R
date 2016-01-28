### =========================================================================
### Slice the bread
### -------------------------------------------------------------------------


setGeneric("slice", signature="x",
           function(x, lower=-Inf, upper=Inf, ...) standardGeneric("slice"))

setMethod("slice", "Rle",
          function(x, lower = -Inf, upper = Inf,
                   includeLower = TRUE, includeUpper = TRUE,
                   rangesOnly = FALSE)
          {
              if (!isSingleNumber(lower)) {
                  stop("'lower' must be a single number")
              }
              if (!isSingleNumber(upper)) {
                  stop("'upper' must be a single number")
              }
              if (!isTRUEorFALSE(includeLower)) {
                  stop("'includeLower' must be TRUE or FALSE")
              }
              if (!isTRUEorFALSE(includeUpper)) {
                  stop("'includeUpper' must be TRUE or FALSE")
              }
              if (!isTRUEorFALSE(rangesOnly)) {
                  stop("'rangesOnly' must be TRUE or FALSE")
              }
              if (lower == -Inf) {
                  ranges <- Rle(TRUE, length(x))
              } else if (includeLower) {
                  ranges <- (x >= lower)
              } else {
                  ranges <- (x > lower)
              }
              if (upper < Inf) {
                  if (includeUpper) {
                      ranges <- ranges & (x <= upper)
                  } else {
                      ranges <- ranges & (x < upper)
                  }
              }
              if (rangesOnly) {
                  as(ranges, "IRanges")
              } else {
                  Views(x, ranges)
              }
          })

setMethod("slice", "RleList",
          function(x, lower = -Inf, upper = Inf,
                   includeLower = TRUE, includeUpper = TRUE,
                   rangesOnly = FALSE)
          {
              if (!isSingleNumber(lower))
                  stop("'lower' must be a single number")
              if (!isSingleNumber(upper))
                  stop("'upper' must be a single number")
              if (!isTRUEorFALSE(includeLower))
                  stop("'includeLower' must be TRUE or FALSE")
              if (!isTRUEorFALSE(includeUpper))
                  stop("'includeUpper' must be TRUE or FALSE")
              if (!isTRUEorFALSE(rangesOnly))
                  stop("'rangesOnly' must be TRUE or FALSE")
              if (lower == -Inf) {
                  ranges <-
                    RleList(lapply(elementNROWS(x),
                                   function(len) Rle(TRUE, len)),
                            compress=FALSE)
              } else if (includeLower) {
                  ranges <- (x >= lower)
              } else {
                  ranges <- (x > lower)
              }
              if (upper < Inf) {
                  if (includeUpper) {
                      ranges <- ranges & (x <= upper)
                  } else {
                      ranges <- ranges & (x < upper)
                  }
              }
              if (rangesOnly) {
                  as(ranges, "CompressedIRangesList")
              } else {
                  RleViewsList(rleList = x,
                               rangesList = as(ranges, "SimpleIRangesList"))
              }
          })

setMethod("slice", "ANY", function(x, lower=-Inf, upper=Inf, ...) {
  slice(as(x, "Rle"), lower=lower, upper=upper, ...)
})
