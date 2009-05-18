### =========================================================================
### DataFrame objects
### -------------------------------------------------------------------------
###
### The DataFrame virtual class is a general container for storing a list of
### sequences.
###

setClass("DataFrame", contains = "ListLike", representation("VIRTUAL"))

setClassUnion("DataFrameORNULL", c("DataFrame", "NULL"))


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Basic methods.
###

setMethod("[", "DataFrame", function(x, i, j, ..., drop = FALSE)
          stop("missing '[' method for DataFrame class ", class(x)))

setReplaceMethod("[", "DataFrame", function(x, i, j,..., value)
                 stop("attempt to modify the value of a ", class(x), " instance"))

setGeneric("cbind", function(..., deparse.level=1) standardGeneric("cbind"),
           signature = "...")

setMethod("cbind", "DataFrame", function(..., deparse.level=1)
          stop("missing 'cbind' method for DataFrame class ",
               class(list(...)[[1]])))

setMethod("dim", "DataFrame",
          function(x) {
              if (length(x) == 0L)
                  c(0L, 0L)
              else
                  c(length(x[[1]]), length(x))
          })

setMethod("head", "DataFrame",
          function(x, n = 6L, ...)
          {
              stopifnot(length(n) == 1L)
              if (n < 0L)
                  n <- max(nrow(x) + n, 0L)
              else
                  n <- min(n, nrow(x))
              if (n == 0L)
                  x[integer(0),,drop = FALSE]
              else
                  window(x, 1L, n)
          })

setMethod("is.array", "DataFrame", function(x) TRUE)

setGeneric("rbind", function(..., deparse.level=1) standardGeneric("rbind"),
           signature = "...")

setMethod("rbind", "DataFrame", function(..., deparse.level=1)
          stop("missing 'rbind' method for DataFrame class ",
               class(list(...)[[1]])))
  
setMethod("seqextract", "DataFrame",
          function(x, start=NULL, end=NULL, width=NULL)
          {
              ir <- IRanges(start=start, end=end, width=width, names=NULL)
              if (any(start(ir) < 1L) || any(end(ir) > nrow(x)))
                  stop("some ranges are out of bounds")
              do.call(c,
                      lapply(seq_len(length(ir)),
                             function(i)
                                 window(x,
                                        start = start(ir)[i],
                                        width = width(ir)[i])))
          })

setMethod("tail", "DataFrame",
          function(x, n = 6L, ...)
          {
              stopifnot(length(n) == 1L)
              xlen <- nrow(x)
              if (n < 0L) 
                  n <- max(xlen + n, 0L)
              else
                  n <- min(n, xlen)
              if (n == 0L)
                  x[integer(0),,drop = FALSE]
              else
                  window(x, xlen - n + 1L, xlen)
          })

setMethod("window", "DataFrame",
          function(x, start = NULL, end = NULL, width = NULL,
                   frequency = NULL, delta = NULL, ...)
          {
              if (is.null(frequency) && is.null(delta)) {
                  solved_SEW <-
                    solveWindowSEW(nrow(x),
                                   start = ifelse(is.null(start), NA, start),
                                   end = ifelse(is.null(end), NA, end),
                                   width = ifelse(is.null(width), NA, width))
                  x[as.integer(solved_SEW),,drop = FALSE]
              } else {
                  if (!is.null(width)) {
                      if (is.null(start))
                          start <- end - width + 1L
                      else if (is.null(end))
                          end <- start + width - 1L
                  }
                  idx <-
                    stats:::window.default(seq_len(nrow(x)), start = start, end = end,
                                           frequency = frequency, deltat = delta, ...)
                  attributes(idx) <- NULL
                  x[idx,,drop = FALSE]
              }
          })


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Looping methods.
###

setMethod("aggregate", "DataFrame",
          function(x, by, FUN, start = NULL, end = NULL, width = NULL,
                   frequency = NULL, delta = NULL, ..., simplify = TRUE)
          {
              FUN <- match.fun(FUN)
              if (!missing(by)) {
                  start <- start(by)
                  end <- end(by)
              } else {
                  if (!is.null(width)) {
                      if (is.null(start))
                          start <- end - width + 1L
                      else if (is.null(end))
                          end <- start + width - 1L
                  }
                  start <- as.integer(start)
                  end <- as.integer(end)
              }
              if (length(start) != length(end))
                  stop("'start', 'end', and 'width' arguments have unequal length")
              n <- length(start)
              if (is.null(frequency) && is.null(delta)) {
                  sapply(seq_len(n), function(i)
                         FUN(window(x, start = start[i], end = end[i]), ...),
                         simplify = simplify)
              } else {
                  frequency <- rep(frequency, length.out = n)
                  delta <- rep(delta, length.out = n)
                  sapply(seq_len(n), function(i)
                         FUN(window(x, start = start[i], end = end[i],
                                    frequency = frequency[i], delta = delta[i]),
                             ...),
                         simplify = simplify)
              }
          })
