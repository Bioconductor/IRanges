### =========================================================================
### Sequence objects
### -------------------------------------------------------------------------
###
### The Sequence virtual class is a general container for storing a sequence
### i.e. an ordered set of elements.
###

setClass("Sequence", representation("VIRTUAL"))


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Basic methods.
###

setMethod("[", "Sequence", function(x, i, j, ..., drop = FALSE)
          stop("missing '[' method for Sequence class ", class(x)))

setReplaceMethod("[", "Sequence", function(x, i, j,..., value)
                 stop("attempt to modify the value of a ", class(x), " instance"))

setMethod("c", "Sequence", function(x, ..., recursive = FALSE)
          stop("missing 'c' method for Sequence class ", class(x)))

setMethod("head", "Sequence",
          function(x, n = 6L, ...)
          {
              stopifnot(length(n) == 1L)
              if (n < 0L)
                  n <- max(length(x) + n, 0L)
              else
                  n <- min(n, length(x))
              if (n == 0L)
                  x[integer(0)]
              else
                  window(x, 1L, n)
          })

setMethod("rep", "Sequence", function(x, times)
          x[rep.int(seq_len(length(x)), times)])

setMethod("rev", "Sequence",
          function(x) {
              if (length(x) == 0)
                  x
              else
                  x[length(x):1]  
          })

setGeneric("seqextract", signature="x",
           function(x, start=NULL, end=NULL, width=NULL) standardGeneric("seqextract"))
  
setMethod("seqextract", "Sequence",
          function(x, start=NULL, end=NULL, width=NULL)
          {
              ir <- IRanges(start=start, end=end, width=width, names=NULL)
              if (any(start(ir) < 1L) || any(end(ir) > length(x)))
                  stop("some ranges are out of bounds")
              do.call(c,
                      lapply(seq_len(length(ir)),
                             function(i)
                                 window(x,
                                        start = start(ir)[i],
                                        width = width(ir)[i])))
          })

setMethod("seqextract", "vector",
          function(x, start=NULL, end=NULL, width=NULL)
          {
              ir <- IRanges(start=start, end=end, width=width, names=NULL)
              .Call("vector_subsetbyranges", x, start(ir), width(ir), PACKAGE="IRanges")
          })

setMethod("tail", "Sequence",
          function(x, n = 6L, ...)
          {
              stopifnot(length(n) == 1L)
              xlen <- length(x)
              if (n < 0L) 
                  n <- max(xlen + n, 0L)
              else
                  n <- min(n, xlen)
              if (n == 0L)
                  x[integer(0)]
              else
                  window(x, xlen - n + 1L, xlen)
          })

### Returns an IRanges instance of length 1.
### Not exported.
solveWindowSEW <- function(seq_length, start, end, width)
{
    solved_SEW <- try(solveUserSEW(seq_length, start=start, end=end, width=width))
    if (is(solved_SEW, "try-error"))
        stop("Invalid sequence coordinates.\n",
             "  Please make sure the supplied 'start', 'end' and 'width' arguments\n",
             "  are defining a region that is within the limits of the sequence.")
    solved_SEW
}

setMethod("window", "Sequence",
          function(x, start = NULL, end = NULL, width = NULL,
                   frequency = NULL, delta = NULL, ...)
          {
              if (is.null(frequency) && is.null(delta)) {
                  solved_SEW <-
                    solveWindowSEW(length(x),
                                   start = ifelse(is.null(start), NA, start),
                                   end = ifelse(is.null(end), NA, end),
                                   width = ifelse(is.null(width), NA, width))
                  x[as.integer(solved_SEW)]
              } else {
                  if (!is.null(width)) {
                      if (is.null(start))
                          start <- end - width + 1L
                      else if (is.null(end))
                          end <- start + width - 1L
                  }
                  idx <-
                    stats:::window.default(seq_len(length(x)), start = start, end = end,
                                           frequency = frequency, deltat = delta, ...)
                  attributes(idx) <- NULL
                  x[idx]
              }
          })

setMethod("window", "vector",
          function(x, start = NULL, end = NULL, width = NULL,
                  frequency = NULL, delta = NULL, ...)
          {
              if (is.null(frequency) && is.null(delta)) {
                  solved_SEW <-
                    solveWindowSEW(length(x),
                                   start = ifelse(is.null(start), NA, start),
                                   end = ifelse(is.null(end), NA, end),
                                   width = ifelse(is.null(width), NA, width))
                  .Call("vector_subsetbyranges",
                        x, start(solved_SEW), width(solved_SEW),
                        PACKAGE="IRanges")
               } else {
                  if (!is.null(width)) {
                      if (is.null(start))
                          start <- end - width + 1L
                      else if (is.null(end))
                          end <- start + width - 1L
                  }
                  idx <-
                    stats:::window.default(seq_len(length(x)), start = start, end = end,
                                           frequency = frequency, deltat = delta, ...)
                  attributes(idx) <- NULL
                  x[idx]
              }
          })

### Maybe this is how `!=` should have been defined in the base package so
### nobody would ever need to bother implementing such an obvious thing.
setMethod("!=", signature(e1="Sequence", e2="Sequence"),
    function(e1, e2) !(e1 == e2)
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Looping methods.
###

.aggregateInternal <-
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
}

setMethod("aggregate", "Sequence", .aggregateInternal)

setMethod("aggregate", "vector", .aggregateInternal)

.shiftApplyInternal <-
function(SHIFT, X, Y, FUN, ..., OFFSET = 0L, simplify = TRUE, verbose = FALSE)
{
    FUN <- match.fun(FUN)
    N <- length(X)
    if (N != length(Y))
        stop("'X' and 'Y' must be of equal length")
    
    if (length(SHIFT) == 0 || !is.numeric(SHIFT) ||
            any(is.na(SHIFT)) || any(SHIFT < 0))
        stop("all 'SHIFT' values must be non-negative")
    SHIFT <- as.integer(SHIFT)
    
    if (length(OFFSET) == 0 || !is.numeric(OFFSET) ||
            any(is.na(OFFSET)) || any(OFFSET < 0))
        stop("'OFFSET' must be non-negative")
    OFFSET <- as.integer(OFFSET)
    
    ## Perform X setup
    shiftedStartX <- rep.int(1L + OFFSET, length(SHIFT))
    shiftedEndX <- N - SHIFT
    
    ## Perform Y setup
    shiftedStartY <- 1L + SHIFT
    shiftedEndY <- rep.int(N - OFFSET, length(SHIFT))
    
    if (verbose) {
        maxI <- length(SHIFT)
        ans <-
          sapply(seq_len(length(SHIFT)),
                 function(i) {
                     cat("\r", i, "/", maxI)
                     FUN(window(X, start = shiftedStartX[i], end = shiftedEndX[i]),
                         window(Y, start = shiftedStartY[i], end = shiftedEndY[i]),
                         ...)
                 }, simplify = simplify)
        cat("\n")
    } else {
        ans <-
          sapply(seq_len(length(SHIFT)),
                 function(i)
                     FUN(window(X, start = shiftedStartX[i], end = shiftedEndX[i]),
                         window(Y, start = shiftedStartY[i], end = shiftedEndY[i]),
                         ...),
                 simplify = simplify)
    }
    ans
}

setGeneric("shiftApply", signature = c("X", "Y"),
           function(SHIFT, X, Y, FUN, ..., OFFSET = 0L, simplify = TRUE,
                    verbose = FALSE)
           standardGeneric("shiftApply"))

setMethod("shiftApply", signature(X = "Sequence", Y = "Sequence"),
          .shiftApplyInternal)

setMethod("shiftApply", signature(X = "vector", Y = "vector"),
          .shiftApplyInternal)
