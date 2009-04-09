### =========================================================================
### Rle objects
### -------------------------------------------------------------------------
###
### Class definitions
###

setClass("Rle",
         representation(values = "vector",
                        lengths = "integer"),
         contains = "Sequence",
         validity = function(object)
         {
             msg <- NULL
             run_values <- runValue(object)
             run_lengths <- runLength(object)
             if (length(run_values) != length(run_lengths))
                 msg <- c(msg, "run values and run lengths must have the same length")
             if (!all(run_lengths > 0L))
                 msg <- c(msg, "all run lengths must be positive")
             ## TODO: Fix the following test.
             #if (length(run_lengths) >= 2 && is.atomic(run_values)
             #      && any(run_values[-1] == run_values[-length(run_values)]))
             #    msg <- c(msg, "consecutive runs must have different values")
             if (is.null(msg)) TRUE else msg
         })
 
### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Accessor methods.
###

setGeneric("runLength", signature = "x",
           function(x) standardGeneric("runLength"))
setMethod("runLength", "Rle", function(x) x@lengths)
 
setGeneric("runValue", signature = "x",
           function(x) standardGeneric("runValue"))
setMethod("runValue", "Rle", function(x) x@values)

setGeneric("nrun", signature = "x", function(x) standardGeneric("nrun"))
setMethod("nrun", "Rle", function(x) length(runLength(x)))

setMethod("start", "Rle", function(x) cumsum(c(1L, runLength(x)))[-(nrun(x)+1L)])
setMethod("end", "Rle", function(x) cumsum(runLength(x)))
setMethod("width", "Rle", function(x) runLength(x))

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Replace methods.
###

setGeneric("runLength<-", signature="x",
           function(x, value) standardGeneric("runLength<-"))
setReplaceMethod("runLength", "Rle",
                 function(x, value) Rle(values = runValue(x), lengths = value))
         
setGeneric("runValue<-", signature="x",
           function(x, value) standardGeneric("runValue<-"))
setReplaceMethod("runValue", "Rle",
                 function(x, value) Rle(values = value, lengths = runLength(x)))

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Constructors
###

setGeneric("Rle", signature = c("values", "lengths"),
           function(values, lengths, ...) standardGeneric("Rle"))

setMethod("Rle", signature = c(values = "missing", lengths = "missing"),
          function(values, lengths) new("Rle", values = vector(), lengths = integer()))

setMethod("Rle", signature = c(values = "vector", lengths = "missing"),
          function(values, lengths)
          {
              if (is.factor(values)) {
                  rleOutput <- rle(as.integer(values))
                  rleOutput[["values"]] <-
                    factor(rleOutput[["values"]],
                           levels = seq_len(length(levels(values))),
                           labels = levels(values))
              } else {
                  rleOutput <- rle(unname(values))
              }
              new("Rle", values = rleOutput[["values"]], lengths = rleOutput[["lengths"]])
          })

setMethod("Rle", signature = c(values = "vector", lengths = "integer"),
          function(values, lengths, check = TRUE)
          {
              if (check) {
                  if (length(values) != length(lengths))
                      stop("'values' and 'lengths' must have the same length")
                  if (any(is.na(lengths)) || any(lengths < 0L))
                      stop("'lengths' must contain all positive integers")
                  zeros <- which(lengths == 0L)
                  if (length(zeros) > 0L) {
                      values <- values[-zeros]
                      lengths <- lengths[-zeros]
                  }
              }
              n <- length(values)
              y <- values[-1L] != values[-n]
              i <- c(which(y | is.na(y)), n)
              new("Rle", values = values[i], lengths = diff(c(0L, cumsum(lengths)[i])))
          })

setMethod("Rle", signature = c(values = "vector", lengths = "numeric"),
          function(values, lengths, check = TRUE)
              Rle(values = values, lengths = as.integer(lengths),
                  check = check))

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion
###

setAs("vector", "Rle", function(from) Rle(from))
setAs("factor", "Rle", function(from) Rle(from))

setAs("Rle", "vector", function(from) as.vector(from))
setAs("Rle","logical",  function(from) as.logical(from))
setAs("Rle", "integer", function(from) as.integer(from))
setAs("Rle", "numeric", function(from) as.numeric(from))
setAs("Rle", "complex", function(from) as.complex(from))
setAs("Rle", "character", function(from) as.character(from))
setAs("Rle", "raw", function(from) as.raw(from))
setAs("Rle", "factor", function(from) as.factor(from))
setAs("Rle", "IRanges",
      function(from) {
          if (!is.logical(runValue(from))) 
              stop("cannot coerce a non-logical 'Rle' to an IRanges object")
          keep <- runValue(from)
          IRanges(start = start(from)[keep], width = runLength(from)[keep])
      })
setAs("Rle", "NormalIRanges",
      function(from) newNormalIRangesFromIRanges(as(from, "IRanges"), check=FALSE))

setMethod("as.vector", c("Rle", "missing"), function(x, mode) rep(runValue(x), runLength(x)))
setMethod("as.logical", "Rle", function(x) rep(as.logical(runValue(x)), runLength(x)))
setMethod("as.integer", "Rle", function(x) rep.int(as.integer(runValue(x)), runLength(x)))
setMethod("as.numeric", "Rle", function(x) rep(as.numeric(runValue(x)), runLength(x)))
setMethod("as.complex", "Rle", function(x) rep(as.complex(runValue(x)), runLength(x)))
setMethod("as.character", "Rle", function(x) rep(as.character(runValue(x)), runLength(x)))
setMethod("as.raw", "Rle", function(x) rep(as.raw(runValue(x)), runLength(x)))
setMethod("as.factor", "Rle", function(x) rep(as.factor(runValue(x)), runLength(x)))

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Group generic methods
###

setMethod("Ops", signature(e1 = "Rle", e2 = "Rle"),
          function(e1, e2)
          {
              n1 <- length(e1)
              n2 <- length(e2)
              if (n1 == 0 || n2 == 0) {
                  ends <- integer(0)
                  which1 <- integer(0)
                  which2 <- integer(0)
              } else {
                  n <- max(n1, n2)
                  if (max(n1, n2) %% min(n1, n2) != 0)
                      warning("longer object length is not a multiple of shorter object length")
                  if (n1 < n)
                      e1 <- rep(e1, length.out = n)
                  if (n2 < n)
                      e2 <- rep(e2, length.out = n)
                  # ends <- sort(unique(c(end(e1), end(e2))))
                  ends <- .Call("Integer_sorted_merge", end(e1), end(e2), PACKAGE="IRanges")
                  which1 <- .Call("Integer_sorted_findInterval", ends, start(e1), PACKAGE="IRanges")
                  which2 <- .Call("Integer_sorted_findInterval", ends, start(e2), PACKAGE="IRanges")
              }
              Rle(values = callGeneric(runValue(e1)[which1], runValue(e2)[which2]),
                  lengths = diff(c(0L, ends)), check = FALSE)
          })

setMethod("Ops", signature(e1 = "Rle", e2 = "vector"),
          function(e1, e2) callGeneric(e1, Rle(e2)))

setMethod("Ops", signature(e1 = "vector", e2 = "Rle"),
          function(e1, e2) callGeneric(Rle(e1), e2))

setMethod("Math", "Rle",
          function(x)
              switch(.Generic,
                     cumsum =, cumprod = callGeneric(as.vector(x)),
                     Rle(values = callGeneric(runValue(x)),
                         lengths = runLength(x), check = FALSE)))

setMethod("Math2", "Rle",
          function(x, digits)
          {
              if (missing(digits))
                  digits <- ifelse(.Generic == "round", 0, 6)
              Rle(values = callGeneric(runValue(x), digits = digits),
                  lengths = runLength(x), check = FALSE)
          })

setMethod("Summary", "Rle",
          function(x, ..., na.rm = FALSE)
              switch(.Generic,
                     all =, any =, min =, max =, range =
                     callGeneric(runValue(x), ..., na.rm = na.rm),
                     sum = sum(runValue(x) * runLength(x), ..., na.rm = na.rm),
                     prod = prod(runValue(x) ^ runLength(x), ..., na.rm = na.rm)))

setMethod("Complex", "Rle",
          function(z)
              Rle(values = callGeneric(runValue(z)), lengths = runLength(z),
                  check = FALSE))

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### General methods
###

setMethod("[", "Rle",
          function(x, i, j, ...,
                   drop = !is.null(getOption("dropRle")) && getOption("dropRle"))
          {
              if (!missing(j) || length(list(...)) > 0)
                  stop("invalid subsetting")
              lx <- length(x)
              if (missing(i) || lx == 0)
                  return(x)
              if (is(i, "Rle") && is.logical(runValue(i)) && lx == length(i)) {
                  if (!any(runValue(i))) {
                      output <- new("Rle")
                  } else {
                      whichValues <- which(runValue(i))
                      starts <- start(i)[whichValues]
                      widths <- width(i)[whichValues]
                      output <-
                        do.call(c,
                                lapply(seq_len(length(starts)),
                                       function(k)
                                       subseq(x, start = starts[k], width = widths[k])))
                  }
                  if (drop)
                      output <- as.vector(output)
              } else if (is(i, "Ranges")) {
                  if (any(start(i) <= 0) || any(end(i) > length(x)))
                    stop("range index out of bounds")
                  if (length(i) == 0) {
                      output <- new("Rle")
                  } else {
                      xstart <- start(x)
                      from <- findInterval(start(i), xstart)
                      to <- findInterval(end(i), xstart)
                      runseq <- mseq(from, to)
                      lens <- runLength(x)[runseq]
                      breaks <- cumsum(c(1L, to - from + 1L))
                      lens[head(breaks, -1)] <-
                        pmin(end(i), end(x)[from]) - start(i) + 1L
                      lens[tail(breaks - 1L, -1)] <-
                        end(i) - pmax(start(i), start[to]) + 1L
                      output <- Rle(runValue(x)[runseq], lens)
                  }
                  if (drop)
                      output <- as.vector(output)
              } else {
                  if (is.numeric(i)) {
                      i <- as.integer(i[!is.na(i)])
                      if (any(i < -lx) || any(i > lx))
                          stop("subscript out of bounds")
                      if (any(i < 0)) {
                          if (any(i > 0))
                              stop("negative and positive indices cannot be mixed")
                          i <- seq_len(lx)[i]
                      }
                  } else if (is.logical(i)) {
                      if (lx %% length(i) != 0)
                          warning("length of x is not a multiple of the length of i")
                      i <- which(rep(i, length.out = lx))
                  } else if (is.null(i)) {
                      i <- integer(0)
                  } else {
                      stop("invalid subscript type")
                  }
                  output <- runValue(x)[findInterval(i, start(x))]
                  if (!drop)
                      output <- Rle(output)
              }
              output
          })

setMethod("%in%", "Rle",
          function(x, table)
              Rle(values = runValue(x) %in% table, lengths = runLength(x),
                  check = FALSE))

setMethod("aggregate", "Rle",
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
                  startX <- start(x)
                  endX <- end(x)
                  runStart <- findInterval(start, startX)
                  runEnd <- findInterval(end, startX)
                  offsetStart <- start - startX[runStart]
                  offsetEnd <- endX[runEnd] - end
                  ## Performance Optimization
                  ## Use a stripped down loop with empty Rle object
                  newRle <- new("Rle")
                  sapply(seq_len(n),
                         function(i)
                             FUN(.Call("Rle_run_subseq",
                                       x, runStart[i], runEnd[i],
                                       offsetStart[i], offsetEnd[i],
                                       newRle, PACKAGE = "IRanges"),
                                 ...),
                         simplify = simplify)
              } else {
                  frequency <- rep(frequency, length.out = n)
                  delta <- rep(delta, length.out = n)
                  sapply(seq_len(n),
                         function(i)
                             FUN(window(x, start = start[i], end = end[i],
                                        frequency = frequency[i], delta = delta[i]),
                                 ...),
                         simplify = simplify)
              }
          })

setMethod("c", "Rle", 
          function(x, ..., recursive = FALSE)
          {
              if (recursive)
                  stop("'recursive' mode is not supported")
              args <- list(x, ...)
              if (!all(unlist(lapply(args, is, "Rle"))))
                  stop("all arguments in '...' must be Rle objects")
              Rle(values  = unlist(lapply(args, slot, "values")),
                  lengths = unlist(lapply(args, slot, "lengths")),
                  check = FALSE)
          })

setGeneric("findRange", signature = "vec",
           function(x, vec) standardGeneric("findRange"))

setMethod("findRange", signature = c(vec = "Rle"),
          function(x, vec) {
              run <- findRun(x, vec)
              if (any(is.na(run)))
                stop("all 'x' values must be in [1, 'length(vec)']")
              IRanges(start = start(vec)[run], width = width(vec)[run])
          })

setGeneric("findRun", signature = "vec",
           function(x, vec) standardGeneric("findRun"))

setMethod("findRun", signature = c(vec = "Rle"),
          function(x, vec) {
            starts <- start(vec)
            runs <- findInterval(x, starts)
            runs[x == 0 | x > length(vec)] <- NA
            runs
          })

setMethod("head", "Rle",
          function(x, n = 6L, ...) {
            stopifnot(length(n) == 1L)
            if (n < 0L)
                n <- max(length(x) + n, 0L)
            else
                n <- min(n, length(x))
            if (n == 0L)
              x[integer(0)]
            else
              subseq(x, 1L, n)
          })

setMethod("is.na", "Rle",
          function(x)
              Rle(values = is.na(runValue(x)), lengths = runLength(x),
                  check = FALSE))

setMethod("length", "Rle", function(x) sum(runLength(x)))

setMethod("rep", "Rle",
          function(x, times, length.out, each)
          {
              if (!missing(each) && length(each) > 0) {
                  x@lengths <- runLength(x) * as.integer(each[1])
              } else if (!missing(times) && length(times) > 0) {
                  times <- as.integer(times)
                  if (length(times) == length(x)) {
                      x@lengths <- runLength(x) + diff(c(0L, cumsum(times)[end(x)])) - 1L
                  } else if (length(times) == 1) {
                      x <- Rle(values  = rep(runValue(x), times = times),
                               lengths = rep.int(runLength(x), times = times),
                               check = FALSE)
                  } else {
                      stop("invalid 'times' argument")
                  }
              } else if (!missing(length.out) && length(length.out) > 0) {
                  n <- length(x)
                  length.out <- as.integer(length.out[1])
                  if (length.out == 0) {
                      x <- new("Rle")
                  } else if (length.out < n) {
                      x <- subseq(x, 1, length.out)
                  } else if (length.out > n) {
                      x <- subseq(rep(x, times = ceiling(length.out / n)), 1, length.out)
                  }
              }
              x
          })

setGeneric("rep.int", signature = "x",
           function(x, ...) standardGeneric("rep.int"),
           useAsDefault = function(x, ...) base::rep.int(x, ...))

setMethod("rep.int", "Rle",
          function(x, times)
          {
              times <- as.integer(times)
              if (length(times) == length(x)) {
                  x@lengths <- runLength(x) + diff(c(0L, cumsum(times)[end(x)])) - 1L
              } else if (length(times) == 1) {
                  x <- Rle(values  = rep.int(runValue(x), times = times),
                           lengths = rep.int(runLength(x), times = times),
                           check = FALSE)
              } else {
                  stop("invalid 'times' argument")
              }
              x
          })

setMethod("rev", "Rle",
          function(x)
          {
              x@values <- rev(runValue(x))
              x@lengths <- rev(runLength(x))
              x
          })

setGeneric("shiftApply", signature = c("X", "Y"),
           function(SHIFT, X, Y, FUN, ..., OFFSET = 0L, simplify = TRUE,
                    verbose = FALSE)
           standardGeneric("shiftApply"))

setMethod("shiftApply", signature(X = "Rle", Y = "Rle"),
          function(SHIFT, X, Y, FUN, ..., OFFSET = 0L, simplify = TRUE,
                   verbose = FALSE)
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
              startX <- start(X)
              endX <- end(X)
              runStartX <- findInterval(shiftedStartX, startX)
              runEndX <- findInterval(shiftedEndX, startX)
              offsetStartX <- shiftedStartX - startX[runStartX]
              offsetEndX <- endX[runEndX] - shiftedEndX

              ## Perform Y setup
              shiftedStartY <- 1L + SHIFT
              shiftedEndY <- rep.int(N - OFFSET, length(SHIFT))
              startY <- start(Y)
              endY <- end(Y)
              runStartY <- findInterval(shiftedStartY, startY)
              runEndY <- findInterval(shiftedEndY, startY)
              offsetStartY <- shiftedStartY - startY[runStartY]
              offsetEndY <- endY[runEndY] - shiftedEndY

              ## Performance Optimization
              ## Use a stripped down loop with empty Rle object
              newX <- new("Rle")
              newY <- new("Rle")
              if (verbose) {
                  maxI <- length(SHIFT)
                  ans <-
                    sapply(seq_len(length(SHIFT)),
                           function(i) {
                               cat("\r", i, "/", maxI)
                               FUN(.Call("Rle_run_subseq",
                                         X, runStartX[i], runEndX[i],
                                         offsetStartX[i], offsetEndX[i],
                                         newX, PACKAGE = "IRanges"),
                                   .Call("Rle_run_subseq",
                                         Y, runStartY[i], runEndY[i],
                                         offsetStartY[i], offsetEndY[i],
                                         newY, PACKAGE = "IRanges"),
                                              ...)
                           }, simplify = simplify)
                  cat("\n")
              } else {
                  ans <-
                    sapply(seq_len(length(SHIFT)),
                           function(i)
                               FUN(.Call("Rle_run_subseq",
                                         X, runStartX[i], runEndX[i],
                                         offsetStartX[i], offsetEndX[i],
                                         newX, PACKAGE = "IRanges"),
                                   .Call("Rle_run_subseq",
                                         Y, runStartY[i], runEndY[i],
                                         offsetStartY[i], offsetEndY[i],
                                         newY, PACKAGE = "IRanges"),
                                   ...),
                               simplify = simplify)
              }
              ans
          })

setMethod("sort", "Rle",
          function(x, decreasing = FALSE, na.last = NA, ...)
          {
              ord <- order(runValue(x), decreasing = decreasing, na.last = na.last)
              Rle(values = runValue(x)[ord], lengths = runLength(x)[ord],
                  check = FALSE)
          })

setMethod("subseq", "Rle",
          function(x, start = NA, end = NA, width = NA)
          {
              solved_SEW <- solveSubseqSEW(length(x), start, end, width)
              .Call("Rle_subseq", x, start(solved_SEW), width(solved_SEW), PACKAGE = "IRanges")
          })

setMethod("summary", "Rle",
          function (object, ..., digits = max(3, getOption("digits") - 3)) 
          {
              value <-
                if (is.logical(runValue(object))) 
                    c(ValueMode = "logical", {
                          tb <- table(object, exclude = NULL)
                          if (!is.null(n <- dimnames(tb)[[1L]]) && any(iN <- is.na(n)))
                              dimnames(tb)[[1L]][iN] <- "NA's"
                          tb
                      })
                else if (is.numeric(runValue(object))) {
                    nas <- is.na(object)
                    object <- object[!nas]
                    qq <- quantile(object)
                    qq <- signif(c(qq[1L:3L], mean(object), qq[4L:5L]), digits)
                    names(qq) <- c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max.")
                    if (any(nas)) 
                        c(qq, `NA's` = sum(nas))
                    else
                        qq
                }
                else
                    c(Length = length(object),
                      Class = class(object),
                      ValueMode = mode(runValue(object)))
              class(value) <- "table"
              value
          })

setGeneric("table", signature = "...",
          function(...) standardGeneric("table"),
              useAsDefault = function(...) base::table(...))
setMethod("table", "Rle",
          function(...)
          {
              x <- sort(...)
              structure(array(runLength(x), dim = nrun(x),
                              dimnames = structure(list(as.character(runValue(x))), 
                                      names = "")),
                      class = "table")
          })

setMethod("tail", "Rle",
          function(x, n = 6L, ...) {
            stopifnot(length(n) == 1L)
            xlen <- length(x)
            if (n < 0L) 
              n <- max(xlen + n, 0L)
            else
              n <- min(n, xlen)
            if (n == 0L)
              x[integer(0)]
            else
              subseq(x, xlen - n + 1L, xlen)
          })

setMethod("unique", "Rle",
          function(x, incomparables = FALSE, ...)
              unique(runValue(x), incomparables = incomparables, ...))

setMethod("window", "Rle",
          function(x, start = NULL, end = NULL, width = NULL,
                   frequency = NULL, delta = NULL, ...)
          {
              if (is.null(frequency) && is.null(delta)) {
                  subseq(x,
                         start = ifelse(is.null(start), NA, start),
                         end = ifelse(is.null(end), NA, end),
                         width = ifelse(is.null(width), NA, width))
              } else {
                  if (!is.null(width)) {
                      if (is.null(start))
                          start <- end - width + 1L
                      else if (is.null(end))
                          end <- start + width - 1L
                  }
                  idx <-
                    window(seq_len(length(x)), start = start, end = end,
                           frequency = frequency, deltat = delta, ...)
                  attributes(idx) <- NULL
                  x[idx]
              }
          })

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Other logical data methods
###

setMethod("!", "Rle",
          function(x)
              Rle(values = !runValue(x), lengths = runLength(x), check = FALSE))

setMethod("which", "Rle",
          function(x, arr.ind = FALSE) {
              if (!is.logical(runValue(x)))
                  stop("argument to 'which' is not logical")
              ok <- runValue(x)
              ok[is.na(ok)] <- FALSE
              from <- start(x)[ok]
              to <- end(x)[ok]
              if (length(from) == 0)
                  integer(0)
              else mseq(from, to)
          })

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Other numerical data methods
###

setMethod("diff", "Rle",
          function(x, lag = 1, differences = 1)
          {
              if (!isSingleNumber(lag) || lag < 1L ||
                  !isSingleNumber(differences) || differences < 1L) 
                  stop("'lag' and 'differences' must be integers >= 1")
              lag <- as.integer(lag)
              differences <- as.integer(differences)
              if (lag * differences >= length(x))
                  return(Rle(vector(class(runValues(x)))))
              for (i in seq_len(differences)) {
                  n <- length(x)
                  x <- subseq(x, 1L + lag, n) - subseq(x, 1L, n - lag)
              }
              x
          })

setGeneric("pmax", signature = "...",
           function(..., na.rm = FALSE) standardGeneric("pmax"))
setMethod("pmax", "Rle",
          function(..., na.rm = FALSE)
          {
              rlist <- RleList(..., compress = FALSE)
              ends <- sort(unique(unlist(lapply(rlist, end))))
              Rle(values =  do.call(pmax, lapply(rlist, "[", ends, drop = TRUE)),
                  lengths = diff(c(0L, ends)), check = FALSE)
          })

setGeneric("pmin", signature = "...",
           function(..., na.rm = FALSE) standardGeneric("pmin"))
setMethod("pmin", "Rle",
          function(..., na.rm = FALSE)
          {
              rlist <- RleList(..., compress = FALSE)
              ends <- sort(unique(unlist(lapply(rlist, end))))
              Rle(values =  do.call(pmin, lapply(rlist, "[", ends, drop = TRUE)),
                  lengths = diff(c(0L, ends)), check = FALSE)
          })

setGeneric("pmax.int", signature = "...",
           function(..., na.rm = FALSE) standardGeneric("pmax.int"))
setMethod("pmax.int", "Rle",
          function(..., na.rm = FALSE)
          {
              rlist <- RleList(..., compress = FALSE)
              ends <- sort(unique(unlist(lapply(rlist, end))))
              Rle(values =  do.call(pmax.int, lapply(rlist, "[", ends, drop = TRUE)),
                  lengths = diff(c(0L, ends)), check = FALSE)
          })

setGeneric("pmin.int", signature = "...",
           function(..., na.rm = FALSE) standardGeneric("pmin.int"))
setMethod("pmin.int", "Rle",
          function(..., na.rm = FALSE)
          {
              rlist <- RleList(..., compress = FALSE)
              ends <- sort(unique(unlist(lapply(rlist, end))))
              Rle(values =  do.call(pmin.int, lapply(rlist, "[", ends, drop = TRUE)),
                  lengths = diff(c(0L, ends)), check = FALSE)
          })

setMethod("mean", "Rle",
          function(x, na.rm = FALSE)
          {
            if (na.rm)
                n <- length(x) - sum(runLength(x)[is.na(runValue(x))])
            else
                n <- length(x)
            sum(x, na.rm = na.rm) / n
          })

setMethod("var", signature = c(x = "Rle", y = "missing"),
          function(x, y = NULL, na.rm = FALSE, use)
          {
              if (na.rm)
                  n <- length(x) - sum(runLength(x)[is.na(runValue(x))])
              else
                  n <- length(x)
              sum((x - mean(x, na.rm = na.rm))^2, na.rm = na.rm) / (n - 1)
          })

setMethod("var", signature = c(x = "Rle", y = "Rle"),
          function(x, y = NULL, na.rm = FALSE, use)
          {
              z <- (x - mean(x, na.rm = na.rm)) * (y - mean(y, na.rm = na.rm))
              if (na.rm)
                  n <- length(z) - sum(runLength(z)[is.na(runValue(z))])
              else
                  n <- length(z)
              sum(z, na.rm = na.rm) / (n - 1)
          })

setMethod("cov", signature = c(x = "Rle", y = "Rle"),
          function(x, y = NULL, use = "everything",
                   method = c("pearson", "kendall", "spearman"))
          {
              use <-
                match.arg(use,
                          c("all.obs", "complete.obs", "pairwise.complete.obs",
                            "everything", "na.or.complete"))
              method <- match.arg(method)
              if (method != "pearson")
                  stop("only 'pearson' method is supported for Rle objects")
              na.rm <-
                use %in% c("complete.obs", "pairwise.complete.obs", "na.or.complete")
              if (use == "all.obs" && (any(is.na(x)) || any(is.na(y))))
                  stop("missing observations in cov/cor")
              var(x, y, na.rm = na.rm)
          })

setMethod("cor", signature = c(x = "Rle", y = "Rle"),
          function(x, y = NULL, use = "everything",
                   method = c("pearson", "kendall", "spearman"))
          {
              use <-
                match.arg(use,
                          c("all.obs", "complete.obs", "pairwise.complete.obs",
                            "everything", "na.or.complete"))
              method <- match.arg(method)
              if (method != "pearson")
                  stop("only 'pearson' method is supported for Rle objects")
              na.rm <-
                use %in% c("complete.obs", "pairwise.complete.obs", "na.or.complete")
              isMissing <- is.na(x) | is.na(y)
              if (any(isMissing)) {
                  if (use == "all.obs") {
                      stop("missing observations in cov/cor")
                  } else if (na.rm) {
                      x <- x[!isMissing]
                      y <- y[!isMissing]
                  }
              }
              var(x, y) / (sd(x) * sd(y))
          })

setMethod("sd", signature = c(x = "Rle"),
          function(x, na.rm = FALSE) sqrt(var(x, na.rm = na.rm)))

.medianDefault <- stats::median.default
environment(.medianDefault) <- globalenv()
setMethod("median", "Rle",
          function(x, na.rm = FALSE)
          {
              if (na.rm)
                  x <- x[!is.na(x)]
              oldOption <- getOption("dropRle")
              options("dropRle" = TRUE)
              on.exit(options("dropRle" = oldOption))
              .medianDefault(x, na.rm = FALSE)
          })

.quantileDefault <- stats::quantile.default
environment(.quantileDefault) <- globalenv()
setMethod("quantile", "Rle",
          function(x, probs = seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, 
                   type = 7, ...)
          {
              if (na.rm)
                  x <- x[!is.na(x)]
              oldOption <- getOption("dropRle")
              options("dropRle" = TRUE)
              on.exit(options("dropRle" = oldOption))
              .quantileDefault(x, probs = probs, na.rm = FALSE, names = names,
                               type = type, ...)
          })

.madDefault <- stats::mad
environment(.madDefault) <- globalenv()
setMethod("mad", "Rle",
          function(x, center = median(x), constant = 1.4826, na.rm = FALSE,
                   low = FALSE, high = FALSE)
          {
              if (na.rm)
                  x <- x[!is.na(x)]
              oldOption <- getOption("dropRle")
              options("dropRle" = TRUE)
              on.exit(options("dropRle" = oldOption))
              .madDefault(x, center = center, constant = constant, na.rm = FALSE, 
                          low = low, high = high)
          })

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Other character data methods
###

setMethod("nchar", "Rle",
          function(x, type = "chars", allowNA = FALSE)
              Rle(values = nchar(runValue(x), type = type, allowNA = allowNA),
                  lengths = runLength(x), check = FALSE))

setMethod("substr", "Rle",
          function(x, start, stop)
              Rle(values = substr(runValue(x), start = start, stop = stop),
                  lengths = runLength(x), check = FALSE))
setMethod("substring", "Rle",
          function(text, first, last = 1000000L)
              Rle(values = substring(runValue(text), first = first, last = last),
                  lengths = runLength(text), check = FALSE))

setMethod("chartr", c(old = "ANY", new = "ANY", x = "Rle"),
          function(old, new, x)
              Rle(values = chartr(old = old, new = new, x = runValue(x)),
                  lengths = runLength(x), check = FALSE))
setMethod("tolower", "Rle",
          function(x)
              Rle(values = tolower(runValue(x)), lengths = runLength(x),
                  check = FALSE))
setMethod("toupper", "Rle",
          function(x)
              Rle(values = toupper(runValue(x)), lengths = runLength(x),
                  check = FALSE))

setMethod("sub", signature = c(pattern = "ANY", replacement = "ANY", x = "Rle"),
          function(pattern, replacement, x, ignore.case = FALSE, extended = TRUE,
                   perl = FALSE, fixed = FALSE, useBytes = FALSE)
              Rle(values = sub(pattern = pattern, replacement = replacement,
                               x = runValue(x), ignore.case = ignore.case,
                               extended = extended, perl = perl, fixed = fixed,
                               useBytes = useBytes),
                  lengths = runLength(x), check = FALSE))
setMethod("gsub", signature = c(pattern = "ANY", replacement = "ANY", x = "Rle"),
          function(pattern, replacement, x, ignore.case = FALSE, extended = TRUE,
                   perl = FALSE, fixed = FALSE, useBytes = FALSE)
              Rle(values = gsub(pattern = pattern, replacement = replacement,
                                x = runValue(x), ignore.case = ignore.case,
                                extended = extended, perl = perl, fixed = fixed,
                                useBytes = useBytes),
                  lengths = runLength(x), check = FALSE))

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "show" method
###

setMethod("show", "Rle",
          function(object)
          {
              cat("  '", class(runValue(object)), "' Rle instance of length ", length(object),
                  " with ", nrun(object), ifelse(nrun(object) == 1, " run\n", " runs\n"),
                  sep = "")
              cat("  Lengths:  ")
              utils::str(runLength(object), give.head = FALSE)
              cat("  Values :  ")
              utils::str(as.vector(runValue(object)), give.head = FALSE)
          })
