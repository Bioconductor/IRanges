### =========================================================================
### Rle objects
### -------------------------------------------------------------------------
###
### Class definitions
###

setClassUnion("vectorORfactor", c("vector", "factor"))

setClass("Rle",
         representation(values = "vectorORfactor",
                        lengths = "integer"),
         prototype = prototype(values = logical()),
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

setMethod("start", "Rle", function(x) .Call("Rle_start", x, PACKAGE="IRanges"))
setMethod("end", "Rle", function(x) .Call("Rle_end", x, PACKAGE="IRanges"))
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
          function(values, lengths)
              new2("Rle", values = vector(), lengths = integer(), check=FALSE))

setMethod("Rle", signature = c(values = "vectorORfactor", lengths = "missing"),
          function(values, lengths) Rle(values, integer(0), check = FALSE))

setMethod("Rle", signature = c(values = "vectorORfactor", lengths = "integer"),
          function(values, lengths, check = TRUE)
          {
              if (!isTRUEorFALSE(check))
                  stop("'check' must be TRUE or FALSE")
              if (check && length(lengths) > 0) {
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
              if (is.factor(values)) {
                  ans <-
                    .Call("Rle_constructor", as.integer(values), lengths,
                          PACKAGE="IRanges")
                  ans@values <-
                    factor(ans@values,
                           levels = seq_len(length(levels(values))),
                           labels = levels(values))
              } else {
                  ans <-
                   .Call("Rle_constructor", values, lengths, PACKAGE="IRanges")
              }
              ans
          })

setMethod("Rle", signature = c(values = "vectorORfactor", lengths = "numeric"),
          function(values, lengths, check = TRUE)
              Rle(values = values, lengths = as.integer(lengths),
                  check = check))

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion
###

setAs("vector", "Rle", function(from) Rle(from))
setAs("logical", "Rle", function(from) Rle(from))
setAs("integer", "Rle", function(from) Rle(from))
setAs("numeric", "Rle", function(from) Rle(from))
setAs("complex", "Rle", function(from) Rle(from))
setAs("character", "Rle", function(from) Rle(from))
setAs("raw", "Rle", function(from) Rle(from))
setAs("factor", "Rle", function(from) Rle(from))

setAs("Rle", "vector", function(from) as.vector(from))
setAs("Rle", "logical", function(from) as.logical(from))
setAs("Rle", "integer", function(from) as.integer(from))
setAs("Rle", "numeric", function(from) as.numeric(from))
setAs("Rle", "complex", function(from) as.complex(from))
setAs("Rle", "character", function(from) as.character(from))
setAs("Rle", "raw", function(from) as.raw(from))
setAs("Rle", "factor", function(from) as.factor(from))

setAs("Rle", "IRanges",
      function(from)
      {
          if (!is.logical(runValue(from)) || any(is.na(runValue(from))))
              stop("cannot coerce a non-logical 'Rle' or a logical 'Rle' ",
                   "with NAs to an IRanges object")
          keep <- runValue(from)
          ## The returned IRanges instance is guaranteed to be normal.
          ans_start <- start(from)[keep]
          ans_width <- runLength(from)[keep]
          new2("IRanges", start=ans_start, width=ans_width, check=FALSE)
      })

setAs("Rle", "NormalIRanges",
      function(from) newNormalIRangesFromIRanges(as(from, "IRanges"), check=FALSE))

setMethod("as.vector", c("Rle", "missing"), function(x, mode) rep.int(runValue(x), runLength(x)))
setMethod("as.logical", "Rle", function(x) rep.int(as.logical(runValue(x)), runLength(x)))
setMethod("as.integer", "Rle", function(x) rep.int(as.integer(runValue(x)), runLength(x)))
setMethod("as.numeric", "Rle", function(x) rep.int(as.numeric(runValue(x)), runLength(x)))
setMethod("as.complex", "Rle", function(x) rep.int(as.complex(runValue(x)), runLength(x)))
setMethod("as.character", "Rle", function(x) rep.int(as.character(runValue(x)), runLength(x)))
setMethod("as.raw", "Rle", function(x) rep.int(as.raw(runValue(x)), runLength(x)))
setMethod("as.factor", "Rle", function(x) rep.int(as.factor(runValue(x)), runLength(x)))

getStartEndRunAndOffset <- function(x, start, end) {
    infoStart <- findIntervalAndStartFromWidth(start, runLength(x))
    infoEnd <- findIntervalAndStartFromWidth(end, runLength(x))
    list(start =
         list(run = infoStart[["interval"]], offset = start - infoStart[["start"]]),
         end =
         list(run = infoEnd[["interval"]],
              offset = (infoEnd[["start"]] + runLength(x)[infoEnd[["interval"]]] - 1L) - end)
        )
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Group generic methods
###

.sumprodRle <- function(e1, e2, na.rm = FALSE)
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
        which1 <- findIntervalAndStartFromWidth(ends, runLength(e1))[["interval"]]
        which2 <- findIntervalAndStartFromWidth(ends, runLength(e2))[["interval"]]
    }
    lengths <- .Call("Integer_diff_with_0", ends, PACKAGE="IRanges")
    values <- runValue(e1)[which1] * runValue(e2)[which2]
    sum(lengths * values, na.rm = na.rm)
}

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
                  which1 <- findIntervalAndStartFromWidth(ends, runLength(e1))[["interval"]]
                  which2 <- findIntervalAndStartFromWidth(ends, runLength(e2))[["interval"]]
              }
              Rle(values = callGeneric(runValue(e1)[which1], runValue(e2)[which2]),
                  lengths = .Call("Integer_diff_with_0", ends, PACKAGE="IRanges"),
                  check = FALSE)
          })

setMethod("Ops", signature(e1 = "Rle", e2 = "vector"),
          function(e1, e2) callGeneric(e1, Rle(e2)))

setMethod("Ops", signature(e1 = "vector", e2 = "Rle"),
          function(e1, e2) callGeneric(Rle(e1), e2))

setMethod("Math", "Rle",
          function(x)
              switch(.Generic,
                     cumsum =
                     {
                         whichZero <- which(runValue(x) == 0)
                         widthZero <- runLength(x)[whichZero]
                         startZero <- cumsum(c(1L, runLength(x)))[whichZero]
                         y <- x
                         y@lengths[y@values == 0] <- 1L
                         values <- cumsum(as.vector(y))
                         lengths <- rep.int(1L, length(values))
                         lengths[startZero - c(0L, cumsum(head(widthZero, -1) - 1L))] <- widthZero
                         Rle(values = values, lengths = lengths, check = FALSE)
                     },
                     cumprod =
                     {
                         whichOne <- which(runValue(x) == 0)
                         widthOne <- runLength(x)[whichOne]
                         startOne <- cumsum(c(1L, runLength(x)))[whichOne]
                         y <- x
                         y@lengths[y@values == 0] <- 1L
                         values <- cumprod(as.vector(y))
                         lengths <- rep.int(1L, length(values))
                         lengths[startOne - c(0L, cumsum(head(widthOne, -1) - 1L))] <- widthOne
                         Rle(values = values, lengths = lengths, check = FALSE)
                     },
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
                                       window(x, start = starts[k], width = widths[k])))
                  }
                  if (drop)
                      output <- as.vector(output)
              } else if (is(i, "Ranges")) {
                  if (any(start(i) <= 0) || any(end(i) > length(x)))
                    stop("range index out of bounds")
                  i <- i[width(i) > 0]
                  if (length(i) == 0) {
                      output <- new("Rle")
                  } else {
                      start <- start(i)
                      end <- end(i)
                      from <- findIntervalAndStartFromWidth(start, runLength(x))
                      to <- findIntervalAndStartFromWidth(end, runLength(x))
                      runseq <- mseq(from[["interval"]], to[["interval"]])
                      lens <- runLength(x)[runseq]
                      breaks <- cumsum(c(1L, to[["interval"]] - from[["interval"]] + 1L))
                      lens[head(breaks, -1)] <-
                        pmin(end, (from[["start"]] + width(x)[from[["interval"]]] - 1L)) - start + 1L
                      lens[tail(breaks - 1L, -1)] <- end - pmax(start, to[["start"]]) + 1L
                      output <-
                        Rle(values  = runValue(x)[runseq],
                            lengths = lens, check = FALSE)
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
                  runs <- findIntervalAndStartFromWidth(i, runLength(x))[["interval"]]
                  output <- runValue(x)[runs]
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
                  start <- as(start, "integer")
                  end <- as(end, "integer")
              }
              if (length(start) != length(end))
                  stop("'start', 'end', and 'width' arguments have unequal length")
              n <- length(start)
              if (!is.null(names(start)))
                  indices <- structure(seq_len(n), names = names(start))
              else
                  indices <- structure(seq_len(n), names = names(end))
              if (is.null(frequency) && is.null(delta)) {
                  info <- getStartEndRunAndOffset(x, start, end)
                  runStart <- info[["start"]][["run"]]
                  offsetStart <- info[["start"]][["offset"]]
                  runEnd <- info[["end"]][["run"]]
                  offsetEnd <- info[["end"]][["offset"]]
                  ## Performance Optimization
                  ## Use a stripped down loop with empty Rle object
                  newRle <- new("Rle")
                  sapply(indices,
                         function(i)
                             FUN(.Call("Rle_window",
                                       x, runStart[i], runEnd[i],
                                       offsetStart[i], offsetEnd[i],
                                       newRle, PACKAGE = "IRanges"),
                                 ...),
                         simplify = simplify)
              } else {
                  frequency <- rep(frequency, length.out = n)
                  delta <- rep(delta, length.out = n)
                  sapply(indices,
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
              IRanges(start = start(vec)[run], width = width(vec)[run],
                      names = names(x))
          })

setGeneric("findRun", signature = "vec",
           function(x, vec) standardGeneric("findRun"))

setMethod("findRun", signature = c(vec = "Rle"),
          function(x, vec) {
            runs <-
              findIntervalAndStartFromWidth(as.integer(x),
                                            runLength(vec))[["interval"]]
            runs[x == 0 | x > length(vec)] <- NA
            runs
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
                      x@lengths <-
                        runLength(x) +
                          .Call("Integer_diff_with_0", cumsum(times)[end(x)],
                                PACKAGE="IRanges") - 1L
                  } else if (length(times) == 1) {
                      x <- Rle(values  = rep.int(runValue(x), times),
                               lengths = rep.int(runLength(x), times),
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
                      x <- window(x, 1, length.out)
                  } else if (length.out > n) {
                      x <- window(rep.int(x, ceiling(length.out / n)), 1, length.out)
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
                  x@lengths <-
                    runLength(x) +
                      .Call("Integer_diff_with_0", cumsum(times)[end(x)],
                            PACKAGE="IRanges") - 1L
              } else if (length(times) == 1) {
                  x <- Rle(values  = rep.int(runValue(x), times),
                           lengths = rep.int(runLength(x), times),
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

setMethod("seqselect", "Rle",
          function(x, start = NULL, end = NULL, width = NULL)
          {
              if (!is.null(start) && is.null(end) && is.null(width)) {
                  if (is(start, "Ranges"))
                      ir <- start
                  else {
                      if (is.logical(start) && length(start) != length(x))
                          start <- rep(start, length.out = length(x))
                      ir <- as(start, "IRanges")
                  }
              } else {
                  ir <- IRanges(start=start, end=end, width=width, names=NULL)
              }
              k <- length(ir)
              start <- start(ir)
              end <- end(ir)
              if (k == 1) {
                  window(x, start = start, end = end)
              } else {
                  if (any(start < 1L) || any(end > length(x)))
                      stop("some ranges are out of bounds")
                  info <- getStartEndRunAndOffset(x, start, end)
                  runStart <- info[["start"]][["run"]]
                  offsetStart <- info[["start"]][["offset"]]
                  runEnd <- info[["end"]][["run"]]
                  offsetEnd <- info[["end"]][["offset"]]
                  subseqs <-
                    lapply(seq_len(k), function(i)
                               .Call("Rle_window_aslist",
                                     x, runStart[i], runEnd[i],
                                     offsetStart[i], offsetEnd[i],
                                     PACKAGE = "IRanges"))
                  Rle(values  = unlist(lapply(subseqs, "[[", "values")),
                      lengths = unlist(lapply(subseqs, "[[", "lengths")),
                      check = FALSE)
              }
          })

setReplaceMethod("seqselect", "Rle",
                 function(x, start = NULL, end = NULL, width = NULL, value)
                 {
                     if (!is.null(value)) {
                         if (length(value) > 1)
                             stop("'value' must be of length 1 or 'NULL'")
                         
                         if (!is(value, class(x))) {
                             value <- try(as(value, class(x)), silent = TRUE)
                             if (inherits(value, "try-error"))
                                 stop("'value' must be a ", class(x),
                                      " object or NULL")
                         }
                         value <- as.vector(value)
                     }
                     if (!is.null(start) && is.null(end) && is.null(width)) {
                         if (is(start, "Ranges"))
                             ir <- start
                         else {
                             if (is.logical(start) && length(start) != length(x))
                                 start <- rep(start, length.out = length(x))
                             ir <- as(start, "IRanges")
                         }
                     } else {
                         ir <- IRanges(start=start, end=end, width=width, names=NULL)
                     }
                     ir <- reduce(ir)
                     if (any(start(ir) < 1L) || any(end(ir) > length(x)))
                         stop("some ranges are out of bounds")
                     valueWidths <- width(ir)
                     ir <- gaps(ir, start = 1, end = length(x))
                     if ((length(ir) == 0) || (start(ir)[1] != 1))
                         ir <- c(IRanges(start = 1, width = 0), ir)
                     if (end(ir[length(ir)]) != length(x))
                         ir <- c(ir, IRanges(start = length(x), width = 0))

                     k <- length(ir)
                     start <- start(ir)
                     end <- end(ir)
                     info <- getStartEndRunAndOffset(x, start, end)
                     runStart <- info[["start"]][["run"]]
                     offsetStart <- info[["start"]][["offset"]]
                     runEnd <- info[["end"]][["run"]]
                     offsetEnd <- info[["end"]][["offset"]]
                     subseqs <- vector("list", length(valueWidths) + k)
                     if (k > 0) {
                         subseqs[seq(1, length(subseqs), by = 2)] <-
                           lapply(seq_len(k), function(i)
                                      .Call("Rle_window_aslist",
                                            x, runStart[i], runEnd[i],
                                            offsetStart[i], offsetEnd[i],
                                            PACKAGE = "IRanges"))
                     }
                     if (length(valueWidths) > 0) {
                         subseqs[seq(2, length(subseqs), by = 2)] <-
                           lapply(seq_len(length(valueWidths)), function(i)
                                      list(values = value,
                                           lengths = valueWidths[i]))
                     }
                     Rle(values  = unlist(lapply(subseqs, "[[", "values")),
                         lengths = unlist(lapply(subseqs, "[[", "lengths")),
                         check = FALSE)
                 })

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
              infoX <-
                getStartEndRunAndOffset(X, rep.int(1L + OFFSET, length(SHIFT)),
                                        N - SHIFT)
              runStartX <- infoX[["start"]][["run"]]
              offsetStartX <- infoX[["start"]][["offset"]]
              runEndX <- infoX[["end"]][["run"]]
              offsetEndX <- infoX[["end"]][["offset"]]

              ## Perform Y setup
              infoY <-
                getStartEndRunAndOffset(Y, 1L + SHIFT,
                                        rep.int(N - OFFSET, length(SHIFT)))
              runStartY <- infoY[["start"]][["run"]]
              offsetStartY <- infoY[["start"]][["offset"]]
              runEndY <- infoY[["end"]][["run"]]
              offsetEndY <- infoY[["end"]][["offset"]]

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
                               FUN(.Call("Rle_window",
                                         X, runStartX[i], runEndX[i],
                                         offsetStartX[i], offsetEndX[i],
                                         newX, PACKAGE = "IRanges"),
                                   .Call("Rle_window",
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
                               FUN(.Call("Rle_window",
                                         X, runStartX[i], runEndX[i],
                                         offsetStartX[i], offsetEndX[i],
                                         newX, PACKAGE = "IRanges"),
                                   .Call("Rle_window",
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
              if (is.na(na.last)) {
                  na.last <- TRUE
                  if (any(is.na(x)))
                      x <- x[!is.na(x)]
              }
              ord <- order(runValue(x), decreasing = decreasing, na.last = na.last)
              Rle(values = runValue(x)[ord], lengths = runLength(x)[ord],
                  check = FALSE)
          })

setMethod("split", "Rle",
          function(x, f, drop=FALSE) {
              newCompressedList("CompressedRleList", x, splitFactor = f,
                                drop = drop)
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

setMethod("unique", "Rle",
          function(x, incomparables = FALSE, ...)
              unique(runValue(x), incomparables = incomparables, ...))

setMethod("window", "Rle",
          function(x, start = NULL, end = NULL, width = NULL,
                   frequency = NULL, delta = NULL, ...)
          {
              if ((!is.null(start) && !isSingleNumber(start)) ||
                  (!is.null(end) && !isSingleNumber(end)) ||
                  (!is.null(width) && !isSingleNumber(width)))
                  stop(paste("'start', 'end', and 'width' each must be",
                             "either NULL or a single number"))
              if (is.null(frequency) && is.null(delta)) {
                  solved_SEW <-
                    solveWindowSEW(length(x),
                                   start = ifelse(is.null(start), NA, start),
                                   end = ifelse(is.null(end), NA, end),
                                   width = ifelse(is.null(width), NA, width))
                  info <-
                    getStartEndRunAndOffset(x, start(solved_SEW), end(solved_SEW))
                  runStart <- info[["start"]][["run"]]
                  offsetStart <- info[["start"]][["offset"]]
                  runEnd <- info[["end"]][["run"]]
                  offsetEnd <- info[["end"]][["offset"]]
                  .Call("Rle_window",
                        x, runStart, runEnd, offsetStart, offsetEnd,
                        new("Rle"), PACKAGE = "IRanges")
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
                  return(Rle(vector(class(runValue(x)))))
              for (i in seq_len(differences)) {
                  n <- length(x)
                  x <- window(x, 1L + lag, n) - window(x, 1L, n - lag)
              }
              x
          })

.psummary.Rle <- function(FUN, ..., MoreArgs = NULL) {
    rlist <- RleList(..., compress = FALSE)
    ends <- end(rlist[[1]])
    if (length(rlist) > 1) {
        for (i in 2:length(rlist))
            ends <- .Call("Integer_sorted_merge", ends, end(rlist[[i]]),
                    PACKAGE="IRanges")
    }
    Rle(values =
        do.call(FUN,
                c(lapply(rlist,
                         function(x) {
                             runs <-
                               findIntervalAndStartFromWidth(ends,
                                       runLength(x))[["interval"]]
                             runValue(x)[runs]
                         }),
                 MoreArgs)),
        lengths = .Call("Integer_diff_with_0", ends, PACKAGE="IRanges"),
        check = FALSE)
}

setGeneric("pmax", signature = "...",
           function(..., na.rm = FALSE) standardGeneric("pmax"))
setMethod("pmax", "Rle", function(..., na.rm = FALSE)
            .psummary.Rle(pmax, ..., MoreArgs = list(na.rm = na.rm)))

setGeneric("pmin", signature = "...",
           function(..., na.rm = FALSE) standardGeneric("pmin"))
setMethod("pmin", "Rle", function(..., na.rm = FALSE)
            .psummary.Rle(pmin, ..., MoreArgs = list(na.rm = na.rm)))

setGeneric("pmax.int", signature = "...",
           function(..., na.rm = FALSE) standardGeneric("pmax.int"))
setMethod("pmax.int", "Rle", function(..., na.rm = FALSE)
            .psummary.Rle(pmax.int, ..., MoreArgs = list(na.rm = na.rm)))

setGeneric("pmin.int", signature = "...",
           function(..., na.rm = FALSE) standardGeneric("pmin.int"))
setMethod("pmin.int", "Rle", function(..., na.rm = FALSE)
            .psummary.Rle(pmin.int, ..., MoreArgs = list(na.rm = na.rm)))

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
              centeredValues <- runValue(x) - mean(x, na.rm = na.rm)
              sum(runLength(x) * centeredValues^2, na.rm = na.rm) / (n - 1)
          })

setMethod("var", signature = c(x = "Rle", y = "Rle"),
          function(x, y = NULL, na.rm = FALSE, use)
          {
              # Direct change to slots for fast computation
              x@values <- runValue(x) - mean(x, na.rm = na.rm)
              y@values <- runValue(y) - mean(y, na.rm = na.rm)
              z <- x * y
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
              # Direct change to slots for fast computation
              x@values <- runValue(x) - mean(x, na.rm = na.rm)
              y@values <- runValue(y) - mean(y, na.rm = na.rm)
              .sumprodRle(x, y, na.rm = na.rm) /
                  (sqrt(sum(runLength(x) * runValue(x)^2, na.rm = na.rm)) *
                   sqrt(sum(runLength(y) * runValue(y)^2, na.rm = na.rm)))
         })

setMethod("sd", signature = c(x = "Rle"),
          function(x, na.rm = FALSE) sqrt(var(x, na.rm = na.rm)))

.medianDefault <- stats::median.default
environment(.medianDefault) <- topenv()
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
environment(.quantileDefault) <- topenv()
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
environment(.madDefault) <- topenv()
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

setMethod("IQR", "Rle",
          function(x, na.rm = FALSE)
              diff(quantile(x, c(0.25, 0.75), na.rm = na.rm, names = FALSE)))

.smoothEndsDefault <- stats::smoothEnds
environment(.smoothEndsDefault) <- topenv()
setMethod("smoothEnds", "Rle", function(y, k = 3)
          {
              oldOption <- getOption("dropRle")
              options("dropRle" = TRUE)
              on.exit(options("dropRle" = oldOption))
              .smoothEndsDefault(y, k = k)
          })

setMethod("runmed", "Rle",
          function(x, k, endrule = c("median", "keep", "drop", "constant"),
                   algorithm = NULL, print.level = 0)
          {
              endrule <- match.arg(endrule)
              n <- length(x)
              k <- normargRunK(k = k, n = n, endrule = endrule)
              i <- (k + 1L) %/% 2L
              ans <- runq(x, k = k, i = i)
              if (endrule == "constant") {
                  runLength(ans)[1] <- runLength(ans)[1] + (i - 1L)
                  runLength(ans)[nrun(ans)] <-
                    runLength(ans)[nrun(ans)] + (i - 1L)
              } else if (endrule != "drop") {
                  ans <- c(head(x, i - 1L), ans, tail(x, i - 1L))
                  if (endrule == "median") {
                      ans <- smoothEnds(ans, k = k)
                  }
              }
              ans
          })

setMethod("runsum", "Rle",
          function(x, k, endrule = c("drop", "constant"))
          {
              endrule <- match.arg(endrule)
              n <- length(x)
              k <- normargRunK(k = k, n = n, endrule = endrule)
              ans <- .Call("Rle_runsum", x, as.integer(k), PACKAGE="IRanges")
              if (endrule == "constant") {
                  j <- (k + 1L) %/% 2L
                  runLength(ans)[1] <- runLength(ans)[1] + (j - 1L)
                  runLength(ans)[nrun(ans)] <-
                    runLength(ans)[nrun(ans)] + (j - 1L)
              }
              ans
          })

setMethod("runwtsum", "Rle",
          function(x, k, wt, endrule = c("drop", "constant"))
          {
              endrule <- match.arg(endrule)
              n <- length(x)
              k <- normargRunK(k = k, n = n, endrule = endrule)
              ans <-
                .Call("Rle_runwtsum", x, as.integer(k), as.numeric(wt),
                      PACKAGE="IRanges")
              if (endrule == "constant") {
                  j <- (k + 1L) %/% 2L
                  runLength(ans)[1] <- runLength(ans)[1] + (j - 1L)
                  runLength(ans)[nrun(ans)] <-
                    runLength(ans)[nrun(ans)] + (j - 1L)
              }
              ans
          })

setMethod("runq", "Rle",
          function(x, k, i, endrule = c("drop", "constant"))
          {
              endrule <- match.arg(endrule)
              n <- length(x)
              k <- normargRunK(k = k, n = n, endrule = endrule)
              ans <-
                .Call("Rle_runq", x, as.integer(k), as.integer(i),
                      PACKAGE="IRanges")
              if (endrule == "constant") {
                  j <- (k + 1L) %/% 2L
                  runLength(ans)[1] <- runLength(ans)[1] + (j - 1L)
                  runLength(ans)[nrun(ans)] <-
                    runLength(ans)[nrun(ans)] + (j - 1L)
              }
              ans
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
### Other factor data methods
###

setMethod("levels", "Rle", function(x) levels(runValue(x)))

setReplaceMethod("levels", "Rle",
                 function(x, value) {
                     levels(runValue(x)) <- value
                     x
                 })

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
