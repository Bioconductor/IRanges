### =========================================================================
### Rle objects
### -------------------------------------------------------------------------
###
### Class definitions
###

setClass("Rle",
         representation(values = "vectorORfactor",
                        lengths = "integer"),
         prototype = prototype(values = logical()),
         contains = "Vector",
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
             #      && any(run_values[-1L] == run_values[-length(run_values)]))
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

setMethod("start", "Rle", function(x) .Call2("Rle_start", x, PACKAGE="IRanges"))
setMethod("end", "Rle", function(x) .Call2("Rle_end", x, PACKAGE="IRanges"))
setMethod("width", "Rle", function(x) runLength(x))
setMethod("ranges", "Rle", function(x) IRanges(start(x), width = width(x)))

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
              ans <- .Call2("Rle_constructor",
                            values, lengths, check, 0L,
                            PACKAGE="IRanges")
              if (is.factor(values)) {
                  ans@values <-
                    factor(ans@values,
                           levels = seq_len(length(levels(values))),
                           labels = levels(values))
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
setAs("Rle", "list", function(from) as.list(from))
setAs("Rle", "data.frame", function(from) as.data.frame(from))

setAs("Rle", "IRanges",
      function(from)
      {
          if (!is.logical(runValue(from)) || anyMissing(runValue(from)))
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

setMethod("as.vector", "Rle", function(x, mode) rep.int(as.vector(runValue(x), mode), runLength(x)))
setMethod("as.logical", "Rle", function(x) rep.int(as.logical(runValue(x)), runLength(x)))
setMethod("as.integer", "Rle", function(x) rep.int(as.integer(runValue(x)), runLength(x)))
setMethod("as.numeric", "Rle", function(x) rep.int(as.numeric(runValue(x)), runLength(x)))
setMethod("as.complex", "Rle", function(x) rep.int(as.complex(runValue(x)), runLength(x)))
setMethod("as.character", "Rle", function(x) rep.int(as.character(runValue(x)), runLength(x)))
setMethod("as.raw", "Rle", function(x) rep.int(as.raw(runValue(x)), runLength(x)))
setMethod("as.factor", "Rle", function(x) rep.int(as.factor(runValue(x)), runLength(x)))
setMethod("as.list", "Rle", function(x) as.list(as.vector(x)))

setGeneric("as.vectorORfactor",
           function(x, ...) standardGeneric("as.vectorORfactor"))
setMethod("as.vectorORfactor", "Rle",
          function(x) rep.int(runValue(x), runLength(x)))

setMethod("as.data.frame", "Rle",
    function(x, row.names = NULL, optional = FALSE, ...)
    {
        value <- as.vectorORfactor(x)
        as.data.frame(value, row.names = row.names,
                      optional = optional, ...)
    }
)

getStartEndRunAndOffset <- function(x, start, end) {
    .Call2("Rle_getStartEndRunAndOffset", x, start, end, PACKAGE="IRanges")
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
        ends <- sortedMerge(end(e1), end(e2))
        which1 <- findIntervalAndStartFromWidth(ends, runLength(e1))[["interval"]]
        which2 <- findIntervalAndStartFromWidth(ends, runLength(e2))[["interval"]]
    }
    lengths <- diffWithInitialZero(ends)
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
                  ends <- sortedMerge(end(e1), end(e2))
                  which1 <- findIntervalAndStartFromWidth(ends, runLength(e1))[["interval"]]
                  which2 <- findIntervalAndStartFromWidth(ends, runLength(e2))[["interval"]]
              }
              Rle(values = callGeneric(runValue(e1)[which1], runValue(e2)[which2]),
                  lengths = diffWithInitialZero(ends), check = FALSE)
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
    {
        switch(.Generic,
        all =, any =, min =, max =, range =
            callGeneric(runValue(x), ..., na.rm=na.rm),
        sum = 
            withCallingHandlers({
                sum(runValue(x) * runLength(x), ..., na.rm=na.rm)
            }, warning=function(warn) {
                msg <- conditionMessage(warn)
                exp <- gettext("Integer overflow - use sum(as.numeric(.))",
                               domain="R")
                if (msg == exp) {
                    msg <- sub("sum\\(as.numeric\\(.\\)\\)",
                               "runValue(.) <- as.numeric(runValue(.))", msg)
                    warning(simpleWarning(msg, conditionCall(warn)))
                    invokeRestart("muffleWarning")
                } else {
                    warn
                }
            }), 
        prod = prod(runValue(x) ^ runLength(x), ..., na.rm=na.rm))
    }
) 

setMethod("Complex", "Rle",
          function(z)
              Rle(values = callGeneric(runValue(z)), lengths = runLength(z),
                  check = FALSE))

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### General methods
###

setMethod("[", "Rle",
          function(x, i, j, ..., drop = getOption("dropRle", FALSE))
          {
              if (!missing(j) || length(list(...)) > 0)
                  stop("invalid subsetting")
              if (!missing(i)) {
                  lx <- length(x)
                  iInfo <- .bracket.Index(i, lx, asRanges = TRUE)
                  if (!is.null(iInfo[["msg"]]))
                      stop(iInfo[["msg"]])
                  if (iInfo[["useIdx"]]) {
                      i <- iInfo[["idx"]]
                      ansList <-
                        .Call2("Rle_seqselect", x, start(i), width(i),
                              PACKAGE="IRanges")
                      if (is.factor(runValue(x))) {
                          attributes(ansList[["values"]]) <-
                            list(levels = levels(x), class = "factor")
                      }
                      if (drop) {
                          x <-
                            rep.int(ansList[["values"]], ansList[["lengths"]])
                      } else {
                          x <-
                            Rle(values  = ansList[["values"]],
                                lengths = ansList[["lengths"]])
                      }
                  } else if (drop) {
                      x <- as.vectorORfactor(x)
                  }
              }
              x
          })

setReplaceMethod("[", "Rle",
    function(x, i, j,..., value)
    {
        if (missing(i)) {
            if (length(value) <= 1L)
                return(callNextMethod(x = x, value = value))
            x <- as.vectorORfactor(x)
            value <- as.vector(value)
            return(Rle(callGeneric(x = x, value = value)))
        }
        if (length(value) <= 1L && length(i) != 0L)
            return(callNextMethod(x = x, i = i, value = value))
        x <- as.vectorORfactor(x)
        value <- as.vector(value)
        i <- as.vector(i)
        Rle(callGeneric(x = x, i = i, value = value))
    }
)

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
                  newRle <- new(class(x))
                  sapply(indices,
                         function(i)
                             FUN(.Call2("Rle_window",
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
              args <- RleList(unname(list(x, ...)))
              args <- args[elementLengths(args) > 0]
              if (length(args) == 0)
                  x
              else
                  Rle(values  = unlist(lapply(args, slot, "values")),
                      lengths = unlist(lapply(args, slot, "lengths")))
          })

setGeneric("findRange", signature = "vec",
           function(x, vec) standardGeneric("findRange"))

setMethod("findRange", signature = c(vec = "Rle"),
          function(x, vec) {
              run <- findRun(x, vec)
              if (anyMissing(run))
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

setMethod("is.unsorted", "Rle",
          function(x, na.rm = FALSE, strictly = FALSE)
          {
              ans <- is.unsorted(runValue(x), na.rm = na.rm, strictly = strictly)
              if (strictly && !ans)
                  ans <- any(runLength(x) > 1L)
              ans
          })

setMethod("length", "Rle", function(x) sum(runLength(x)))

setMethod("match", "Rle",
          function(x, table, nomatch = NA_integer_, incomparables = NULL)
              Rle(values =
                  match(runValue(x), table = table, nomatch = nomatch,
                        incomparables = incomparables), 
                  lengths = runLength(x), check = FALSE))

setGeneric("orderAsRanges", signature = c("x"),
           function(x, na.last = TRUE, decreasing = FALSE)
               standardGeneric("orderAsRanges"))

setMethod("orderAsRanges", "Rle",
           function(x, na.last = TRUE, decreasing = FALSE)
           {
               ord <- orderInteger(runValue(x), na.last = na.last,
                                   decreasing = decreasing)
               new2("IRanges", start = start(x)[ord], width = runLength(x)[ord],
                    check = FALSE)
           })

setMethod("rep", "Rle",
          function(x, times, length.out, each)
          {
              usedEach <- FALSE
              if (!missing(each) && length(each) > 0) {
                  each <- as.integer(each[1L])
                  if (!is.na(each)) {
                      if (each < 0)
                          stop("invalid 'each' argument")
                      usedEach <- TRUE
                      if (each == 0)
                          x <- new(class(x), values = runValue(x)[0L])
                      else
                          x@lengths <- each[1L] * runLength(x)
                  }
              }
              if (!missing(length.out) && length(length.out) > 0) {
                  n <- length(x)
                  length.out <- as.integer(length.out[1L])
                  if (!is.na(length.out)) {
                      if (length.out == 0) {
                          x <- new(class(x), values = runValue(x)[0L])
                      } else if (length.out < n) {
                          x <- window(x, 1, length.out)
                      } else if (length.out > n) {
                          x <-
                            window(rep.int(x, ceiling(length.out / n)),
                                   1, length.out)
                      }
                  }
              } else if (!missing(times)) {
                  if (usedEach && length(times) != 1)
                      stop("invalid 'times' argument")
                  x <- rep.int(x, times)
              }
              x
          })

setMethod("rep.int", "Rle",
          function(x, times)
          {
              n <- length(x)
              if (!is.integer(times))
                  times <- as.integer(times)
              if ((length(times) > 1 && length(times) < n) ||
                  anyMissingOrOutside(times, 0L))
                  stop("invalid 'times' argument")
              if (length(times) == n) {
                  runLength(x) <- diffWithInitialZero(cumsum(times)[end(x)])
              } else if (length(times) == 1) {
                  x <- Rle(values  = rep.int(runValue(x), times),
                           lengths = rep.int(runLength(x), times))
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
              if (!is.null(end) || !is.null(width))
                  start <- IRanges(start = start, end = end, width = width)
              x[start]
          })

setReplaceMethod("seqselect", "Rle",
                 function(x, start = NULL, end = NULL, width = NULL, value)
                 {
                     isFactorRle <- is.factor(runValue(x))
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
                         if (isFactorRle)
                             value <- factor(value, levels = levels(x))
                     }
                     if (is.null(end) && is.null(width)) {
                         if (is.null(start))
                             ir <- IRanges(start = 1, width = length(x))
                         else if (is(start, "Ranges"))
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
                     if (length(ir) == 0)
                         return(x)
                     if (anyMissingOrOutside(start(ir), 1L, length(x)) ||
                         anyMissingOrOutside(end(ir), 1L, length(x)))
                         stop("some ranges are out of bounds")
                     valueWidths <- width(ir)
                     ir <- gaps(ir, start = 1, end = length(x))

                     k <- length(ir)
                     start <- start(ir)
                     end <- end(ir)
                     info <- getStartEndRunAndOffset(x, start, end)
                     runStart <- info[["start"]][["run"]]
                     offsetStart <- info[["start"]][["offset"]]
                     runEnd <- info[["end"]][["run"]]
                     offsetEnd <- info[["end"]][["offset"]]

                     if ((length(ir) == 0) || (start(ir)[1L] != 1)) {
                         k <- k + 1L
                         runStart <- c(1L, runStart)
                         offsetStart <- c(0L, offsetStart)
                         runEnd <- c(0L, runEnd)
                         offsetEnd <- c(0L, offsetEnd)
                     } 
                     if ((length(ir) > 0) &&
                         (end(ir[length(ir)]) != length(x))) {
                         k <- k + 1L
                         runStart <- c(runStart, 1L)
                         offsetStart <- c(offsetStart, 0L)
                         runEnd <- c(runEnd, 0L)
                         offsetEnd <- c(offsetEnd, 0L)
                     }

                     subseqs <- vector("list", length(valueWidths) + k)
                     if (k > 0) {
                         if (!isFactorRle)
                             subseqs[seq(1, length(subseqs), by = 2)] <-
                               lapply(seq_len(k), function(i)
                                          .Call2("Rle_window_aslist",
                                                x, runStart[i], runEnd[i],
                                                offsetStart[i], offsetEnd[i],
                                                PACKAGE = "IRanges"))
                         else
                             subseqs[seq(1, length(subseqs), by = 2)] <-
                               lapply(seq_len(k), function(i) {
                                          ans <-
                                            .Call2("Rle_window_aslist",
                                                  x, runStart[i], runEnd[i],
                                                  offsetStart[i], offsetEnd[i],
                                                  PACKAGE = "IRanges")
                                          ans[["values"]] <-
                                            factor(levels(x),
                                                   levels = levels(x))[ans[["values"]]]
                                          ans
                                      })
                     }
                     if (length(valueWidths) > 0) {
                         subseqs[seq(2, length(subseqs), by = 2)] <-
                           lapply(seq_len(length(valueWidths)), function(i)
                                      list(values = value,
                                           lengths = valueWidths[i]))
                     }
                     values <- unlist(lapply(subseqs, "[[", "values"))
                     if (isFactorRle)
                         values <- factor(levels(x), levels = levels(x))[values]
                     Rle(values  = values,
                         lengths = unlist(lapply(subseqs, "[[", "lengths")))
                 })

setMethod("shiftApply", signature(X = "Rle", Y = "Rle"),
          function(SHIFT, X, Y, FUN, ..., OFFSET = 0L, simplify = TRUE,
                   verbose = FALSE)
          {
              FUN <- match.fun(FUN)
              N <- length(X)
              if (N != length(Y))
                  stop("'X' and 'Y' must be of equal length")

              if (!is.integer(SHIFT))
                  SHIFT <- as.integer(SHIFT)
              if (length(SHIFT) == 0 || anyMissingOrOutside(SHIFT, 0L))
                  stop("all 'SHIFT' values must be non-negative")

              if (!is.integer(OFFSET))
                  OFFSET <- as.integer(OFFSET)
              if (length(OFFSET) == 0 || anyMissingOrOutside(OFFSET, 0L))
                  stop("'OFFSET' must be non-negative")

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
                               FUN(.Call2("Rle_window",
                                         X, runStartX[i], runEndX[i],
                                         offsetStartX[i], offsetEndX[i],
                                         newX, PACKAGE = "IRanges"),
                                   .Call2("Rle_window",
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
                               FUN(.Call2("Rle_window",
                                         X, runStartX[i], runEndX[i],
                                         offsetStartX[i], offsetEndX[i],
                                         newX, PACKAGE = "IRanges"),
                                   .Call2("Rle_window",
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
                  if (anyMissing(x))
                      x <- x[!is.na(x)]
              }
              if (is.integer(runValue(x)) || is.factor(runValue(x))) {
                  ord <- orderInteger(runValue(x), decreasing = decreasing,
                                      na.last = na.last)
              } else {
                  ord <-
                    order(runValue(x), decreasing = decreasing,
                          na.last = na.last)
              }
              Rle(values = runValue(x)[ord], lengths = runLength(x)[ord],
                  check = FALSE)
          })

setGeneric("splitRanges", signature = "x",
           function(x) standardGeneric("splitRanges"))

setMethod("splitRanges", "Rle",
          function(x) {
              split(IRanges(start = start(x), width = runLength(x)),
                    runValue(x))
          })

setMethod("splitRanges", "vectorORfactor",
          function(x) {
              callGeneric(Rle(x))
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
              class(value) <- c("summaryDefault", "table")
              value
          })

setMethod("table", "Rle",
          function(...)
          {
            ## idea for doing this over multiple Rles
            ## use disjoin(), findRun() to find matching runs,
            ## then xtabs(length ~ value...)
              x <- sort(...)
              structure(array(runLength(x), dim = nrun(x),
                              dimnames =
                              structure(list(as.character(runValue(x))), 
                                        names = "")),
                        class = "table")
          })

setMethod("unique", "Rle",
          function(x, incomparables = FALSE, ...)
              unique(runValue(x), incomparables = incomparables, ...))

setMethod("window", "Rle",
          function(x, start = NA, end = NA, width = NA,
                   frequency = NULL, delta = NULL, ...)
          {
              solved_SEW <- solveWindowSEW(length(x), start, end, width)
              if (is.null(frequency) && is.null(delta)) {
                  info <-
                    getStartEndRunAndOffset(x, start(solved_SEW), end(solved_SEW))
                  runStart <- info[["start"]][["run"]]
                  offsetStart <- info[["start"]][["offset"]]
                  runEnd <- info[["end"]][["run"]]
                  offsetEnd <- info[["end"]][["offset"]]
                  ans <-
                    .Call2("Rle_window",
                          x, runStart, runEnd, offsetStart, offsetEnd,
                          new("Rle"), PACKAGE = "IRanges")
                  if (is.factor(runValue(x)))
                      attributes(runValue(ans)) <-
                        list(levels = levels(x), class = "factor")
                  ans
              } else {
                  idx <-
                    stats:::window.default(seq_len(length(x)),
                                           start = start(solved_SEW),
                                           end = end(solved_SEW),
                                           frequency = frequency,
                                           deltat = delta, ...)
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

setMethod("which.max", "Rle",
          function(x) {
            start(x)[which.max(runValue(x))]
          })

setMethod("ifelse", "Rle", function (test, yes, no) 
          {
            test <- as.vector(test)
            yes <- as.vector(yes)
            no <- as.vector(no)
            as(callGeneric(), "Rle")
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
    ends <- end(rlist[[1L]])
    if (length(rlist) > 1) {
        for (i in 2:length(rlist))
            ends <- sortedMerge(ends, end(rlist[[i]]))
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
        lengths = diffWithInitialZero(ends), check = FALSE)
}

setMethod("pmax", "Rle", function(..., na.rm = FALSE)
            .psummary.Rle(pmax, ..., MoreArgs = list(na.rm = na.rm)))

setMethod("pmin", "Rle", function(..., na.rm = FALSE)
            .psummary.Rle(pmin, ..., MoreArgs = list(na.rm = na.rm)))

setMethod("pmax.int", "Rle", function(..., na.rm = FALSE)
            .psummary.Rle(pmax.int, ..., MoreArgs = list(na.rm = na.rm)))

setMethod("pmin.int", "Rle", function(..., na.rm = FALSE)
            .psummary.Rle(pmin.int, ..., MoreArgs = list(na.rm = na.rm)))

setMethod("mean", "Rle",
          function(x, na.rm = FALSE)
          {
            if (is.integer(runValue(x)))
                runValue(x) <- as.double(runValue(x))
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
              sum(runLength(x) * centeredValues * centeredValues,
                  na.rm = na.rm) / (n - 1)
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
              if (use == "all.obs" && (anyMissing(x) || anyMissing(y)))
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
                  (sqrt(sum(runLength(x) * runValue(x) * runValue(x),
                            na.rm = na.rm)) *
                   sqrt(sum(runLength(y) * runValue(y) * runValue(y),
                            na.rm = na.rm)))
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

setMethod("runmean", "Rle",
          function(x, k, endrule = c("drop", "constant"), na.rm = FALSE)
          {
              sums <- runsum(x, k, endrule, na.rm)
              if (na.rm) {
                  d <- Rle(rep(1L, length(x)))
                  d[is.na(x)] <- 0L 
                  sums / runsum(d, k, endrule, na.rm)
              } else {
                  sums / k
              }
          })

setMethod("runmed", "Rle",
          function(x, k, endrule = c("median", "keep", "drop", "constant"),
                   algorithm = NULL, print.level = 0)
          {
              if (!all(is.finite(as.vector(x))))
                  stop("NA/NaN/Inf not supported in runmed,Rle-method")
              endrule <- match.arg(endrule)
              n <- length(x)
              k <- normargRunK(k = k, n = n, endrule = endrule)
              i <- (k + 1L) %/% 2L
              ans <- runq(x, k = k, i = i)
              if (endrule == "constant") {
                  runLength(ans)[1L] <- runLength(ans)[1L] + (i - 1L)
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
          function(x, k, endrule = c("drop", "constant"), na.rm = FALSE)
          {
              endrule <- match.arg(endrule)
              n <- length(x)
              k <- normargRunK(k = k, n = n, endrule = endrule)
              ans <- .Call2("Rle_runsum", x, as.integer(k), as.logical(na.rm), 
                            PACKAGE="IRanges")
              if (endrule == "constant") {
                  j <- (k + 1L) %/% 2L
                  runLength(ans)[1L] <- runLength(ans)[1L] + (j - 1L)
                  runLength(ans)[nrun(ans)] <-
                    runLength(ans)[nrun(ans)] + (j - 1L)
              }
              ans
          })

setMethod("runwtsum", "Rle",
          function(x, k, wt, endrule = c("drop", "constant"), na.rm = FALSE)
          {
              endrule <- match.arg(endrule)
              n <- length(x)
              k <- normargRunK(k = k, n = n, endrule = endrule)
              ans <-
                .Call2("Rle_runwtsum", x, as.integer(k), as.numeric(wt),
                      as.logical(na.rm), PACKAGE="IRanges")
              if (endrule == "constant") {
                  j <- (k + 1L) %/% 2L
                  runLength(ans)[1L] <- runLength(ans)[1L] + (j - 1L)
                  runLength(ans)[nrun(ans)] <-
                    runLength(ans)[nrun(ans)] + (j - 1L)
              }
              ans
          })

setMethod("runq", "Rle",
          function(x, k, i, endrule = c("drop", "constant"), na.rm = FALSE)
          {
              endrule <- match.arg(endrule)
              n <- length(x)
              k <- normargRunK(k = k, n = n, endrule = endrule)
              ans <-
                .Call2("Rle_runq", x, as.integer(k), as.integer(i), 
                      as.logical(na.rm), PACKAGE="IRanges")
              if (endrule == "constant") {
                  j <- (k + 1L) %/% 2L
                  runLength(ans)[1L] <- runLength(ans)[1L] + (j - 1L)
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
          {
              if (is.factor(runValue(x))) {
                  levels(x) <- substr(levels(x), start = start, stop = stop)
              } else {
                  runValue(x) <- substr(runValue(x), start = start, stop = stop)
              }
              x
          })
setMethod("substring", "Rle",
          function(text, first, last = 1000000L)
          {
              if (is.factor(runValue(text))) {
                  levels(text) <-
                    substring(levels(text), first = first, last = last)
              } else {
                  runValue(text) <-
                    substring(runValue(text), first = first, last = last)
              }
              text
          })

setMethod("chartr", c(old = "ANY", new = "ANY", x = "Rle"),
          function(old, new, x)
          {
              if (is.factor(runValue(x))) {
                  levels(x) <- chartr(old = old, new = new, levels(x))
              } else {
                  runValue(x) <- chartr(old = old, new = new, runValue(x))
              }
              x
          })
setMethod("tolower", "Rle",
          function(x) {
              if (is.factor(runValue(x))) {
                  levels(x) <- tolower(levels(x))
              } else {
                  runValue(x) <- tolower(runValue(x))
              }
              x
          })
setMethod("toupper", "Rle",
          function(x)
          {
              if (is.factor(runValue(x))) {
                  levels(x) <- toupper(levels(x))
              } else {
                  runValue(x) <- toupper(runValue(x))
              }
              x
          })

setMethod("sub", signature = c(pattern = "ANY", replacement = "ANY", x = "Rle"),
          function(pattern, replacement, x, ignore.case = FALSE,
                   perl = FALSE, fixed = FALSE, useBytes = FALSE)
          {
              if (is.factor(runValue(x))) {
                  levels(x) <-
                    sub(pattern = pattern, replacement = replacement,
                        x = levels(x), ignore.case = ignore.case,
                        perl = perl, fixed = fixed, useBytes = useBytes)
              } else {
                  runValue(x) <-
                    sub(pattern = pattern, replacement = replacement,
                        x = runValue(x), ignore.case = ignore.case,
                        perl = perl, fixed = fixed, useBytes = useBytes)
              }
              x
          })
setMethod("gsub", signature = c(pattern = "ANY", replacement = "ANY", x = "Rle"),
          function(pattern, replacement, x, ignore.case = FALSE,
                   perl = FALSE, fixed = FALSE, useBytes = FALSE)
          {
              if (is.factor(runValue(x))) {
                  levels(x) <-
                    gsub(pattern = pattern, replacement = replacement,
                         x = levels(x), ignore.case = ignore.case,
                         perl = perl, fixed = fixed, useBytes = useBytes)
              } else {
                  runValue(x) <-
                    gsub(pattern = pattern, replacement = replacement,
                         x = runValue(x), ignore.case = ignore.case,
                         perl = perl, fixed = fixed, useBytes = useBytes)
              }
              x
          })

.pasteTwoRles <- function(e1, e2, sep = " ", collapse = NULL)
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
        ends <- sortedMerge(end(e1), end(e2))
        which1 <- findIntervalAndStartFromWidth(ends, runLength(e1))[["interval"]]
        which2 <- findIntervalAndStartFromWidth(ends, runLength(e2))[["interval"]]
    }
    if (is.null(collapse) &&
        is.factor(runValue(e1)) && is.factor(runValue(e2))) {
        levelsTable <-
          expand.grid(levels(e2), levels(e1), KEEP.OUT.ATTRS = FALSE,
                      stringsAsFactors = FALSE)
        values <-
          structure((as.integer(runValue(e1)[which1]) - 1L) * nlevels(e2) +
                    as.integer(runValue(e2)[which2]),
                    levels =
                    paste(levelsTable[[2L]], levelsTable[[1L]], sep = sep),
                    class = "factor")
    } else {
        values <-
          paste(runValue(e1)[which1], runValue(e2)[which2], sep = sep,
                collapse = collapse)
    }
    Rle(values = values, lengths = diffWithInitialZero(ends), check = FALSE)
}

setMethod("paste", "Rle",
          function(..., sep = " ", collapse = NULL)
          {
              rleList <- RleList(...)
              ans <- rleList[[1L]]
              if (length(rleList) > 1) {
                  for (i in 2:length(rleList)) {
                      ans <-
                        .pasteTwoRles(ans, rleList[[i]], sep = sep,
                                      collapse = collapse)
                  }
              }
              ans
          })

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Other factor data methods
###

setMethod("levels", "Rle", function(x) levels(runValue(x)))

setReplaceMethod("levels", "Rle",
                 function(x, value) {
                     levels(x@values) <- value
                     if (anyDuplicated(value)) {
                         x <-
                           Rle(values = runValue(x), lengths = runLength(x),
                               check = FALSE)
                     }
                     x
                 })

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "show" method
###

setMethod("show", "Rle",
          function(object)
          {
              lo <- length(object)
              nr <- nrun(object)
              halfWidth <- getOption("width") %/% 2L
              cat(class(runValue(object)), "-Rle of length ", lo,
                  " with ", nr, ifelse(nr == 1, " run\n", " runs\n"),
                  sep = "")
              first <- max(1L, halfWidth)
              showMatrix <-
                rbind(as.character(head(runLength(object), first)),
                      as.character(head(runValue(object), first)))
              if (nr > first) {
                  last <- min(nr - first, halfWidth)
                  showMatrix <-
                    cbind(showMatrix,
                          rbind(as.character(tail(runLength(object), last)),
                                as.character(tail(runValue(object), last))))
              }
              if (is.character(runValue(object))) {
                  showMatrix[2L,] <-
                    paste("\"", showMatrix[2L,], "\"", sep = "")
              }
              showMatrix <- format(showMatrix, justify = "right")
              cat(labeledLine("  Lengths", showMatrix[1L,], count = FALSE))
              cat(labeledLine("  Values ", showMatrix[2L,], count = FALSE))
              if (is.factor(runValue(object)))
                  cat(labeledLine("Levels", levels(object)))
          })

setMethod("showAsCell", "Rle", function(object) as.vector(object))
