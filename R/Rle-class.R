### =========================================================================
### Rle objects
### -------------------------------------------------------------------------
###
### Class definitions
###

setClass("Rle",
         representation(lengths = "integer"),
         contains = c("Sequence", "vector"),
         validity = function(object)
         {
             if (length(runValue(object)) != length(runLength(object)))
                 "run values and run lengths must have the same length"
             else
                 TRUE
         })
 
### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Accessor methods.
###

setGeneric("runLength", signature = "x",
           function(x) standardGeneric("runLength"))
setMethod("runLength", "Rle", function(x) x@lengths)
 
setGeneric("runValue", signature = "x",
           function(x) standardGeneric("runValue"))
setMethod("runValue", "Rle", function(x) x@.Data)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Constructors
###

setGeneric("Rle", signature = c("values", "lengths"),
           function(values, lengths) standardGeneric("Rle"))

setMethod("Rle", signature = c(values = "vector", lengths = "missing"),
          function(values, lengths) {
              rleOutput <- rle(unname(values))
              new("Rle", rleOutput[["values"]], lengths = rleOutput[["lengths"]])
          })

setMethod("Rle", signature = c(values = "vector", lengths = "integer"),
          function(values, lengths) {
              if (length(values) != length(lengths))
                  stop("'values' and 'lengths' must have the same length")
              if (any(is.na(lengths)) || any(lengths < 0))
                  stop("'lengths' must contain all positive integers")
              zeros <- which(lengths == 0)
              if (length(zeros) > 0) {
                  values <- values[-zeros]
                  lengths <- lengths[-zeros]
              }
              n <- length(values)
              y <- values[-1L] != values[-n]
              i <- c(which(y | is.na(y)), n)
              new("Rle", values[i], lengths = diff(c(0L, cumsum(lengths)[i])))
          })

setMethod("Rle", signature = c(values = "vector", lengths = "numeric"),
          function(values, lengths) {
              Rle(values = values, lengths = as.integer(lengths))
          })

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion
###

setAs("vector", "Rle", function(from) Rle(from))

setAs("Rle", "vector", function(from) as.vector(from))
setAs("Rle","logical",  function(from) as.logical(from))
setAs("Rle", "integer", function(from) as.integer(from))
setAs("Rle", "numeric", function(from) as.numeric(from))
setAs("Rle", "complex", function(from) as.complex(from))
setAs("Rle", "character", function(from) as.character(from))
setAs("Rle", "raw", function(from) as.raw(from))

setMethod("as.vector", c("Rle", "missing"), function(x, mode) rep(runValue(x), runLength(x)))
setMethod("as.logical", "Rle", function(x) rep(as.logical(runValue(x)), runLength(x)))
setMethod("as.integer", "Rle", function(x) rep.int(as.integer(runValue(x)), runLength(x)))
setMethod("as.numeric", "Rle", function(x) rep(as.numeric(runValue(x)), runLength(x)))
setMethod("as.complex", "Rle", function(x) rep(as.complex(runValue(x)), runLength(x)))
setMethod("as.character", "Rle", function(x) rep(as.character(runValue(x)), runLength(x)))
setMethod("as.raw", "Rle", function(x) rep(as.raw(runValue(x)), runLength(x)))

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### General methods
###

setMethod("length", "Rle", function(x) sum(runLength(x)))

setMethod("c", "Rle", 
          function(x, ..., recursive = FALSE) {
            if (recursive)
              stop("'recursive' mode is not supported")
            args <- list(x, ...)
            if (!all(unlist(lapply(args, is, "Rle"))))
                stop("all arguments in '...' must be instances of 'Rle'")
            Rle(values  = unlist(lapply(args, slot, ".Data")),
                lengths = unlist(lapply(args, slot, "lengths")))
          })

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Subscript methods
###

setMethod("[", "Rle",
          function(x, i, j, ..., drop=FALSE)
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
                      starts <- cumsum(c(1L, runLength(i)))[runValue(i)]
                      widths <- runLength(i)[runValue(i)]
                      output <-
                        do.call(c,
                                lapply(seq_len(length(starts)),
                                       function(k)
                                       subseq(x, start = starts[k], width = widths[k])))
                  }
                  if (drop)
                      output <- as.vector(output)
              } else  if (is(i, "IRanges")) {
                  i <- restrict(i, start = 1, end = lx)
                  if (length(i) == 0) {
                      output <- new("Rle")
                  } else {
                      starts <- start(i)
                      widths <- width(i)
                      output <-
                        do.call(c,
                                lapply(seq_len(length(starts)),
                                       function(k)
                                       subseq(x, start = starts[k], width = widths[k])))
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
                  breaks <- c(0L, cumsum(runLength(x)))
                  group <- findInterval(i - 1e-6, breaks)
                  output <- runValue(x)[group]
                  if (!drop)
                      output <- Rle(output)
              }
              output
          })

setMethod("subseq", "Rle",
          function(x, start=NA, end=NA, width=NA)
          {
              solved_SEW <- solveUserSEW(length(x), start=start, end=end, width=width)
              if (start(solved_SEW) > 1 || end(solved_SEW) < length(x)) {
                  breaks <- c(0L, cumsum(runLength(x)))
                  rangeGroups <- findInterval(c(start(solved_SEW), end(solved_SEW)) - 1e-6, breaks)
                  lengths <- subseq(runLength(x), rangeGroups[1], rangeGroups[2])
                  lengths[1] <- breaks[rangeGroups[1] + 1L, drop = TRUE] - start(solved_SEW) + 1L
                  lengths[length(lengths)] <- end(solved_SEW) - breaks[rangeGroups[2], drop = TRUE]
                  x@lengths <- lengths
                  x@.Data <- subseq(runValue(x), rangeGroups[1], rangeGroups[2])
              }
              x
          })

setMethod("rev", "Rle",
          function(x)
          {
              x@lengths <- rev(runLength(x))
              x@.Data <- rev(runValue(x))
              x
          })

setMethod("rep", "Rle",
          function(x, times, length.out, each)
          {
              if (!missing(each) && length(each) > 0) {
                  x@lengths <- runLength(x) * as.integer(each[1])
              } else if (!missing(times) && length(times) > 0) {
                  times <- as.integer(times)
                  if (length(times) == length(x)) {
                      x@lengths <- runLength(x) + diff(c(0L, cumsum(times)[cumsum(runLength(x))])) - 1L
                  } else if (length(times) == 1) {
                      x <- Rle(values  = rep(runValue(x), times = times),
                               lengths = rep(runLength(x), times = times))
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

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Group generic methods
###

setMethod("Ops", signature(e1 = "Rle", e2 = "Rle"),
          function(e1, e2)
          {
              n1 <- length(e1)
              n2 <- length(e2)
              n <- max(n1, n2)
              if (max(n1, n2) %% min(n1, n2) != 0)
                  warning("longer object length is not a multiple of shorter object length")
              e1 <- rep(e1, length.out = n)
              e2 <- rep(e2, length.out = n)
              allEnds <- sort(unique(c(cumsum(runLength(e1)), cumsum(runLength(e2)))))
              lengths <- diff(c(0L, allEnds))
              values <- callGeneric(e1[allEnds, drop = TRUE], e2[allEnds, drop = TRUE])
              Rle(values = values, lengths = lengths)
          })

setMethod("Ops", signature(e1 = "Rle", e2 = "vector"),
          function(e1, e2) callGeneric(e1, Rle(e2)))

setMethod("Ops", signature(e1 = "vector", e2 = "Rle"),
          function(e1, e2) callGeneric(Rle(e1), e2))

setMethod("Math", "Rle", function(x)
          Rle(values = callGeneric(runValue(x)), lengths = runLength(x)))

setMethod("Math2", "Rle", function(x, digits)
          {
              if (missing(digits))
                  digits <- ifelse(.Generic == "round", 0, 6)
              Rle(values = callGeneric(runValue(x), digits = digits), lengths = runLength(x))
          })

setMethod("Summary", "Rle",
          function(x, ..., na.rm = FALSE)
          {
              switch(.Generic,
                     all=, any=, min=, max=, range=
                     callGeneric(runValue(x), ..., na.rm = na.rm),
                     sum = sum(runValue(x) * runLength(x), ..., na.rm = na.rm),
                     prod = prod(runValue(x) ^ runLength(x), ..., na.rm = na.rm))
          })

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Data manipulation methods
###

setMethod("!", "Rle", function(x) Rle(values = !runValue(x), lengths = runLength(x)))

setMethod("mean", "Rle",
          function(x, na.rm = FALSE)
          {
            if (na.rm)
                n <- length(x) - sum(runLength(x)[is.na(runValue(x))])
            else
                n <- length(x)
            sum(x, na.rm = na.rm) / n
          })

setMethod("median", signature = c(x = "Rle"),
          function(x, na.rm = FALSE)
          {
              nas <- which(is.na(runValue(x)))
              if (length(nas) > 0) {
                  if (na.rm) {
                      x@.Data <- runValue(x)[-nas]
                      x@lengths <- runLength(x)[-nas]
                  } else {
                      return(as(NA, class(runValue(x))))
                  }
              }
              n <- length(x)
              if (n == 0L) 
                  return(as(NA, class(runValue(x))))
              ord <- order(runValue(x))
              x@.Data <- runValue(x)[ord]
              x@lengths <- runLength(x)[ord]
              half <- (n + 1L) %/% 2L
              if (n %% 2L == 1L) 
                  x[half, drop = TRUE]
              else
                  sum(as.vector(subseq(x, half, half + 1L)))/2
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

setMethod("sd", signature = c(x = "Rle"),
          function(x, na.rm = FALSE) sqrt(var(x, na.rm = na.rm)))

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "show" method
###

setMethod("show", "Rle",
          function(object)
          {
              cat("  An Rle instance of length ", length(object),"\n", sep = "")
              cat("  Lengths:  ")
              utils::str(runLength(object), give.head = FALSE)
              cat("  Values :  ")
              utils::str(runValue(object), give.head = FALSE)
          })
