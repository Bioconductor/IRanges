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
             if (length(object@.Data) != length(object@lengths))
                 "'.Data' and 'lengths' must have the same length"
             else
                 TRUE
         })

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
              n <- length(values)
              y <- values[-1L] != values[-n]
              i <- c(which(y | is.na(y)), n)
              new("Rle", values[i], lengths = diff(c(0L, cumsum(lengths)[i])))
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

setMethod("as.vector", c("Rle", "missing"), function(x, mode) rep(x@.Data, x@lengths))
setMethod("as.logical", "Rle", function(x) rep(as.logical(x@.Data), x@lengths))
setMethod("as.integer", "Rle", function(x) rep.int(as.integer(x@.Data), x@lengths))
setMethod("as.numeric", "Rle", function(x) rep(as.numeric(x@.Data), x@lengths))
setMethod("as.complex", "Rle", function(x) rep(as.complex(x@.Data), x@lengths))
setMethod("as.character", "Rle", function(x) rep(as.character(x@.Data), x@lengths))
setMethod("as.raw", "Rle", function(x) rep(as.raw(x@.Data), x@lengths))

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Basic methods
###

setMethod("length", "Rle", function(x) sum(x@lengths))

setMethod("[", "Rle",
          function(x, i, j, ..., drop=FALSE)
          {
              if (!missing(j) || length(list(...)) > 0)
                  stop("invalid subsetting")
              if (missing(i))
                  return(x)
              if (!is.atomic(i))
                  stop("invalid subscript type")
              lx <- length(x)
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
              breaks <- c(0L, cumsum(x@lengths))
              group <- findInterval(i - 1e-6, breaks)
              output <- x@.Data[group]
              if (!drop)
                  output <- Rle(output)
              output
          })

setMethod("subseq", "Rle",
          function(x, start=NA, end=NA, width=NA)
          {
              solved_SEW <- solveUserSEW(length(x), start=start, end=end, width=width)
              if (start(solved_SEW) > 1 || end(solved_SEW) < length(x)) {
                  breaks <- c(0L, cumsum(x@lengths))
                  rangeGroups <- findInterval(c(start(solved_SEW), end(solved_SEW)) - 1e-6, breaks)
                  lengths <- subseq(x@lengths, rangeGroups[1], rangeGroups[2])
                  lengths[1] <- breaks[rangeGroups[1] + 1L, drop = TRUE] - start(solved_SEW) + 1L
                  lengths[length(lengths)] <- end(solved_SEW) - breaks[rangeGroups[2], drop = TRUE]
                  x@lengths <- lengths
                  x@.Data <- subseq(x@.Data, rangeGroups[1], rangeGroups[2])
              }
              x
          })

setMethod("rep", "Rle",
          function(x, times, length.out, each)
          {
              if (!missing(each) && length(each) > 0) {
                  x@lengths <- x@lengths * as.integer(each[1])
              } else if (!missing(times) && length(times) > 0) {
                  times <- as.integer(times)
                  if (length(times) == length(x)) {
                      x@lengths <- x@lengths + diff(c(0L, cumsum(times)[cumsum(x@lengths)])) - 1L
                  } else if (length(times) == 1) {
                      x <- Rle(values  = rep(x@.Data, times = times),
                               lengths = rep(x@lengths, times = times))
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
              ends1 <- unique(c(cumsum(e1@lengths), n))
              ends2 <- unique(c(cumsum(e2@lengths), n))
              allEnds <- sort(union(ends1, ends2))
              lengths <- diff(c(0L, allEnds))
              values <- do.call(.Generic, list(e1[allEnds, drop = TRUE], e2[allEnds, drop = TRUE]))
              Rle(values = values, lengths = lengths)
          })

setMethod("Ops", signature(e1 = "Rle", e2 = "vector"),
          function(e1, e2) callNextMethod(e1, Rle(e2)))

setMethod("Ops", signature(e1 = "vector", e2 = "Rle"),
          function(e1, e2) callNextMethod(Rle(e1), e2))

setMethod("Math", "Rle", function(x)
          Rle(values = callNextMethod(x@.Data), lengths = x@lengths))

setMethod("Math2", "Rle", function(x, digits)
          {
              if (missing(digits))
                  digits <- ifelse(.Generic == "round", 0, 6)
              Rle(values = callNextMethod(x@.Data, digits = digits), lengths = x@lengths)
          })

setMethod("Summary", "Rle",
          function(x, ..., na.rm = FALSE)
          {
              switch(.Generic,
                     all=, any=, min=, max=, range=
                     do.call(.Generic, list(x@.Data, ..., na.rm = na.rm)),
                     sum = sum(x@.Data * x@lengths, ..., na.rm = na.rm),
                     prod = prod(x@.Data ^ x@lengths, ..., na.rm = na.rm))
          })

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "show" method.
###

setMethod("show", "Rle",
          function(object)
          {
              cat("  An Rle instance of length ", length(object),"\n", sep = "")
              cat("  Lengths:  ")
              utils::str(object@lengths, give.head = FALSE)
              cat("  Values :  ")
              utils::str(object@.Data, give.head = FALSE)
          })
