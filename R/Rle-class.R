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

setGeneric("nrun", signature = "x", function(x) standardGeneric("nrun"))
setMethod("nrun", "Rle", function(x) length(runLength(x)))

setMethod("start", "Rle", function(x) cumsum(c(1L, runLength(x))[seq_len(nrun(x))]))
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
                  breaks <- c(0L, end(x))
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
                  breaks <- c(0L, end(x))
                  rangeGroups <- findInterval(c(start(solved_SEW), end(solved_SEW)) - 1e-6, breaks)
                  lengths <- subseq(runLength(x), rangeGroups[1], rangeGroups[2])
                  lengths[1] <- breaks[rangeGroups[1] + 1L, drop = TRUE] - start(solved_SEW) + 1L
                  if (length(lengths) > 1)
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

setMethod("sort", "Rle",
          function(x, decreasing = FALSE, na.last = NA, ...) {
              ord <- order(runValue(x), decreasing = decreasing, na.last = na.last)
              Rle(values = runValue(x)[ord], lengths = runLength(x)[ord])
          })

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
                               lengths = rep.int(runLength(x), times = times))
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
                           lengths = rep.int(runLength(x), times = times))
              } else {
                  stop("invalid 'times' argument")
              }
              x
          })

setGeneric("table", signature = "...",
           function(...) standardGeneric("table"),
           useAsDefault = function(...) base::table(...))
setMethod("table", "Rle",
          function(...) {
              x <- sort(...)
              structure(array(runLength(x), dim = nrun(x),
                              dimnames = structure(list(as.character(runValue(x))), 
                                                   names = "")),
                        class = "table")
          })

setMethod("is.na", "Rle",
          function(x) Rle(values = is.na(runValue(x)), lengths = runLength(x)))

setMethod("%in%", "Rle",
          function(x, table) Rle(values = runValue(x) %in% table, lengths = runLength(x)))

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
                else if (is.numeric(object)) {
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
              allEnds <- sort(unique(c(end(e1), end(e2))))
              lengths <- diff(c(0L, allEnds))
              values <- callGeneric(e1[allEnds, drop = TRUE], e2[allEnds, drop = TRUE])
              Rle(values = values, lengths = lengths)
          })

setMethod("Ops", signature(e1 = "Rle", e2 = "vector"),
          function(e1, e2) callGeneric(e1, Rle(e2)))

setMethod("Ops", signature(e1 = "vector", e2 = "Rle"),
          function(e1, e2) callGeneric(Rle(e1), e2))

setMethod("Math", "Rle",
          function(x) {
              switch(.Generic,
                     cumsum =, cumprod = callGeneric(as.vector(x)),
                     Rle(values = callGeneric(runValue(x)), lengths = runLength(x)))
          })

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
                     all =, any =, min =, max =, range =
                     callGeneric(runValue(x), ..., na.rm = na.rm),
                     sum = sum(runValue(x) * runLength(x), ..., na.rm = na.rm),
                     prod = prod(runValue(x) ^ runLength(x), ..., na.rm = na.rm))
          })

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Other logical data methods
###

setMethod("!", "Rle", function(x) Rle(values = !runValue(x), lengths = runLength(x)))

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Other numerical data methods
###

setMethod("diff", "Rle",
          function(x, lag = 1, differences = 1) {
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
          function(..., na.rm = FALSE) {
              rlist <- RleList(..., compress = FALSE)
              ends <- sort(unique(unlist(lapply(rlist, end))))
              Rle(values =  do.call(pmax, lapply(rlist, "[", ends, drop = TRUE)),
                  lengths = diff(c(0L, ends)))
          })

setGeneric("pmin", signature = "...",
           function(..., na.rm = FALSE) standardGeneric("pmin"))
setMethod("pmin", "Rle",
          function(..., na.rm = FALSE) {
              rlist <- RleList(..., compress = FALSE)
              ends <- sort(unique(unlist(lapply(rlist, end))))
              Rle(values =  do.call(pmin, lapply(rlist, "[", ends, drop = TRUE)),
                  lengths = diff(c(0L, ends)))
          })

setGeneric("pmax.int", signature = "...",
           function(..., na.rm = FALSE) standardGeneric("pmax.int"))
setMethod("pmax.int", "Rle",
          function(..., na.rm = FALSE) {
              rlist <- RleList(..., compress = FALSE)
              ends <- sort(unique(unlist(lapply(rlist, end))))
              Rle(values =  do.call(pmax.int, lapply(rlist, "[", ends, drop = TRUE)),
                  lengths = diff(c(0L, ends)))
          })

setGeneric("pmin.int", signature = "...",
           function(..., na.rm = FALSE) standardGeneric("pmin.int"))
setMethod("pmin.int", "Rle",
          function(..., na.rm = FALSE) {
              rlist <- RleList(..., compress = FALSE)
              ends <- sort(unique(unlist(lapply(rlist, end))))
              Rle(values =  do.call(pmin.int, lapply(rlist, "[", ends, drop = TRUE)),
                  lengths = diff(c(0L, ends)))
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
                   type = 7, ...) {
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
                   low = FALSE, high = FALSE) {
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
                  lengths = runLength(x)))

setMethod("substr", "Rle",
          function(x, start, stop)
              Rle(values = substr(runValue(x), start = start, stop = stop),
                  lengths = runLength(x)))
setMethod("substring", "Rle",
          function(text, first, last = 1000000L)
              Rle(values = substring(runValue(text), first = first, last = last),
                  lengths = runLength(text)))

setMethod("chartr", c(old = "ANY", new = "ANY", x = "Rle"),
          function(old, new, x)
              Rle(values = chartr(old = old, new = new, x = runValue(x)),
                  lengths = runLength(x)))
setMethod("tolower", "Rle",
          function(x) Rle(values = tolower(runValue(x)), lengths = runLength(x)))
setMethod("toupper", "Rle",
         function(x) Rle(values = toupper(runValue(x)), lengths = runLength(x)))

setMethod("gsub", signature = c(pattern = "ANY", replacement = "ANY", x = "Rle"),
          function(pattern, replacement, x, ignore.case = FALSE, extended = TRUE,
                   perl = FALSE, fixed = FALSE, useBytes = FALSE)
              Rle(values = gsub(pattern = pattern, replacement = replacement,
                                x = runValue(x), ignore.case = ignore.case,
                                extended = extended, perl = perl, fixed = fixed,
                                useBytes = useBytes),
                  lengths = runLength(x)))

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
              utils::str(runValue(object), give.head = FALSE)
          })
