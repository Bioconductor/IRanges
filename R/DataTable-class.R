### =========================================================================
### DataTable objects
### -------------------------------------------------------------------------
###
### The DataTable virtual class is a general container for storing a list of
### sequences.
###

setClass("DataTable", contains = "Sequence", representation("VIRTUAL"))

setClassUnion("DataTableORNULL", c("DataTable", "NULL"))


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Basic methods.
###

setGeneric("cbind", function(..., deparse.level=1) standardGeneric("cbind"),
           signature = "...")

setMethod("cbind", "DataTable", function(..., deparse.level=1)
          stop("missing 'cbind' method for DataTable class ",
               class(list(...)[[1]])))

setMethod("dim", "DataTable", function(x) c(nrow(x), ncol(x)))

setMethod("dimnames", "DataTable",
          function(x) {
            list(rownames(x), colnames(x))
          })

setReplaceMethod("dimnames", "DataTable",
                 function(x, value)
                 {
                   if (!is.list(value))
                     stop("replacement value must be a list")
                   rownames(x) <- value[[1]]
                   colnames(x) <- value[[2]]
                   x
                 })

setMethod("head", "DataTable",
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

setMethod("NCOL", "DataTable", function(x) ncol(x))

setMethod("NROW", "DataTable", function(x) nrow(x))

setGeneric("rbind", function(..., deparse.level=1) standardGeneric("rbind"),
           signature = "...")

setMethod("rbind", "DataTable", function(..., deparse.level=1)
          stop("missing 'rbind' method for DataTable class ",
               class(list(...)[[1]])))

setMethod("seqselect", "DataTable",
          function(x, start=NULL, end=NULL, width=NULL)
          {
              if (is.null(end) && is.null(width)) {
                  if (is.null(start))
                      ir <- IRanges(start = 1, width = nrow(x))
                  else if (is(start, "Ranges"))
                      ir <- start
                  else {
                      if (is.logical(start) && length(start) != nrow(x))
                          start <- rep(start, length.out = nrow(x))
                      ir <- as(start, "IRanges")
                  }
              } else {
                  ir <- IRanges(start=start, end=end, width=width, names=NULL)
              }
              if (any(start(ir) < 1L) || any(end(ir) > nrow(x)))
                  stop("some ranges are out of bounds")
              if (length(ir) == 0)
                  x[integer(0),,drop=FALSE]
              else
                  do.call(rbind,
                          lapply(seq_len(length(ir)), function(i)
                                 window(x,
                                        start = start(ir)[i],
                                        width = width(ir)[i])))
          })

setReplaceMethod("seqselect", "DataTable",
                 function(x, start = NULL, end = NULL, width = NULL, value)
                 {
                     if (is.null(end) && is.null(width)) {
                         if (is.null(start))
                             ir <- IRanges(start = 1, width = nrow(x))
                         else if (is(start, "Ranges"))
                             ir <- start
                         else {
                             if (is.logical(start) && length(start) != nrow(x))
                                 start <- rep(start, length.out = nrow(x))
                             ir <- as(start, "IRanges")
                         }
                     } else {
                         ir <- IRanges(start=start, end=end, width=width, names=NULL)
                     }
                     ir <- reduce(ir)
                     if (any(start(ir) < 1L) || any(end(ir) > nrow(x)))
                         stop("some ranges are out of bounds")
                     if (!is.null(value)) {
                         if (!is(value, class(x))) {
                             value <- try(as(value, class(x)), silent = TRUE)
                             if (inherits(value, "try-error"))
                                 stop("'value' must be a ", class(x),
                                      " object or NULL")
                         }
                         lr <- sum(width(ir))
                         nrv <- nrow(value)
                         if (lr != nrv) {
                             if ((lr == 0) || (lr %% nrv != 0))
                                 stop(paste(nrv, "rows in value to replace",
                                            lr, "rows"))
                             else
                                 value <-
                                   value[rep(seq_len(nrv), length.out = lr), ,
                                         drop=FALSE]
                         }
                     }
                     irValues <- PartitioningByEnd(cumsum(width(ir)))
                     ir <- gaps(ir, start = 1, end = nrow(x))
                     if ((length(ir) == 0) || (start(ir)[1] != 1))
                         ir <- c(IRanges(start = 1, width = 0), ir)
                     if (end(ir[length(ir)]) != nrow(x))
                         ir <- c(ir, IRanges(start = nrow(x), width = 0))
                     subseqs <- vector("list", length(irValues) + length(ir))
                     if (length(ir) > 0) {
                         subseqs[seq(1, length(subseqs), by = 2)] <-
                           lapply(seq_len(length(ir)), function(i)
                                  window(x,
                                         start = start(ir)[i],
                                         width = width(ir)[i]))
                     }
                     if (length(irValues) > 0) {
                         nms <- rownames(x)
                         if (is.null(nms)) {
                             rownames(value) <- NULL
                         } else {
                             rownames(value) <- seqselect(nms, irValues)
                         }
                         colnames(value) <- colnames(x)
                         subseqs[seq(2, length(subseqs), by = 2)] <-
                           lapply(seq_len(length(irValues)), function(i)
                                  window(value,
                                         start = start(irValues)[i],
                                         width = width(irValues)[i]))
                     }
                     do.call(rbind, subseqs)
                 })

setMethod("subset", "DataTable",
          function (x, subset, select, drop = FALSE, ...) 
          {
              if (missing(subset)) 
                  i <- TRUE
              else {
                  i <- eval(substitute(subset), x, parent.frame())
                  if (!is.logical(i)) 
                      stop("'subset' must evaluate to logical")
                  i <- i & !is.na(i)
              }
              if (missing(select)) 
                  j <- TRUE
              else {
                  nl <- as.list(seq_len(ncol(x)))
                  names(nl) <- names(x)
                  j <- eval(substitute(select), nl, parent.frame())
              }
              x[i, j, drop = drop]
          })

setMethod("tail", "DataTable",
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

setMethod("window", "DataTable",
          function(x, start = NA, end = NA, width = NA,
                   frequency = NULL, delta = NULL, ...)
          {
              solved_SEW <- solveWindowSEW(nrow(x), start, end, width)
              if (is.null(frequency) && is.null(delta)) {
                  x[as.integer(solved_SEW),,drop = FALSE]
              } else {
                  idx <-
                    stats:::window.default(seq_len(nrow(x)),
                                           start = start(solved_SEW),
                                           end = end(solved_SEW),
                                           frequency = frequency,
                                           deltat = delta, ...)
                  attributes(idx) <- NULL
                  x[idx,,drop = FALSE]
              }
          })

setReplaceMethod("window", "DataTable",
                 function(x, start = NA, end = NA, width = NA,
                          keepLength = TRUE, ..., value)
                 {
                     if (!isTRUEorFALSE(keepLength))
                         stop("'keepLength' must be TRUE or FALSE")
                     solved_SEW <- solveWindowSEW(nrow(x), start, end, width)
                     if (!is.null(value)) {
                         if (!is(value, class(x))) {
                             value <- try(as(value, class(x)), silent = TRUE)
                             if (inherits(value, "try-error"))
                                 stop("'value' must be a ", class(x),
                                      " object or NULL")
                         }
                         if (keepLength && (nrow(value) != width(solved_SEW)))
                             value <-
                               value[rep(seq_len(nrow(value)),
                                         length.out = width(solved_SEW)), ,
                                     drop=FALSE]
                     }
                     rbind(window(x, end = start(solved_SEW) - 1L),
                           value,
                           window(x, start = end(solved_SEW) + 1L))
                 })

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Looping methods.
###

### FIXME: this is not the same signature/contract as for data.frame

setMethod("aggregate", "DataTable",
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
                  sapply(indices, function(i)
                         FUN(window(x, start = start[i], end = end[i]), ...),
                         simplify = simplify)
              } else {
                  frequency <- rep(frequency, length.out = n)
                  delta <- rep(delta, length.out = n)
                  sapply(indices, function(i)
                         FUN(window(x, start = start[i], end = end[i],
                                    frequency = frequency[i], delta = delta[i]),
                             ...),
                         simplify = simplify)
              }
          })

setGeneric("by", function(data, INDICES, FUN, ..., simplify = TRUE)
           standardGeneric("by"))

.by.data.frame <- by.data.frame # so it will find our generic
environment(.by.data.frame) <- topenv()
setMethod("by", "DataTable",
          function(data, INDICES, FUN, ..., simplify = TRUE)
          {
              .mc <- mc <- match.call()
              .mc[[1]] <- .by.data.frame
              ans <- eval(.mc)
              attr(ans, "call") <- mc
              ans
          })

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion
###

setMethod("as.env", "DataTable",
          function(x, enclos = parent.frame()) {
              env <- new.env(parent = enclos)
              lapply(colnames(x),
                     function(col) {
                         colFun <- function() {
                             val <- x[[col]]
                             rm(list=col, envir=env)
                             assign(col, val, env)
                             val
                         }
                         makeActiveBinding(col, colFun, env)
                     })
              env
          })

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "show" method.
###

setGeneric("showAsCell", function(object) standardGeneric("showAsCell"))
setMethod("showAsCell", "vectorORfactor", function(object) object)
setMethod("showAsCell", "Sequence", function(object)
          rep("########", length(object)))

setMethod("show", "DataTable",
          function(object)
          {
              nr <- nrow(object)
              nc <- ncol(object)
              cat(class(object), " with ",
                  nr, ifelse(nr == 1, " row and ", " rows and "),
                  nc, ifelse(nc == 1, " column\n", " columns\n"),
                  sep = "")
              if (nr > 0 && nc > 0) {
                  k <- min(nr, 10)
                  out <-
                    as.matrix(format.data.frame(do.call(data.frame,
                              lapply(object,
                                     function(x) showAsCell(head(x, k))))))
                  if (!is.null(rownames(object)))
                      rownames(out) <- head(rownames(object), k)
                  classinfo <-
                    matrix(unlist(lapply(object, function(x)
                                  paste("<", class(x), ">", sep = "")),
                                  use.names = FALSE), nrow = 1,
                           dimnames = list("", colnames(out)))
                  out <- rbind(classinfo, out)
                  print(out, quote = FALSE, right = TRUE)
                  diffK <- nr - k
                  if (diffK > 0)
                      cat("...\n<", diffK,
                          ifelse(diffK == 1, " more row>\n", " more rows>\n"),
                          sep="")
              }
          })
