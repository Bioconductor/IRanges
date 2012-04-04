### =========================================================================
### The DataTable API
### -------------------------------------------------------------------------
###
### DataTable is an API only (i.e. virtual class with no slots) for accessing
### objects with a rectangular shape like DataFrame or RangedData objects.
### It mimics the API for standard data.frame objects.
###
### See the Vector-class.R file for the definitions of the DataTable and
### DataTableORNULL virtual classes (they need to occur before the definition
### of the Vector class).


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Basic methods.
###

setMethod("NROW", "DataTable", function(x) nrow(x))

setMethod("NCOL", "DataTable", function(x) ncol(x))

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
                   rownames(x) <- value[[1L]]
                   colnames(x) <- value[[2L]]
                   x
                 })

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Subsetting.
###

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

setMethod("seqselect", "DataTable",
          function(x, start=NULL, end=NULL, width=NULL)
          {
              if (!is.null(end) || !is.null(width))
                  start <- IRanges(start = start, end = end, width = width)
              irInfo <-
                .bracket.Index(start, nrow(x), rownames(x), asRanges = TRUE)
              if (!is.null(irInfo[["msg"]]))
                  stop(irInfo[["msg"]])
              if (irInfo[["useIdx"]]) {
                  ir <- irInfo[["idx"]]
                  if (length(ir) == 0) {
                      x <- x[integer(0),,drop=FALSE]
                  } else {
                      x <-
                        do.call(rbind,
                                lapply(seq_len(length(ir)), function(i)
                                       window(x,
                                              start = start(ir)[i],
                                              width = width(ir)[i])))
                  }
              }
              x
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
                     if (length(ir) == 0)
                         return(x)
                     if (anyMissingOrOutside(start(ir), 1L, nrow(x)) ||
                         anyMissingOrOutside(end(ir), 1L, nrow(x)))
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
                                            lr, " rows"))
                             else
                                 value <-
                                   value[rep(seq_len(nrv), length.out = lr), ,
                                         drop=FALSE]
                         }
                     }
                     irValues <- PartitioningByEnd(cumsum(width(ir)))
                     ir <- gaps(ir, start = 1, end = nrow(x))
                     if ((length(ir) == 0) || (start(ir)[1L] != 1))
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
                         rownames(value) <- seqselect(rownames(x), irValues)
                         colnames(value) <- colnames(x)
                         subseqs[seq(2, length(subseqs), by = 2)] <-
                           lapply(seq_len(length(irValues)), function(i)
                                  window(value,
                                         start = start(irValues)[i],
                                         width = width(irValues)[i]))
                     }
                     do.call(rbind, subseqs)
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

setMethod("subset", "DataTable",
          function (x, subset, select, drop = FALSE, ...) 
          {
              if (missing(subset)) 
                  i <- TRUE
              else {
                  i <- eval(substitute(subset), x, parent.frame())
                  i <- try(as.logical(i), silent=TRUE)
                  if (inherits(i, "try-error"))
                    stop("'subset' must be coercible to logical")
                  i <- i & !is.na(i)
              }
              if (missing(select)) 
                  j <- TRUE
              else {
                  nl <- as.list(seq_len(ncol(x)))
                  names(nl) <- colnames(x)
                  j <- eval(substitute(select), nl, parent.frame())
              }
              x[i, j, drop = drop]
          })

setMethod("na.omit", "DataTable",
          function(object, ...) {
            attr(object, "row.names") <- rownames(object)
            object.omit <- stats:::na.omit.data.frame(object)
            attr(object.omit, "row.names") <- NULL
            object.omit
          })

setMethod("na.exclude", "DataTable",
          function(object, ...) {
            attr(object, "row.names") <- rownames(object)
            object.ex <- stats:::na.exclude.data.frame(object)
            attr(object.ex, "row.names") <- NULL
            object.ex
          })

setMethod("is.na", "DataTable", function(x) {
  na <- do.call(cbind, lapply(seq(ncol(x)), function(xi) is.na(x[[xi]])))
  rownames(na) <- rownames(x)
  na
})

setMethod("complete.cases", "DataTable", function(...) {
  args <- list(...)
  if (length(args) == 1) {
    x <- args[[1L]]
    rowSums(is.na(x)) == 0
  } else complete.cases(args[[1L]]) & do.call(complete.cases, args[-1L])
})

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Combining.
###

setMethod("cbind", "DataTable", function(..., deparse.level=1)
          stop("missing 'cbind' method for DataTable class ",
               class(list(...)[[1L]])))

setMethod("rbind", "DataTable", function(..., deparse.level=1)
          stop("missing 'rbind' method for DataTable class ",
               class(list(...)[[1L]])))

## FIXME: do not cheat by going through data.frame
setMethod("merge", c("DataTable", "DataTable"), function(x, y, ...) {
  DataFrame(merge(as.data.frame(x), as.data.frame(y), ...))
})
setMethod("merge", c("data.frame", "DataTable"), function(x, y, ...) {
  DataFrame(merge(x, as.data.frame(y), ...))
})
setMethod("merge", c("DataTable", "data.frame"), function(x, y, ...) {
  DataFrame(merge(as.data.frame(x), y, ...))
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
              .mc[[1L]] <- .by.data.frame
              ans <- eval(.mc, parent.frame())
              attr(ans, "call") <- mc
              ans
          })

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Comparison
###

setMethod("unique", "DataTable", function (x, incomparables = FALSE, ...) 
          {
            x[!duplicated(x, incomparables = incomparables, ...), ]
          })

setMethod("duplicated", "DataTable",
          function (x, incomparables = FALSE, fromLast = FALSE, ...) 
          {
            x <- as.data.frame(x)
            callGeneric()
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
                  nms <- rownames(object)
                  if (nr < 20) {
                      out <-
                        as.matrix(format(as.data.frame(
                                         lapply(object, showAsCell),
                                         optional = TRUE)))
                      if (!is.null(nms))
                          rownames(out) <- nms
                  } else {
                      out <-
                        rbind(as.matrix(format(as.data.frame(
                                               lapply(object, function(x)
                                                      showAsCell(head(x, 9))),
                                               optional = TRUE))),
                              rbind(rep.int("...", nc)),
                              as.matrix(format(as.data.frame(
                                               lapply(object, function(x) 
                                                      showAsCell(tail(x, 9))),
                                               optional = TRUE))))
                      if (is.null(nms)) {
                          rownames(out) <-
                            c(as.character(1:9), "...",
                              as.character((nr-8L):nr))
                      } else {
                          rownames(out) <- c(head(nms, 9), "...", tail(nms, 9))
                      }
                  }
                  classinfo <-
                    matrix(unlist(lapply(object, function(x)
                                  paste("<", class(x), ">", sep = "")),
                                  use.names = FALSE), nrow = 1,
                           dimnames = list("", colnames(out)))
                  out <- rbind(classinfo, out)
                  print(out, quote = FALSE, right = TRUE)
              }
          })
