### =========================================================================
### Sequence objects
### -------------------------------------------------------------------------
###
### The Sequence virtual class is a general container for storing a finite
### sequence i.e. an ordered finite collection of elements.
###

### Is it the right place for this?
setClassUnion("vectorORfactor", c("vector", "factor"))

### Need to be defined before the Sequence class. See DataTable-API.R for the
### implementation of the DataTable API.
setClass("DataTable", representation("VIRTUAL"))
setClassUnion("DataTableORNULL", c("DataTable", "NULL"))


setClass("Sequence",
    contains="Annotated",
    representation(
        "VIRTUAL",
        elementMetadata="DataTableORNULL"
    )
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Internal utility (not exported).
###

setGeneric("showAsCell", function(object) standardGeneric("showAsCell"))
setMethod("showAsCell", "ANY", function(object) object)
setMethod("showAsCell", "list", function(object)
          rep.int("########", length(object)))
setMethod("showAsCell", "Sequence", function(object)
          rep.int("########", length(object)))


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Accessor methods.
###

setMethod("NROW", "Sequence", function(x) length(x))

### Same definition as base::nlevels() but needed anyway because the call to
### levels(x) in base::nlevels() won't dispatch on the appropriate "levels"
### method.
setMethod("nlevels", "Sequence", function(x) length(levels(x)))

setGeneric("elementMetadata",
           function(x, ...) standardGeneric("elementMetadata"))
setMethod("elementMetadata", "Sequence",
          function(x) {
              if ("elementMetadata" %in% names(attributes(x))) {
                  emd <- x@elementMetadata
                  if (!is.null(emd) && !is.null(names(x)))
                      rownames(emd) <- head(names(x), nrow(emd))
              } else {
                  emd <- NULL
              }
              emd
          })
setGeneric("values", function(x, ...) standardGeneric("values"))
setMethod("values", "Sequence", function(x, ...) elementMetadata(x, ...))

setGeneric("elementMetadata<-",
           function(x, ..., value) standardGeneric("elementMetadata<-"))
setReplaceMethod("elementMetadata", "Sequence",
                 function(x, value) {
                     if (!is(value, "DataTableORNULL"))
                         stop("replacement 'elementMetadata' value must be a DataTable object or NULL")
                     if ("elementMetadata" %in% names(attributes(x))) {
                         if (!is.null(value) && length(x) != nrow(value))
                             stop("the number of rows in elementMetadata 'value' ",
                                     "(if non-NULL) must match the length of 'x'")
                         if (!is.null(value))
                             rownames(value) <- NULL
                         x@elementMetadata <- value
                     }
                     x
                 })
setGeneric("values<-", function(x, ..., value) standardGeneric("values<-"))
setReplaceMethod("values", "Sequence",
                 function(x, value) {
                     elementMetadata(x) <- value
                     x
                 })


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Validity.
###

.valid.Sequence.elementMetadata <- function(x)
{
    emd <- elementMetadata(x)
    if (!is(emd, "DataTableORNULL"))
        return("'elementMetadata(x)' must be a DataTable object or NULL")
    if (is.null(emd))
        return(NULL)
    ## 'emd' is a DataTable object.
    if (nrow(emd) != length(x)) {
        msg <- c("number of rows in DataTable 'elementMetadata(x)' ",
                 "must match length of 'x'")
        return(paste(msg, collapse=""))
    }
    if (!identical(rownames(emd), names(x))) {
        msg <- c("the rownames of DataTable 'elementMetadata(x)' ",
                 "match the names of 'x'")
        return(paste(msg, collapse=""))
    }
    NULL
}

.valid.Sequence <- function(x)
{
    c(.valid.Sequence.elementMetadata(x))
}
setValidity2("Sequence", .valid.Sequence)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Subsetting.
###

.bracket.Sequence <-
function(x, i, j, ..., drop)
{
    if (!is.null(elementMetadata(x)))
        elementMetadata(x) <- elementMetadata(x)[i,,drop=FALSE]
    x
}

## 'allowAppend' allows new elements in 'idx' for appending. Ideally,
## all list-like sequences should allow appending through [<-, but
## this is only supported by DataFrame for now. 
.bracket.Index <-
function(idx, lx, nms = NULL, dup.nms = FALSE, asRanges = FALSE,
         allowAppend = FALSE)
{
    msg <- NULL
    newNames <- character(0)
    if (is.numeric(idx)) {
        if (!is.integer(idx))
            idx <- as.integer(idx)
        if (anyMissingOrOutside(idx, -lx,
                                if (!allowAppend) lx else .Machine$integer.max))
        {
            msg <- "subscript contains NAs or out of bounds indices"
        } else {
            anyPos <- anyMissingOrOutside(idx, upper = 0L)
            anyNeg <- anyMissingOrOutside(idx, 0L)
            if (anyPos && anyNeg)
                msg <- "negative and positive indices cannot be mixed"
        }
    } else if (is.logical(idx)) {
        if (anyMissing(idx))
            msg <- "subscript contains NAs"
        else if (!allowAppend && length(idx) > lx)
            msg <- "subscript out of bounds"
    } else if (is.character(idx) || is.factor(idx)) {
        if (anyMissing(idx))
            msg <- "subscript contains NAs"
        else if (!allowAppend && is.null(nms) && length(idx) > 0)
            msg <- "cannot subset by character when names are NULL"
        else if (!allowAppend) {
            if (dup.nms)
                m <- pmatch(idx, nms, duplicates.ok = TRUE)
            else
                m <- match(idx, nms)
            if (!dup.nms && anyMissing(m))
                msg <- "mismatching names"
        }
    } else if (is(idx, "Rle")) {
        if (anyMissing(runValue(idx)))
            msg <- "subscript contains NAs"
        else if (!allowAppend && length(idx) > lx)
            msg <- "subscript out of bounds"
    } else if (is(idx, "Ranges")) {
        rng <- range(idx)
        if ((length(rng) > 0) && (start(rng) < 1 || end(rng) > lx))
            stop("range index out of bounds")
        else if (anyMissingOrOutside(width(idx), 1L)) {
            idx <- idx[width(idx) > 0L]
        }
    } else if (!is.null(idx)) {
        msg <- "invalid subscript type"
    }
    if (!is.null(msg)) {
        useIdx <- NULL
        idx <- NULL
    } else {
        useIdx <- TRUE
        if (asRanges) {
            if (length(idx) == 0) {
                idx <- IRanges()
            } else if (is.character(idx)) {
                if (allowAppend) {
                    m <- match(idx, nms)
                    nam <- is.na(m)
                    m[nam] <- lx + seq(sum(nam))
                    newNames <- idx[nam]
                    idx <- as(m, "IRanges")
                } else idx <-
                    as(pmatch(idx, nms, duplicates.ok = TRUE), "IRanges")
            } else if (is.logical(idx)) {
                if (all(idx)) {
                    useIdx <- FALSE
                } else {
                    if (length(idx) < lx)
                        idx <- rep(idx, length.out = lx)
                    idx <- as(idx, "NormalIRanges")
                }
            } else if (is.integer(idx)) {
                if (anyNeg)
                    idx <- seq_len(lx)[idx]
                idx <- as(idx, "IRanges")
            } else if (is(idx, "Rle")) {
                if (all(runValue(idx))) {
                    useIdx <- FALSE
                } else {
                    if (length(idx) < lx)
                        idx <- rep(idx, length.out = lx)
                    idx <- as(idx, "NormalIRanges")
                }
            }
            if (length(idx) == 1 && start(idx) == 1 && end(idx) == lx)
                useIdx <- FALSE
        } else {
            if (length(idx) == 0) {
                idx <- integer()
            } else if (is.character(idx)) {
                if (allowAppend) {
                    m <- match(idx, nms)
                    nam <- is.na(m)
                    newNames <- idx[nam]
                    m[nam] <- lx + seq_len(sum(nam))
                    idx <- m
                } else idx <- pmatch(idx, nms, duplicates.ok = TRUE)
            } else if (is.logical(idx)) {
                if (all(idx)) {
                    useIdx <- FALSE
                } else {
                    if (length(idx) < lx)
                        idx <- rep(idx, length.out = lx)
                    idx <- which(idx)
                }
            } else if (is.integer(idx) && anyNeg) {
                idx <- seq_len(lx)[idx]
            } else if (is(idx, "Rle")) {
                if (all(runValue(idx))) {
                    useIdx <- FALSE
                } else {
                    if (length(idx) < lx)
                        idx <- rep(idx, length.out = lx)
                    idx <- which(idx)
                }
            } else if (is(idx, "Ranges")) {
                if (length(idx) == 1 && start(idx) == 1 && end(idx) == lx)
                    useIdx <- FALSE
                else
                    idx <- as.integer(idx)
            }
        }
    }
    list(msg = msg, useIdx = useIdx, idx = idx, newNames = newNames)
}

setMethod("[", "Sequence", function(x, i, j, ..., drop)
          stop("missing '[' method for Sequence class ", class(x)))

setReplaceMethod("[", "Sequence",
                 function(x, i, j,..., value) {
                     if (!missing(j) || length(list(...)) > 0)
                         stop("invalid replacement")
                     if (missing(i)) {
                         seqselect(x, start = 1, end = length(x)) <- value
                     } else {
                         iInfo <- .bracket.Index(i, length(x), names(x))
                         if (is.null(iInfo[["msg"]])) {
                             if (iInfo[["useIdx"]])
                                 seqselect(x, iInfo[["idx"]], width = 1) <- value
                             else
                                 seqselect(x, start = 1, end = length(x)) <- value
                         } else if (is.atomic(i)) {
                             stop(iInfo[["msg"]])
                         } else {
                             seqselect(x, i) <- value
                         }
                     }
                     x
                 })

### Returns an IRanges instance of length 1.
### Not exported.
solveWindowSEW <- function(seq_length, start, end, width)
{
    solved_SEW <-
      try(solveUserSEW(seq_length, start=start, end=end, width=width),
          silent = TRUE)
    if (inherits(solved_SEW, "try-error"))
        stop("Invalid sequence coordinates.\n",
             "  Please make sure the supplied 'start', 'end' and 'width' arguments\n",
             "  are defining a region that is within the limits of the sequence.")
    solved_SEW
}

setMethod("window", "Sequence",
          function(x, start = NA, end = NA, width = NA,
                   frequency = NULL, delta = NULL, ...)
          {
              solved_SEW <- solveWindowSEW(length(x), start, end, width)
              if (is.null(frequency) && is.null(delta)) {
                  x[as.integer(solved_SEW)]
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

setMethod("window", "NULL",
        function(x, start = NA, end = NA, width = NA,
                 frequency = NULL, delta = NULL, ...) NULL)

setMethod("window", "vector",
          function(x, start = NA, end = NA, width = NA,
                   frequency = NULL, delta = NULL, ...)
          {
              solved_SEW <- solveWindowSEW(length(x), start, end, width)
              if (is.null(frequency) && is.null(delta)) {
                  .Call("vector_seqselect",
                        x, start(solved_SEW), width(solved_SEW),
                        PACKAGE="IRanges")
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

setMethod("window", "factor",
          function(x, start = NA, end = NA, width = NA,
                   frequency = NULL, delta = NULL, ...)
          {
              labels <- levels(x)
              factor(callGeneric(as.integer(x), start = start, end = end,
                                 width = width, frequency = frequency,
                                 delta = delta, ...),
                     levels = seq_len(length(labels)), labels = labels)
          })

setReplaceMethod("window", "Sequence",
                 function(x, start = NA, end = NA, width = NA,
                          keepLength = TRUE, ..., value)
                 {
                     if (!isTRUEorFALSE(keepLength))
                         stop("'keepLength' must be TRUE or FALSE")
                     solved_SEW <- solveWindowSEW(length(x), start, end, width)
                     if (!is.null(value)) {
                         if (!is(value, class(x))) {
                             value <- try(as(value, class(x)), silent = TRUE)
                             if (inherits(value, "try-error"))
                                 stop("'value' must be a ", class(x),
                                      " object or NULL")
                         }
                         if (keepLength && (length(value) != width(solved_SEW)))
                             value <- rep(value, length.out = width(solved_SEW))
                     }
                     c(window(x, end = start(solved_SEW) - 1L),
                       value,
                       window(x, start = end(solved_SEW) + 1L))
                 })

setReplaceMethod("window", "vector",
                 function(x, start = NA, end = NA, width = NA,
                          keepLength = TRUE, ..., value)
                 {
                     if (!isTRUEorFALSE(keepLength))
                         stop("'keepLength' must be TRUE or FALSE")
                     solved_SEW <- solveWindowSEW(length(x), start, end, width)
                     if (!is.null(value)) {
                         if (!is(value, class(x))) {
                             value <- try(as(value, class(x)), silent = TRUE)
                             if (inherits(value, "try-error"))
                                 stop("'value' must be a ", class(x),
                                      " object or NULL")
                         }
                         if (keepLength && (length(value) != width(solved_SEW)))
                             value <- rep(value, length.out = width(solved_SEW))
                     }
                     c(window(x, end = start(solved_SEW) - 1L),
                       value,
                       window(x, start = end(solved_SEW) + 1L))
                 })

setReplaceMethod("window", "factor",
                 function(x, start = NA, end = NA, width = NA,
                          keepLength = TRUE, ..., value)
                 {
                     levels <- levels(x)
                     x <- as.character(x)
                     value <- as.character(value)
                     factor(callGeneric(), levels = levels)
                 })

setGeneric("seqselect", signature="x",
           function(x, start=NULL, end=NULL, width=NULL)
           standardGeneric("seqselect"))

setMethod("seqselect", "Sequence",
          function(x, start=NULL, end=NULL, width=NULL)
          {
              if (!is.null(end) || !is.null(width))
                  start <- IRanges(start = start, end = end, width = width)
              irInfo <-
                .bracket.Index(start, length(x), names(x), asRanges = TRUE)
              if (!is.null(irInfo[["msg"]]))
                  stop(irInfo[["msg"]])
              if (irInfo[["useIdx"]]) {
                  ir <- irInfo[["idx"]]
                  if (length(ir) == 0) {
                      x <- x[integer(0)]
                  } else {
                      x <-
                        do.call(c,
                                lapply(seq_len(length(ir)), function(i)
                                       window(x,
                                              start = start(ir)[i],
                                              width = width(ir)[i])))
                  }
              }
              x
          })

setMethod("seqselect", "NULL",
          function(x, start=NULL, end=NULL, width=NULL) NULL)

setMethod("seqselect", "vector",
          function(x, start=NULL, end=NULL, width=NULL)
          {
              if (!is.null(end) || !is.null(width))
                  start <- IRanges(start = start, end = end, width = width)
              irInfo <-
                .bracket.Index(start, length(x), names(x), asRanges = TRUE)
              if (!is.null(irInfo[["msg"]]))
                  stop(irInfo[["msg"]])
              if (irInfo[["useIdx"]]) {
                  ir <- irInfo[["idx"]]
                  x <-
                    .Call("vector_seqselect", x, start(ir), width(ir),
                          PACKAGE="IRanges")
              }
              x
          })

setMethod("seqselect", "factor",
          function(x, start=NULL, end=NULL, width=NULL)
          {
              ans <-
                callGeneric(as.integer(x), start = start, end = end,
                            width = width)
              attributes(ans) <- list(levels = levels(x), class = "factor")
              ans
          })

setMethod("seqselect", "ANY",
          function(x, start=NULL, end=NULL, width=NULL)
          {
            ans <-
              callGeneric(seq_len(length(x)), start = start, end = end,
                          width = width)
            x[ans]
          })

setGeneric("seqselect<-", signature="x",
           function(x, start = NULL, end = NULL, width = NULL, value)
           standardGeneric("seqselect<-"))

setReplaceMethod("seqselect", "Sequence",
                 function(x, start = NULL, end = NULL, width = NULL, value)
                 {
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
                     lr <- sum(width(ir))
                     lv <- length(value)
                     if (!is.null(value)) {
                         if (!is(value, class(x))) {
                             value <- try(as(value, class(x)), silent = TRUE)
                             if (inherits(value, "try-error"))
                                 stop("'value' must be a ", class(x),
                                      " object or NULL")
                         }
                         if (lr != lv) {
                             if ((lr == 0) || (lr %% lv != 0))
                                 stop(paste(lv, "elements in value to replace",
                                            lr, "elements"))
                             else
                                 value <- rep(value, length.out = lr)
                         }
                         names(value) <- seqselect(names(x), ir)
                     }
                     irValues <- PartitioningByEnd(cumsum(width(ir)))
                     ir <- gaps(ir, start = 1, end = length(x))
                     if ((length(ir) == 0) || (start(ir)[1L] != 1))
                         ir <- c(IRanges(start = 1, width = 0), ir)
                     if (end(ir[length(ir)]) != length(x))
                         ir <- c(ir, IRanges(start = length(x), width = 0))
                     subseqs <- vector("list", length(irValues) + length(ir))
                     if (length(ir) > 0) {
                         subseqs[seq(1, length(subseqs), by = 2)] <-
                           lapply(seq_len(length(ir)), function(i)
                                  window(x,
                                         start = start(ir)[i],
                                         width = width(ir)[i]))
                     }
                     if (length(irValues) > 0) {
                         subseqs[seq(2, length(subseqs), by = 2)] <-
                           lapply(seq_len(length(irValues)), function(i)
                                  window(value,
                                         start = start(irValues)[i],
                                         width = width(irValues)[i]))
                     }
                     do.call(c, subseqs)
                 })

setReplaceMethod("seqselect", "vector",
                 function(x, start = NULL, end = NULL, width = NULL, value)
                 {
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
                     i <- unlist(ir)
                     if (is.null(value)) {
                         x <- x[-i]
                     } else {
                         if (!is(value, class(x))) {
                             value <- try(as(value, class(x)), silent = TRUE)
                             if (inherits(value, "try-error"))
                                 stop("'value' must be a ", class(x),
                                      " object or NULL")
                         }
                         x[i] <- value
                     }
                     x
                 })

setReplaceMethod("seqselect", "factor",
                 function(x, start = NULL, end = NULL, width = NULL, value)
                 {
                     levels <- levels(x)
                     x <- as.character(x)
                     value <- as.character(value)
                     factor(callGeneric(), levels = levels)
                 })


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Simple helper functions for some common subsetting operations.
###

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

setMethod("rev", "Sequence",
          function(x) {
              if (length(x) == 0)
                  x
              else
                  x[length(x):1]  
          })

setMethod("rep", "Sequence", function(x, ...)
          x[rep(seq_len(length(x)), ...)])

### The natural (and cleaner) thing to do for this generic would be to use
### the same arguments as base::rep.int() (i.e. x, times) but then
### 'R CMD check' would get confused and think that we are trying to define
### an S3 method for base::rep() and would complain (observed with R <= 2.12):
###   * checking S3 generic/method consistency ... WARNING
###   rep:
###     function(x, ...)
###   rep.int:
###     function(x, times)
### so we use the arguments of base::rep() (i.e. x, ...) just to make
### 'R CMD check' happy.
setGeneric("rep.int",
    function(x, ...) standardGeneric("rep.int"),
        useAsDefault = function(x, ...) base::rep.int(x, ...)
)

setMethod("rep.int", "Sequence",
    function(x, times) x[rep.int(seq_len(length(x)), times)]
)

setMethod("subset", "Sequence",
          function (x, subset, ...) 
          {
              if (!is.logical(subset)) 
                  stop("'subset' must be logical")
              x[subset & !is.na(subset)]
          })


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Comparison.
###

### Maybe this is how `!=` should have been defined in the base package so
### nobody would ever need to bother implementing such an obvious thing.
setMethod("!=", signature(e1="Sequence", e2="Sequence"),
    function(e1, e2) !(e1 == e2)
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Combining and splitting.
###

.addNAElementMetadataRow <- function(x) {
  emd <- elementMetadata(x)
  if (!is.null(emd))
    elementMetadata(x)[nrow(emd)+1L,] <- NA
  x
}

.rbind.elementMetadata <- function(x, ...)
{
    l <- list(x, ...)
    emd <- lapply(l, elementMetadata)
    noEmd <- sapply(emd, is.null)
    if (all(noEmd))
        return(NULL)
    newDf <- function(nr)
      new("DataFrame", listData = structure(list(), names = character(0)),
          nrows = nr)
    emd[noEmd] <- lapply(elementLengths(l[noEmd]), newDf)
    allCols <- unique(do.call(c, lapply(emd, colnames)))
    fillCols <- function(df) {
      if (nrow(df))
          df[setdiff(allCols, colnames(df))] <- DataFrame(NA)
      df
    }
    do.call(rbind, lapply(emd, fillCols))
}

.c.Sequence <- function(x, ..., recursive = FALSE)
{
    if (!is.null(elementMetadata(x)))
      elementMetadata(x) <- .rbind.elementMetadata(x, ...)
    x
}

setMethod("c", "Sequence",
          function(x, ..., recursive = FALSE)
          stop("missing 'c' method for Sequence class ", class(x)))

setMethod("append", c("Sequence", "Sequence"),
          function(x, values, after=length(x)) {
              if (!isSingleNumber(after))
                  stop("'after' must be a single number")
              xlen <- length(x)
              if (after == 0L)
                  c(values, x)
              else if (after >= xlen)
                  c(x, values)
              else
                  c(window(x, 1L, after), values, window(x, after + 1L, xlen))
             })


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Looping methods.
###

#.tapplyDefault <- base::tapply
#environment(.tapplyDefault) <- topenv()
.tapplyDefault <-
function (X, INDEX, FUN = NULL, ..., simplify = TRUE) 
{
    if (!is.null(FUN))
        FUN <- match.fun(FUN)
    if (missing(INDEX))
        stop("'INDEX' is missing")
    if (!is(INDEX, "RleList")) {
        if (!is.list(INDEX) && !is(INDEX, "Rle"))
            INDEX <- Rle(INDEX)
        INDEX <- RleList(INDEX)
    }
    nI <- length(INDEX)
    namelist <- vector("list", nI)
    names(namelist) <- names(INDEX)
    extent <- integer(nI)
    nx <- length(X)
    one <- 1L
    group <- Rle(one, nx)
    ngroup <- one
    for (i in seq_len(nI)) {
        index <- INDEX[[i]]
        if (!is.factor(runValue(index)))
            runValue(index) <- factor(runValue(index))
        offset <- index
        runValue(offset) <- ngroup * (as.integer(runValue(index)) - one)
        if (length(index) != nx) 
            stop("arguments must have same length")
        namelist[[i]] <- levels(index)
        extent[i] <- nlevels(index)
        group <- group + offset
        ngroup <- ngroup * nlevels(index)
    }
    if (is.null(FUN))
        return(as.vector(group))
    groupRanges <- splitRanges(group)
    ans <- lapply(groupRanges, function(i) FUN(seqselect(X, i), ...))
    index <- as.integer(names(ans))
    if (simplify && all(unlist(lapply(ans, length), use.names=FALSE) == 1L)) {
        ansmat <- array(dim = extent, dimnames = namelist)
        ans <- unlist(ans, recursive = FALSE)
    }
    else {
        ansmat <-
          array(vector("list", prod(extent)), dim = extent,
                dimnames = namelist)
    }
    if (length(index) > 0) {
        names(ans) <- NULL
        ansmat[index] <- ans
    }
    ansmat
}
setMethod("tapply", "Sequence", .tapplyDefault)

.shiftApplyInternal <-
function(SHIFT, X, Y, FUN, ..., OFFSET = 0L, simplify = TRUE, verbose = FALSE)
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

.aggregateInternal <-
function(x, by, FUN, start = NULL, end = NULL, width = NULL,
         frequency = NULL, delta = NULL, ..., simplify = TRUE)
{
    FUN <- match.fun(FUN)
    if (!missing(by)) {
        if (is.list(by)) {
            return(callGeneric(x = as.data.frame(x), by = by, FUN = FUN, ...))
        }
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
}

setMethod("aggregate", "Sequence", .aggregateInternal)

setMethod("aggregate", "vector", .aggregateInternal)

setMethod("aggregate", "matrix", stats:::aggregate.default)

setMethod("aggregate", "data.frame", stats:::aggregate.data.frame)

setMethod("aggregate", "ts", stats:::aggregate.ts)

