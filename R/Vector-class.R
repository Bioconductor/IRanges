### =========================================================================
### Vector objects
### -------------------------------------------------------------------------
###
### The Vector virtual class is a general container for storing a finite
### sequence i.e. an ordered finite collection of elements.
###

### Is it the right place for this?
setClassUnion("vectorORfactor", c("vector", "factor"))

### Need to be defined before the Vector class. See DataTable-API.R for the
### implementation of the DataTable API.
setClass("DataTable", representation("VIRTUAL"))
setClassUnion("DataTableORNULL", c("DataTable", "NULL"))


setClass("Vector",
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
setMethod(IRanges:::showAsCell, "ANY", function(object) {
  if (length(dim(object)) > 2)
    dim(object) <- c(nrow(object), prod(tail(dim(object), -1)))
  if (NCOL(object) > 1) {
    df <- as.data.frame(object[, head(seq_len(ncol(object)), 3), drop = FALSE])
    attempt <- do.call(paste, df)
    if (ncol(object) > 3)
      attempt <- paste(attempt, "...")
    attempt
  } else {
    attempt <- try(data.frame(object), silent=TRUE)
    if (is(attempt, "try-error"))
      rep.int("########", length(object))
    object
  }
})
setMethod("showAsCell", "list", function(object)
          rep.int("########", length(object)))
setMethod("showAsCell", "Vector", function(object)
          rep.int("########", length(object)))


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Accessor methods.
###

setMethod("NROW", "Vector", function(x) length(x))

### Same definition as base::nlevels() but needed anyway because the call to
### levels(x) in base::nlevels() won't dispatch on the appropriate "levels"
### method.
setMethod("nlevels", "Vector", function(x) length(levels(x)))

### 3 accessors for the same slot: elementMetadata(), mcols(), and values().
### mcols() is the recommended one, use of elementMetadata() or values() is
### discouraged.
setGeneric("elementMetadata",
    function(x, use.names=FALSE, ...) standardGeneric("elementMetadata")
)

setMethod("elementMetadata", "Vector",
    function(x, use.names=FALSE, ...)
    {
        if (!isTRUEorFALSE(use.names)) 
            stop("'use.names' must be TRUE or FALSE")
        ans <- x@elementMetadata
        if (use.names && !is.null(ans))
            rownames(ans) <- names(x)
        ans
    }
)

setGeneric("mcols",
    function(x, use.names=FALSE, ...) standardGeneric("mcols")
)

setMethod("mcols", "Vector",
    function(x, use.names=FALSE, ...)
        elementMetadata(x, use.names=use.names, ...)
)

setGeneric("values", function(x, ...) standardGeneric("values"))

setMethod("values", "Vector", function(x, ...) elementMetadata(x, ...))

setGeneric("elementMetadata<-",
           function(x, ..., value) standardGeneric("elementMetadata<-"))

setReplaceMethod("elementMetadata", "Vector",
                 function(x, ..., value) {
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

setGeneric("mcols<-", function(x, ..., value) standardGeneric("mcols<-"))

setReplaceMethod("mcols", "Vector",
    function(x, ..., value) `elementMetadata<-`(x, ..., value=value)
)

setGeneric("values<-", function(x, ..., value) standardGeneric("values<-"))

setReplaceMethod("values", "Vector",
                 function(x, value) {
                     elementMetadata(x) <- value
                     x
                 })

setGeneric("rename", function(x, value, ...) standardGeneric("rename"))

.renameVector <- function(x, value, ...) {
  if (missing(value))
    newNames <- c(...)
  else newNames <- c(value, ...)
  badOldNames <- setdiff(names(newNames), names(x))
  if (length(badOldNames))
    stop("Some 'from' names in value not found on 'x': ",
         paste(badOldNames, collapse = ", "))
  names(x)[match(names(newNames), names(x))] <- newNames
  x
}

setMethod("rename", "vector", .renameVector)
setMethod("rename", "Vector", .renameVector)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Validity.
###

.valid.Vector.mcols <- function(x)
{
    x_mcols <- mcols(x)
    if (!is(x_mcols, "DataTableORNULL"))
        return("'mcols(x)' must be a DataTable object or NULL")
    if (is.null(x_mcols))
        return(NULL)
    ## 'x_mcols' is a DataTable object.
    if (nrow(x_mcols) != length(x)) {
        msg <- c("number of rows in DataTable 'mcols(x)' ",
                 "must match length of 'x'")
        return(paste(msg, collapse=""))
    }
    if (!is.null(rownames(x_mcols)) && !identical(rownames(x_mcols), names(x))) {
        msg <- c("the rownames of DataTable 'mcols(x)' ",
                 "must match the names of 'x'")
        return(paste(msg, collapse=""))
    }
    NULL
}

.valid.Vector <- function(x)
{
    c(.valid.Vector.mcols(x))
}
setValidity2("Vector", .valid.Vector)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Subsetting.
###

normalizeSingleBracketSubscript <- function(i, x)
{
    if (is.null(i))
        return(integer(0))
    if (is(i, "Ranges")) {
        ## Subsetting by ranges will be re-introduced later but it will do
        ## the right thing.
        stop("'[' subsetting by Ranges is defunct.\n",
             "Use 'subsetByOverlaps' instead.")
    }
    if (is(i, "Rle"))
        i <- as.vector(i)
    if (!is.atomic(i))
        stop("invalid subscript type")
    lx <- length(x)
    if (is.numeric(i)) {
        if (!is.integer(i))
            i <- as.integer(i)
        if (anyMissingOrOutside(i, upper = lx))
            stop("subscript contains NAs or out of bounds indices")
        i <- i[i != 0L]
        if (any(i < 0L) && any(i > 0L))
            stop("cannot mix negative with positive subscripts")
        if (is(x, "NormalIRanges") && all(i >= 0L)) {
            if (isNotStrictlySorted(i))
                stop("positive numeric subscript must be strictly increasing ",
                     "for NormalIRanges objects")
        }
        return(i)
    }
    if (is.logical(i)) {
        if (anyMissing(i))
            stop("subscript contains NAs")
        li <- length(i)
        if (li > lx)
            stop("subscript out of bounds")
        if (li < lx)
            i <- rep(i, length.out=lx)
        return(which(i))
    }
    if (is.character(i) || is.factor(i)) {
        if (is.null(names(x)))
            stop("cannot subset by character when names are NULL")
        i <- match(i, names(x), incomparables=c(NA_character_, ""))
        if (anyMissing(i))
            stop("subscript contains invalid names")
        return(i)
    }
    stop("invalid subscript type")
}

mk_singleBracketReplacementValue <- function(x, value)
{
    lv <- length(value)
    value <- try(as(value, class(x)), silent=TRUE)
    if (inherits(value, "try-error"))
        stop("'value' must be a ", class(x), " object")
    if (length(value) != lv)
        stop("coercing replacement value to ", class(x), "\n",
             "  changed its length!\n",
             "  Please do the explicit coercion ",
             "yourself with something like:\n",
             "    x[...] <- as(value, \"", class(x), "\")\n",
             "  but first make sure this coercion does what you want.")
    if (is(value, "SplitDataFrameList") && length(x) != 0L)
        colnames(value) <- colnames(x)[[1L]]
    value
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
        if (anyMissingOrOutside(idx, upper = if (!allowAppend) lx
                                             else .Machine$integer.max))
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

setMethod("[", "Vector", function(x, i, j, ..., drop)
          stop("missing '[' method for Vector class ", class(x)))

setReplaceMethod("[", "Vector",
                 function(x, i, j,..., value) {
                     if (!missing(j) || length(list(...)) > 0)
                         stop("invalid replacement")
                     if (missing(i)) {
                         seqselect(x, start = 1, end = length(x)) <- value
                     } else {
                         iInfo <- .bracket.Index(i, length(x), names(x))
                         if (is.null(iInfo[["msg"]])) {
                             if (iInfo[["useIdx"]]) {
                                 w <- rep.int(1L, length(iInfo[["idx"]]))
                                 seqselect(x, iInfo[["idx"]], width = w) <- value
                             }
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

setMethod("window", "Vector",
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
                  .Call2("vector_seqselect",
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

setReplaceMethod("window", "Vector",
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

setMethod("seqselect", "Vector",
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
                    .Call2("vector_seqselect", x, start(ir), width(ir),
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

setReplaceMethod("seqselect", "Vector",
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
                         ## allow conventional vector type promotion rules
                         if (!is(value, "vector") && !is(value, class(x))) {
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

setMethod("head", "Vector",
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

setMethod("tail", "Vector",
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

setMethod("rev", "Vector",
          function(x) {
              if (length(x) == 0)
                  x
              else
                  x[length(x):1]  
          })

setMethod("rep", "Vector", function(x, ...)
          x[rep(seq_len(length(x)), ...)])

setMethod("rep.int", "Vector",
    function(x, times) x[rep.int(seq_len(length(x)), times)]
)

setMethod("subset", "Vector",
          function (x, subset, ...) 
          {
              if (!is.logical(subset) ||
                  (is(subset, "Rle") && !is.logical(runValue(subset)))) 
                  stop("'subset' must be logical")
              x[subset & !is.na(subset)]
          })

setMethod("unique", "Vector",
    function(x, incomparables=FALSE, ...)
    {
        names(x) <- NULL  # for consistency with base::unique() on vectors
        x[!duplicated(x, incomparables=incomparables, ...)]
    }
)

setMethod("sort", "Vector",
    function(x, decreasing=FALSE, ...)
    {
        x[order(x, decreasing=decreasing, ...)]
    }
)

`.%in%.default.method` <- function(x, table) {!is.na(match(x, table))}
setMethod("%in%", c("ANY", "Vector"), `.%in%.default.method`)
setMethod("%in%", c("Vector", "ANY"), `.%in%.default.method`)
### Not strictly required! Defining the above 2 methods covers that case but
### with the following note:
###   > hits %in% hits
###   Note: Method with signature "Vector#ANY" chosen for function "%in%",
###    target signature "Hits#Hits".
###    "ANY#Vector" would also be valid
setMethod("%in%", c("Vector", "Vector"), `.%in%.default.method`)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion.
###

setMethod("as.logical", "Vector",
    function(x) as.vector(x, mode="logical")
)
setMethod("as.integer", "Vector",
    function(x) as.vector(x, mode="integer")
)
setMethod("as.numeric", "Vector",
    function(x) as.vector(x, mode="numeric")
)
setMethod("as.double", "Vector",
    function(x) as.vector(x, mode="double")
)
setMethod("as.complex", "Vector",
    function(x) as.vector(x, mode="complex")
)
setMethod("as.character", "Vector",
    function(x) as.vector(x, mode="character")
)
setMethod("as.raw", "Vector",
    function(x) as.vector(x, mode="raw")
)

setAs("Vector", "vector", function(from) as.vector(from))
setAs("Vector", "logical", function(from) as.logical(from))
setAs("Vector", "integer", function(from) as.integer(from))
setAs("Vector", "numeric", function(from) as.numeric(from))
setAs("Vector", "double", function(from) as.double(from))
setAs("Vector", "complex", function(from) as.complex(from))
setAs("Vector", "character", function(from) as.character(from))
setAs("Vector", "raw", function(from) as.raw(from))

setAs("Vector", "data.frame", function(from) as.data.frame(from))

setMethod("as.data.frame", "Vector",
          function(x, row.names=NULL, optional=FALSE, ...)
          {
            x <- as.vector(x)
            callGeneric()
          })


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Comparison.
###

### Maybe this is how `!=` should have been defined in the base package so
### nobody would ever need to bother implementing such an obvious thing.
setMethod("!=", signature(e1="Vector", e2="Vector"),
    function(e1, e2) !(e1 == e2)
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Combining.
###

rbindRowOfNAsToMetadatacols <- function(x) {
  x_mcols <- mcols(x)
  if (!is.null(x_mcols))
    mcols(x)[nrow(x_mcols)+1L,] <- NA
  x
}

rbind.mcols <- function(x, ...)
{
    l <- list(x, ...)
    l_mcols <- lapply(l, mcols)
    no_mcols <- sapply(l_mcols, is.null)
    if (all(no_mcols))
        return(NULL)
    newDf <- function(nr)
      new("DataFrame", listData = structure(list(), names = character(0)),
          nrows = nr)
    l_mcols[no_mcols] <- lapply(elementLengths(l[no_mcols]), newDf)
    allCols <- unique(do.call(c, lapply(l_mcols, colnames)))
    fillCols <- function(df) {
      if (nrow(df))
          df[setdiff(allCols, colnames(df))] <- DataFrame(NA)
      df
    }
    do.call(rbind, lapply(l_mcols, fillCols))
}

.c.Vector <- function(x, ..., recursive = FALSE)
{
    if (!is.null(mcols(x)))
      mcols(x) <- rbind.mcols(x, ...)
    x
}

setMethod("c", "Vector",
          function(x, ..., recursive = FALSE)
          stop("missing 'c' method for Vector class ", class(x)))

setMethod("append", c("Vector", "Vector"),
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
### Splitting / relisting.
###

setGeneric("splitAsListReturnedClass",
    function(x) standardGeneric("splitAsListReturnedClass")
)

setMethod("splitAsListReturnedClass", "ANY",
    function(x)
    {
        cl <- class(x)
        cl <- c(cl, names(getClass(cl)@contains))
        substring(cl, 1L, 1L) <- toupper(substring(cl, 1L, 1L))
        compressedClass <- c(paste0("Compressed", cl, "List"),
                             paste0(cl, "List"))
        clExists <- which(sapply(compressedClass, isClass) &
                          sapply(compressedClass, extends, "CompressedList"))
        if (length(clExists) == 0L) {
          stop("don't know what class to use for storing the result of ",
               "splitting a ", class(x), " object as a List")
        }
        compressedClass[clExists[1L]]
    }
)

setMethod("relist", c("ANY", "PartitioningByEnd"),
    function(flesh, skeleton)
    {
        ans_class <- splitAsListReturnedClass(flesh)
        if (!extends(ans_class, "CompressedList"))
            stop("don't know how to split or relist a ",
                 class(x), " object as a List")
        skeleton_len <- length(skeleton)
        if (skeleton_len == 0L) {
            flesh_len2 <- 0L
        } else {
            flesh_len2 <- end(skeleton)[skeleton_len]
        }
        if (NROW(flesh) != flesh_len2)
            stop("shape of 'skeleton' is not compatible with 'NROW(flesh)'")
        newCompressedList0(ans_class, flesh, skeleton)
    }
)

setMethod("relist", c("ANY", "List"),
    function(flesh, skeleton)
    {
        relist(flesh, PartitioningByEnd(skeleton))
    }
)

setMethod("relist", c("Vector", "list"),
    function(flesh, skeleton)
    {
        relist(flesh, PartitioningByEnd(skeleton))
    }
)

.splitAsList_by_listlike <- function(x, f, drop)
{
    if (!identical(drop, FALSE))
        warning("'drop' is ignored when 'f' is a list-like object")
    if (!is(f, "PartitioningByEnd"))
        f <- PartitioningByEnd(f)
    relist(x, f)
}

.splitAsList_by_integer <- function(x, f, drop)
{
    if (length(f) > NROW(x))
        stop("'f' cannot be longer than 'NROW(x)' when it's an integer vector")
    idx <- orderInteger(f)
    tmp <- Rle(f[idx])
    f <- cumsum(runLength(tmp))
    names(f) <- as.character(runValue(tmp))
    if (!identical(drop, FALSE))
        warning("'drop' is ignored when 'f' is an integer vector")
    if (length(dim(x)) < 2L)
        x <- x[idx]
    else
        x <- x[idx, , drop=FALSE]
    f <- PartitioningByEnd(f)
    relist(x, f)
}

.splitAsList_by_factor <- function(x, f, drop)
{
    x_NROW <- NROW(x)
    f_len <- length(f)
    f_levels <- levels(f)
    f <- as.integer(f)
    if (f_len > x_NROW)
        f <- head(f, n=x_NROW)
    idx <- orderInteger(f)
    f <- tabulate(f, nbins=length(f_levels))
    names(f) <- f_levels
    if (drop)
        f <- f[f != 0L]
    f <- cumsum(f)
    if (length(dim(x)) < 2L)
        x <- x[idx]
    else
        x <- x[idx, , drop=FALSE]
    f <- PartitioningByEnd(f)
    relist(x, f)
}

.splitAsList_by_integer_Rle <- function(x, f, drop)
{
    if (length(f) > NROW(x))
        stop("'f' cannot be longer than data when it's an integer-Rle")
    f_vals <- runValue(f)
    f_lens <- runLength(f)
    idx <- orderInteger(f_vals)
    xranges <- successiveIRanges(f_lens)[idx]
    tmp <- Rle(f_vals[idx], f_lens[idx])
    f <- cumsum(runLength(tmp))
    names(f) <- as.character(runValue(tmp))
    if (!identical(drop, FALSE))
        warning("'drop' is ignored when 'f' is an integer-Rle")
    x <- seqselect(x, xranges)
    f <- PartitioningByEnd(f)
    relist(x, f)
}

.splitAsList_by_Rle <- function(x, f, drop)
{
    x_NROW <- NROW(x)
    f_len <- length(f)
    f_vals <- runValue(f)
    if (!is.factor(f_vals)) {
        f_vals <- as.factor(f_vals)
        if (f_len > x_NROW) {
            runValue(f) <- f_vals
            f <- head(f, n=x_NROW)
            f_vals <- runValue(f)
        }
    } else if (f_len > x_NROW) {
        f <- head(f, n=x_NROW)
        f_vals <- runValue(f)
    }
    f_lens <- runLength(f)
    f_levels <- levels(f_vals)
    f_vals <- as.integer(f_vals)
    idx <- orderInteger(f_vals)
    xranges <- successiveIRanges(f_lens)[idx]
    ## Using tabulate2() is 5x faster than doing:
    ##   f <- integer(length(f_levels))
    ##   tmp <- Rle(f_vals[idx], f_lens[idx])
    ##   f[runValue(tmp)] <- runLength(tmp)
    f <- tabulate2(f_vals, nbins=length(f_levels), weight=f_lens)
    names(f) <- f_levels
    if (drop)
        f <- f[f != 0L]
    f <- cumsum(f)
    x <- seqselect(x, xranges)
    f <- PartitioningByEnd(f)
    relist(x, f)
}

splitAsList <- function(x, f, drop=FALSE)
{
    if (!isTRUEorFALSE(drop))
        stop("'drop' must be TRUE or FALSE")
    if (is.list(f) || is(f, "List"))
        return(.splitAsList_by_listlike(x, f, drop))
    x_NROW <- NROW(x)
    f_len <- length(f)
    if (f_len < x_NROW) {
        if (f_len == 0L)
            stop("'length(f)' is 0 but 'NROW(x)' is > 0")
        if (x_NROW %% f_len != 0L)
            warning("'NROW(x)' is not a multiple of 'length(f)'")
        f <- rep(f, length.out=x_NROW)
    }
    if (is.integer(f))
        return(.splitAsList_by_integer(x, f, drop))
    if (is.atomic(f) && is.vector(f))
        f <- as.factor(f)
    if (is.factor(f))
        return(.splitAsList_by_factor(x, f, drop))
    if (!is(f, "Rle"))
        stop("'f' must be an atomic vector or a factor (possibly ",
             "in Rle form), or a list-like object")
    f_vals <- runValue(f)
    if (!(is.atomic(f_vals) && is.vector(f_vals)) && !is.factor(f_vals))
        stop("'f' must be an atomic vector or a factor (possibly ",
             "in Rle form), or a list-like object")
    if (is.integer(f_vals))
        return(.splitAsList_by_integer_Rle(x, f, drop))
    return(.splitAsList_by_Rle(x, f, drop))
}

setMethod("split", "Vector",
    function(x, f, drop=FALSE, ...) splitAsList(x, f, drop=drop)
)

`seqsplit<-` <- function(x, f, drop = FALSE, ..., value) {
  if (!isTRUEorFALSE(drop))
    stop("'drop' must be TRUE or FALSE")
  if (length(x) != length(f))
    stop("Length of 'f' must equal the length of 'x'")
  ind <- seqsplit(seq_len(length(x)), f, drop = drop)
  if (length(ind) != length(value))
    stop("Length of 'value' must equal the length of a split on 'f'")
  x[unlist(ind, use.names=FALSE)] <- unlist(value, use.names = FALSE)
  x
}

setReplaceMethod("split", "Vector", function(x, f, drop = FALSE, ..., value) {
  seqsplit(x, f, drop = drop, ...) <- value
  x
})

multisplit <- function(x, f) {
  if (!is.list(f) && !is(f, "List"))
    stop("'f' must be a list")
  if (length(x) != length(f))
    stop("Length of 'f' must equal length of 'x'")
  seqsplit(rep(x, elementLengths(f)), unlist(f, use.names = FALSE))
}

setGeneric("mstack", function(...) standardGeneric("mstack"))

setMethod("mstack", "Vector", function(..., .index.var = "name") {
  if (!isSingleString(.index.var))
    stop("'.index.var' must be a single, non-NA string")
  args <- list(...)
  combined <- do.call(c, unname(args))
  df <- .stack.ind(args, .index.var)
  if (!is.null(mcols(combined)))
    df <- cbind(mcols(combined), df)
  mcols(combined) <- df
  combined
})
setMethod("mstack", "vector",
          function(..., .index.var = "name", .value.var = "value")
          {
            if (!isSingleString(.index.var))
              stop("'.index.var' must be a single, non-NA string")
            args <- list(...)
            combined <- do.call(c, unname(args))
            df <- DataFrame(combined, .stack.ind(args, .index.var))
            colnames(df)[1] <- .value.var
            df
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
setMethod("tapply", "Vector", .tapplyDefault)

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

setMethod("shiftApply", signature(X = "Vector", Y = "Vector"),
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

setMethod("aggregate", "Vector", .aggregateInternal)

setMethod("aggregate", "vector", .aggregateInternal)

setMethod("aggregate", "matrix", stats:::aggregate.default)

setMethod("aggregate", "data.frame", stats:::aggregate.data.frame)

setMethod("aggregate", "ts", stats:::aggregate.ts)

