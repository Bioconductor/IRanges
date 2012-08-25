### =========================================================================
### DataFrame objects
### -------------------------------------------------------------------------

## A data.frame-like interface for S4 objects that implement length() and `[`

## NOTE: Normal data.frames always have rownames (sometimes as integers),
## but we allow the rownames to be NULL for efficiency. This means that we
## need to store the number of rows (nrows).
setClass("DataFrame",
         representation(
                        rownames = "characterORNULL",
                        nrows = "integer"
                        ),
         prototype(rownames = NULL,
                   nrows = 0L,
                   listData = structure(list(), names = character())),
         contains = c("DataTable", "SimpleList"))

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Accessor methods.
###

setMethod("nrow", "DataFrame", function(x) x@nrows)

setMethod("ncol", "DataFrame", function(x) length(x))

setMethod("rownames", "DataFrame",
          function(x, do.NULL = TRUE, prefix = "row")
          {
            rn <- x@rownames
            if (is.null(rn) && !do.NULL) {
              nr <- NROW(x)
              if (nr > 0L) 
                rn <- paste(prefix, seq_len(nr), sep = "")
              else
                rn <- character(0L)
            }
            rn
          })

setMethod("colnames", "DataFrame",
          function(x, do.NULL = TRUE, prefix = "col")
          {
            if (!identical(do.NULL, TRUE)) warning("do.NULL arg is ignored ",
                "in this method")
            cn <- names(x@listData)
            if (!is.null(cn))
                return(cn)
            if (length(x@listData) != 0L)
                stop("DataFrame object with NULL colnames, please fix it ",
                     "with colnames(x) <- value")
            return(character(0)) 
          })

setReplaceMethod("rownames", "DataFrame",
                 function(x, value)
                 {
                   if (!is.null(value)) {
                     if (anyMissing(value))
                       stop("missing values not allowed in rownames")
                     if (length(value) != nrow(x))
                       stop("invalid rownames length")
                     if (anyDuplicated(value))
                       stop("duplicate rownames not allowed")
                     if (!is(value, "XStringSet"))
                       value <- as.character(value)
                   }
                   x@rownames <- value
                   x
                 })

setReplaceMethod("colnames", "DataFrame",
                 function(x, value)
                 {
                   if (!is.character(value)) 
                       stop("'value' must be a character vector ",
                            "in colnames(x) <- value")
                   if (length(value) > length(x))
                     stop("more column names than columns")
                   names(x) <- value
                   x
                 })

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Validity.
###


.valid.DataFrame.dim <- function(x)
{
  nr <- dim(x)[1L]
  if (!length(nr) == 1)
    return("length of 'nrows' slot must be 1")
  if (nr < 0)
    return("number of rows must be non-negative")
  NULL
}

.valid.DataFrame.rownames <- function(x)
{
  if (is.null(rownames(x)))
    return(NULL)
  if (length(rownames(x)) != nrow(x))
    return("number of row names and number of rows differ")
  NULL
}

.valid.DataFrame.names <- function(x)
{
  ## DataFrames with no columns can have NULL column name
  if (is.null(names(x)) && ncol(x) != 0)
    return("column names should not be NULL")
  if (length(names(x)) != ncol(x))
    return("number of columns and number of column names differ")
  NULL
}

.valid.DataFrame <- function(x)
{
  c(.valid.DataFrame.dim(x),
    .valid.DataFrame.rownames(x),
    .valid.DataFrame.names(x))
}

setValidity2("DataFrame", .valid.DataFrame)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Constructor.
###

DataFrame <- function(..., row.names = NULL, check.names = TRUE)
{
  ## build up listData, with names from arguments
  if (!isTRUEorFALSE(check.names))
    stop("'check.names' must be TRUE or FALSE")
  nr <- 0
  listData <- list(...)
  varlist <- vector("list", length(listData))
  if (length(listData) > 0) {
    dotnames <- names(listData)
    dotvalues <- 
      sapply(as.list(substitute(list(...)))[-1L],
             function(arg) deparse(arg)[1L])
    if (is.null(dotnames)) {
      emptynames <- rep.int(TRUE, length(listData))
      names(listData) <- dotvalues
    } else {
      emptynames <- !nzchar(dotnames)
      if (any(emptynames)) {
        names(listData)[emptynames] <- dotvalues[emptynames]
      }
    }
    varnames <- as.list(names(listData))
    nrows <- ncols <- integer(length(varnames))
    for (i in seq_along(listData)) {
      element <- try(as(listData[[i]], "DataFrame"), silent = TRUE)
      if (inherits(element, "try-error"))
        stop("cannot coerce class \"", class(listData[[i]]),
             "\" to a DataFrame")
      nrows[i] <- nrow(element)
      ncols[i] <- ncol(element)
      varlist[[i]] <- as.list(element, use.names = FALSE)
      if ((length(dim(listData[[i]])) > 1) || (ncol(element) > 1)) {
        if (emptynames[i])
          varnames[[i]] <- colnames(element)
        else
          varnames[[i]] <- paste(varnames[[i]], colnames(element), sep = ".")
      } else if (is.list(listData[[i]]) && length(names(listData[[i]])))
          varnames[[i]] <- names(element)
    }
    nr <- max(nrows)
    for (i in which((nrows > 0L) & (nrows < nr) & (nr %% nrows == 0L))) {
      recycle <- rep(seq_len(nrows[i]), length.out = nr)
      varlist[[i]] <- lapply(varlist[[i]], `[`, recycle, drop=FALSE)
      nrows[i] <- nr
    }
    if (!all(nrows == nr))
      stop("different row counts implied by arguments")
    varlist <- unlist(varlist, recursive = FALSE, use.names = FALSE)
    nms <- unlist(varnames[ncols > 0L])
    if (check.names)
      nms <- make.names(nms, unique = TRUE)
    names(varlist) <- nms
  } else names(varlist) <- character(0)
  
  if (!is.null(row.names)) {
    if (anyMissing(row.names))
      stop("missing values in 'row.names'")
    if (length(varlist) && length(row.names) != nr)
      stop("invalid length of row names")
    if (anyDuplicated(row.names))
      stop("duplicate row names")
    row.names <- as.character(row.names)
  }
  
  new2("DataFrame", listData=varlist, rownames=row.names,
       nrows=as.integer(max(nr, length(row.names))), check=FALSE)
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Subsetting.
###

setMethod("[[", "DataFrame",
          function(x, i, j, ...)
          {
            dotArgs <- list(...)
            if (length(dotArgs) > 0)
              dotArgs <- dotArgs[names(dotArgs) != "exact"]
            if (!missing(j) || length(dotArgs) > 0)
              stop("invalid subsetting")
            if (missing(i))
              stop("subscript is missing")
            if (!is.character(i) && !is.numeric(i))
              stop("invalid subscript type")
            if (length(i) < 1L)
              stop("attempt to select less than one element")
            if (length(i) > 1L)
              stop("attempt to select more than one element")
            if (!is.character(i) && !is.na(i) && (i < 1L || i > length(x)))
              stop("subscript out of bounds")
            els <- as.list(x, use.names = FALSE)
            names(els) <- names(x)
            els[[i, ...]]
          }
          )

setReplaceMethod("[[", "DataFrame",
                 function(x, i, j,..., value)
                 {
                   nrx <- nrow(x)
                   lv <- NROW(value)
                   if (!missing(j) || length(list(...)) > 0)
                     warning("arguments beyond 'i' ignored")
                   if (missing(i))
                     stop("subscript is missing")
                   if (!is.character(i) && !is.numeric(i))
                     stop("invalid subscript type")
                   if (length(i) < 1L)
                     stop("attempt to select less than one element")
                   if (length(i) > 1L)
                     stop("attempt to select more than one element")
                   if (is.numeric(i) && (i < 1L || i > ncol(x) + 1L))
                     stop("subscript out of bounds")
                   if (!is.null(value) && (nrx != lv)) {
                     if ((nrx == 0) || (nrx %% lv != 0))
                       stop(paste(lv, "elements in value to replace",
                                  nrx, "elements"))
                     else
                       value <- rep(value, length.out = nrx)
                   }
                   x <- callNextMethod(x, i, value=value)
                   ## ensure unique, valid names
                   names(x) <- make.names(names(x), unique=TRUE)
                   x
                 })

setMethod("[", "DataFrame",
          function(x, i, j, ..., drop)
          {
            if (length(list(...)) > 0)
              warning("parameters in '...' not supported")

            ## no ',' -- forward to list
            ## NOTE: matrix-style subsetting by logical matrix not supported
            if ((nargs() - !missing(drop)) < 3) { 
              if (!missing(drop))
                warning("parameter 'drop' ignored by list-style subsetting")
              if (missing(i))
                return(x)
              iInfo <- .bracket.Index(i, ncol(x), colnames(x))
              if (!is.null(iInfo[["msg"]]))
                stop("subsetting as list: ", iInfo[["msg"]])
              x <- callNextMethod(x, iInfo[["idx"]])
              if (anyDuplicated(names(x)))
                names(x) <- make.names(names(x))
              return(x)
            }

            dim <- dim(x)
            if (!missing(j)) {
              jInfo <- .bracket.Index(j, ncol(x), colnames(x))
              if (!is.null(jInfo[["msg"]]))
                stop("selecting cols: ", jInfo[["msg"]])
              x <- callNextMethod(x, jInfo[["idx"]])
              if (anyDuplicated(names(x)))
                names(x) <- make.unique(names(x))
              dim[2L] <- length(x)
            }

            if (!missing(i)) {
              iInfo <- .bracket.Index(i, nrow(x), rownames(x), dup.nms = TRUE)
              if (!is.null(iInfo[["msg"]]))
                stop("selecting rows: ", iInfo[["msg"]])
              useI <- iInfo[["useIdx"]]
              i <- iInfo[["idx"]]
              if (useI) {
                x@listData <- lapply(as.list(x), function(y) {
                  if (length(dim(y)) > 1)
                    y[i, , drop = FALSE]
                  else y[i, drop = FALSE]
                })
                dim[1L] <- length(seq(dim[1L])[i]) #may have 0 cols, no rownames
                x@nrows <- dim[1L]
                rn <- rownames(x)[i]
                if (anyDuplicated(rn))
                  x@rownames <- make.unique(rn)
                else
                  x@rownames <- rn
              }
            }

            if (missing(drop)) ## drop by default if only one column left
              drop <- dim[2L] == 1
            if (drop) {
              ## one column left
              if (dim[2L] == 1) 
                return(x[[1L]])
              ## one row left
              if (dim[1L] == 1)
                return(as(x, "list"))
            }

            x
          })

setReplaceMethod("[", "DataFrame",
                 function(x, i, j, ..., value)
                 {
                   if (length(list(...)) > 0)
                     warning("parameters in '...' not supported")

                   if (nargs() < 4) {
                     iInfo <- list(msg = NULL, useIdx = FALSE, idx = NULL)
                     if (missing(i)) {
                       jInfo <-
                         list(msg = NULL, useIdx = FALSE, idx = seq_len(ncol(x)))
                     } else {
                       jInfo <- .bracket.Index(i, ncol(x), colnames(x),
                                               allowAppend = TRUE)
                     }
                   } else {
                     if (missing(i)) {
                       iInfo <- list(msg = NULL, useIdx = FALSE, idx = NULL)
                     } else {
                       iInfo <-
                         .bracket.Index(i, nrow(x), rownames(x),
                                        allowAppend = TRUE)
                     }
                     if (missing(j)) {
                       jInfo <-
                         list(msg = NULL, useIdx = FALSE, idx = seq_len(ncol(x)))
                     } else {
                       jInfo <- .bracket.Index(j, ncol(x), colnames(x),
                                               allowAppend = TRUE)
                     }
                   }
                   if (!is.null(iInfo[["msg"]]))
                     stop("replacing rows: ", iInfo[["msg"]])
                   if (!is.null(jInfo[["msg"]]))
                     stop("replacing cols: ", jInfo[["msg"]])
                   i <- iInfo[["idx"]]
                   j <- jInfo[["idx"]]
                   newcn <- jInfo[["newNames"]]
                   newrn <- iInfo[["newNames"]]
                   if (!length(j)) # nothing to replace
                     return(x)
                   useI <- iInfo[["useIdx"]]
                   if (is(value, "list") || is(value, "List"))
                     value <- as(value, "DataFrame")
                   if (!is(value, "DataFrame")) {
                     if (useI)
                       li <- length(i)
                     else
                       li <- nrow(x)
                     lv <- length(value)
                     if (li != lv) {
                       if ((li == 0) || (li %% lv != 0))
                         stop(paste(lv, "rows in value to replace",
                                    li, " rows"))
                       else
                         value <- rep(value, length.out = li)
                     }
                     ## come up with some default row and col names
                     if (!length(newcn) && max(j) > length(x)) {
                       newcn <- paste("V", seq.int(length(x) + 1L, max(j)),
                                      sep = "")
                       if (length(newcn) != sum(j > length(x)))
                         stop("new columns would leave holes after",
                              "existing columns")
                     }
                     if (useI) {
                       if (length(newrn) == 0L && li > 0L && max(i) > nrow(x))
                         newrn <- as.character(seq.int(nrow(x) + 1L, max(i)))
                       x@listData[j] <-
                         lapply(x@listData[j], function(y) {y[i] <- value; y})
                     } else {
                       x@listData[j] <- list(value)
                     }
                   } else {
                     vc <- seq_len(ncol(value))
                     if (ncol(value) > length(j))
                       stop("ncol(x[j]) < ncol(value)")
                     if (ncol(value) < length(j))
                       vc <- rep(vc, length.out = length(j))
                     if (useI)
                       li <- length(i)
                     else
                       li <- nrow(x)
                     nrv <- nrow(value)
                     if (li != nrv) {
                       if ((li == 0) || (li %% nrv != 0))
                         stop(paste(nrv, "rows in value to replace",
                                    li, " rows"))
                       else
                         value <-
                           value[rep(seq_len(nrv), length.out = li), ,
                                 drop=FALSE]
                     }
                     ## attempt to derive new row and col names from value
                     if (!length(newcn) && max(j) > length(x)) {
                       newcn <- rep(names(value), length.out = length(j))
                       newcn <- newcn[j > length(x)]
                     }
                     if (useI) {
                       if (length(newrn) == 0L && li > 0L && max(i) > nrow(x)) {
                         if (!is.null(rownames(value))) {
                           newrn <- rep(rownames(value), length.out = length(i))
                           newrn <- newrn[i > nrow(x)]
                         } else newrn <-
                           as.character(seq.int(nrow(x) + 1L, max(i)))
                       }
                       for (k in seq_len(length(j))) {
                         if (j[k] > length(x))
                           v <- NULL
                         else v <- x@listData[[j[k]]]
                         v[i] <- value[[vc[k]]]
                         x@listData[[j[k]]] <- v
                       }
                     } else {
                       for (k in seq_len(length(j)))
                         x@listData[[j[k]]] <- value[[vc[k]]]
                     }
                   }
                   ## update row and col names, making them unique
                   if (length(newcn)) {
                     oldcn <- head(colnames(x), length(x) - length(newcn))
                     colnames(x) <- make.unique(c(oldcn, newcn))
                     if (!is.null(mcols(x)))
                       mcols(x)[tail(names(x),length(newcn)),] <-
                         DataFrame(NA)
                   }
                   if (length(newrn)) {
                     notj <- setdiff(seq_len(ncol(x)), j)
                     x@listData[notj] <-
                       lapply(x@listData[notj],
                              function(y) c(y, rep(NA, length(newrn))))
                     x@rownames <- make.unique(c(rownames(x), newrn))
                   }
                   x@nrows <- length(x[[1]]) # we should always have a column
                   x
                 })

setMethod("seqselect", "DataFrame",
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
                      slot(x, "listData", check=FALSE) <-
                        lapply(structure(seq_len(ncol(x)),
                                         names = names(x)),
                               function(i) seqselect(x[[i]], ir))
                      slot(x, "nrows", check=FALSE) <- sum(width(ir))
                      if (!is.null(rownames(x))) {
                          slot(x, "rownames", check=FALSE) <-
                            make.unique(seqselect(rownames(x), ir))
                      }
                  }
              }
              x
          })

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion.
###

## Break DataFrame into a normal R data.frame
setAs("DataFrame", "data.frame",
      function(from) {
        as.data.frame(from)
      })

injectIntoScope <- function(x, ...) {
  nms <- sapply(tail(substitute(list(...)), -1), deparse)
  environment(x) <- list2env(setNames(list(...), nms), parent = environment(x))
  x
}

setMethod("as.data.frame", "DataFrame",
          function(x, row.names=NULL, optional=FALSE, ...)
          {
            l <- as(x, "list")
            if (is.null(row.names))
              row.names <- rownames(x)
            if (!length(l) && is.null(row.names))
              row.names <- seq_len(nrow(x))
            l <- lapply(l,
                   function(y) {
                     if (is(y, "SimpleList") || is(y, "CompressedList"))
                       y <- as.list(y)
                     if (is.list(y))
                       y <- I(y)
                     y
                   })
            IRanges.data.frame <- injectIntoScope(data.frame, as.data.frame)
            do.call(IRanges.data.frame, c(l, list(row.names = row.names)))
          })

setMethod("as.matrix", "DataFrame", function(x) {
  if (length(x) == 0L)
    m <- matrix(logical(), nrow = nrow(x), ncol = 0L)
  else m <- do.call(cbind, as.list(x))
  rownames(m) <- rownames(x)
  m
})

## take data.frames to DataFrames
setAs("data.frame", "DataFrame",
      function(from) {
        rn <- attributes(from)[["row.names"]]
        if (is.integer(rn))
          rn <- NULL
        rownames(from) <- NULL
        new2("DataFrame", listData = as.list(from),
             nrows = nrow(from), rownames = rn, check=FALSE)
      })

# matrices and tables just go through data.frame
setAs("matrix", "DataFrame", function(from) {
    df <- as.data.frame(from)
    if (0L == ncol(from))
        ## colnames on matrix with 0 columns are 'NULL'
        names(df) <- character()
    as(df, "DataFrame")
})

setAs("table", "DataFrame",
      function(from) {
        df <- as.data.frame(from)
        factors <- sapply(df, is.factor)
        factors[1] <- FALSE
        do.call(DataFrame, c(df[1], lapply(df[factors], Rle), df["Freq"]))
      })

setAs("xtabs", "DataFrame",
      function(from) {
        class(from) <- "table"
        as(from, "DataFrame")
      })

setAs("vector", "DataFrame",
      function(from) {
        new2("DataFrame", listData = structure(list(unname(from)), names = "X"),
             nrows = length(from), rownames = names(from), check=FALSE)
      })

## note that any element named 'row.names' will be interpreted differently
## is this a bug or a feature?
setAs("list", "DataFrame",
      function(from) {
        do.call(DataFrame, c(from, check.names = FALSE))
      })

setAs("NULL", "DataFrame", function(from) as(list(), "DataFrame"))

### FIXME: only exists due to annoying S4 warning due to its caching of
### coerce methods.
setAs("integer", "DataFrame",
      function(from) {
        selectMethod("coerce", c("vector", "DataFrame"))(from)
      })

setAs("Vector", "DataFrame",
      function(from) {
        new2("DataFrame", listData = setNames(list(unname(from)), "X"),
             nrows = length(from), rownames = names(from), check=FALSE)
      })

setAs("AsIs", "DataFrame",
      function(from) {
        if (length(class(from)) > 1)
          class(from) <- tail(class(from), -1)
        else from <- unclass(from)
        df <- new2("DataFrame", nrows = NROW(from), check=FALSE)
        df[[1]] <- from
        df
      })
