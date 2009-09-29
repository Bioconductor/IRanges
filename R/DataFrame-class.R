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
                   nrows = 0L),
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
            cn <- names(x)
            if (is.null(cn) && !do.NULL) {
              nc <- NCOL(x)
              if (nc > 0L) 
                cn <- paste(prefix, seq_len(nc), sep = "")
              else
                cn <- character(0L)
            }
            cn
          })

setReplaceMethod("rownames", "DataFrame",
                 function(x, value)
                 {
                   if (!is.null(value)) {
                     if (any(is.na(value)))
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
                   if (!is.null(value)) {
                     if (length(value) > length(x))
                       stop("more column names than columns")
                     value <- make.names(value, unique=TRUE)
                   }
                   names(x) <- value
                   x
                 })

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Validity.
###


.valid.DataFrame.dim <- function(x)
{
  nr <- dim(x)[1]
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
  if (is.null(names(x)))
    return(NULL)
  if (anyDuplicated(names(x)))
    return("duplicate column names")
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

DataFrame <- function(..., row.names = NULL)
{
  ## build up listData, with names from arguments
  nr <- 0
  listData <- list(...)
  varlist <- vector("list", length(listData))
  if (length(listData) > 0) {
    dotnames <- names(listData)
    dotvalues <- 
      sapply(as.list(substitute(list(...)))[-1],
             function(arg) deparse(arg)[1])
    if (is.null(dotnames)) {
      emptynames <- rep.int(TRUE, length(listData))
      names(listData) <- dotvalues
    } else {
      emptynames <- !nzchar(names(listData))
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
             "\" to an DataFrame")
      nrows[i] <- nrow(element)
      ncols[i] <- ncol(element)
      varlist[[i]] <- as.list(element, use.names = FALSE)
      if (length(dim(listData[[i]])) > 1) {
        if (emptynames[i])
          varnames[[i]] <- colnames(element)
        else
          varnames[[i]] <- paste(varnames[[i]], colnames(element), sep = ".")
      }
    }
    nr <- max(nrows)
    for (i in which((nrows > 0L) & (nrows < nr) & (nr %% nrows == 0L))) {
      recycle <- rep(seq_len(nrows[i]), length = nr)
      varlist[[i]] <- lapply(varlist[[i]], `[`, recycle, drop=FALSE)
      nrows[i] <- nr
    }
    if (!all(nrows == nr))
      stop("different row counts implied by arguments")
    varlist <- unlist(varlist, recursive = FALSE, use.names = FALSE)
    names(varlist) <- make.names(unlist(varnames[ncols > 0L]), unique = TRUE)
  }
  
  if (!is.null(row.names)) {
    if (any(is.na(row.names)))
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

XDataFrame <- function(..., row.names = NULL) {
  .Deprecated("DataFrame", "IRanges")
  mc <- match.call()
  mc[1] <- DataFrame
  eval(mc)
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
            els[[i]]
          }
          )

setReplaceMethod("[[", "DataFrame",
                 function(x, i, j,..., value)
                 {
                   nrx <- nrow(x)
                   lv <- length(value)
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
              iInfo <- .bracket.Index(i, colnames(x), ncol(x))
              if (!is.null(iInfo[["msg"]]))
                stop("subsetting as list: ", iInfo[["msg"]])
              x <- callNextMethod(x, iInfo[["idx"]])
              if (anyDuplicated(names(x)))
                names(x) <- make.names(names(x))
              return(x)
            }

            dim <- dim(x)
            if (!missing(j)) {
              jInfo <- .bracket.Index(j, colnames(x), ncol(x))
              if (!is.null(jInfo[["msg"]]))
                stop("selecting cols: ", jInfo[["msg"]])
              x <- callNextMethod(x, jInfo[["idx"]])
              if (anyDuplicated(names(x)))
                names(x) <- make.unique(names(x))
              dim[2] <- length(x)
            }

            if (!missing(i)) {
              iInfo <- .bracket.Index(i, rownames(x), nrow(x), dup.nms = TRUE)
              if (!is.null(iInfo[["msg"]]))
                stop("selecting rows: ", iInfo[["msg"]])
              useI <- iInfo[["useIdx"]]
              i <- iInfo[["idx"]]
              if (useI) {
                x@listData <- lapply(as.list(x), function(y) y[i, drop = FALSE])
                dim[1] <- length(seq(dim[1])[i]) # may have 0 cols, no rownames
                x@nrows <- dim[1]
                rn <- rownames(x)[i]
                if (anyDuplicated(rn))
                  x@rownames <- make.unique(rn)
                else
                  x@rownames <- rn
              }
            }

            if (missing(drop)) ## drop by default if only one column left
              drop <- dim[2] == 1
            if (drop) {
              ## one column left
              if (dim[2] == 1) 
                return(x[[1]])
              ## one row left
              if (dim[1] == 1)
                return(as(x, "list"))
            }

            x
          })

setReplaceMethod("[", "DataFrame",
                 function(x, i, j, ..., value)
                 {
                   if (length(list(...)) > 0)
                     warning("parameters in '...' not supported")

                   if (missing(i)) {
                     iInfo <- list(msg = NULL, useIdx = FALSE, idx = NULL)
                   } else {
                     iInfo <- .bracket.Index(i, rownames(x), nrow(x), dup.nms = TRUE)
                     if (!is.null(iInfo[["msg"]]))
                       stop("replacing rows: ", iInfo[["msg"]])
                     i <- iInfo[["idx"]]
                   }
                   useI <- iInfo[["useIdx"]]
                   if (missing(j)) {
                     jInfo <- list(msg = NULL, useIdx = FALSE, idx = NULL)
                     j <- seq_len(ncol(x))
                   } else {
                     jInfo <- .bracket.Index(j, colnames(x), ncol(x))
                     if (!is.null(jInfo[["msg"]]))
                       stop("replacing cols: ", jInfo[["msg"]])
                     j <- jInfo[["idx"]]
                   }
                   if (!is(value, "DataFrame")) {
                     if (useI)
                       li <- length(i)
                     else
                       li <- nrow(x)
                     lv <- length(value)
                     if (li != lv) {
                       if ((li == 0) || (li %% lv != 0))
                         stop(paste(lv, "rows in value to replace",
                                    li, "rows"))
                       else
                         value <- rep(value, length.out = li)
                     }
                     if (useI) {
                       x@listData[j] <-
                         lapply(x@listData[j], function(y) {y[i] <- value; y})
                     } else {
                       x@listData[j] <- list(value)
                     }
                   } else {
                     if (ncol(value) != length(j))
                       stop("ncol(x[j]) != ncol(value)")
                     if (useI)
                       li <- length(i)
                     else
                       li <- nrow(x)
                     nrv <- nrow(value)
                     if (li != nrv) {
                       if ((li == 0) || (li %% nrv != 0))
                         stop(paste(nrv, "rows in value to replace",
                                    li, "rows"))
                       else
                         value <-
                           value[rep(seq_len(nrv), length.out = li), ,
                                 drop=FALSE]
                     }
                     if (useI) {
                       for (k in seq_len(length(j)))
                         x@listData[[j[[k]]]][i] <- value[[k]]
                     } else {
                       for (k in seq_len(length(j)))
                         x@listData[[j[[k]]]] <- value[[k]]
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

setMethod("as.data.frame", "DataFrame",
          function(x, row.names=NULL, optional=FALSE, ...)
          {
            if (length(list(...)))
              stop("arguments in '...' ignored")
            l <- as(x, "list")
            if (is.null(row.names))
              row.names <- rownames(x)
            if (!length(l) && is.null(row.names))
              row.names <- seq_len(nrow(x))
            ## we call as.data.frame here, because data.frame uses S3 dispatch
            do.call(data.frame, c(lapply(l, as.data.frame, optional = TRUE),
                                  list(row.names = row.names)))
          })

## take data.frames to DataFrames
setAs("data.frame", "DataFrame",
      function(from) {
        rn <- attributes(from)[["row.names"]]
        if (is.integer(rn))
          rn <- NULL
        rownames(from) <- NULL
        new("DataFrame", listData = as.list(from),
            nrows = nrow(from), rownames = rn)
      })

# matrices just go through data.frame
setAs("matrix", "DataFrame",
      function(from) as(as.data.frame(from), "DataFrame"))

setAs("vector", "DataFrame",
      function(from) {
        new("DataFrame", listData = structure(list(unname(from)), names = "X"),
            nrows = length(from), rownames = names(from))
      })

### FIXME: only exists due to annoying S4 warning due to its caching of
### coerce methods.
setAs("integer", "DataFrame",
      function(from) {
        selectMethod("coerce", c("vector", "DataFrame"))(from)
      })

setAs("Sequence", "DataFrame",
      function(from) {
        new("DataFrame", listData = structure(list(unname(from)), names = "X"),
            nrows = length(from), rownames = names(from))
      })
