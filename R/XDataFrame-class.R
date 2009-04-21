### =========================================================================
### XDataFrame objects
### -------------------------------------------------------------------------

## A data.frame-like interface for S4 objects that implement length() and `[`
## Currently tries to coerce objects to XSequence for storage, and
## drops them down to vectors again during extraction. Should probably give
## user control over this.

##setClassUnion("XStringSetORNULL", c("XStringSet", "NULL"))
setClassUnion("XIntegerORNULL", c("XInteger", "NULL"))

## NOTE: Normal data.frames always have rownames (sometimes as integers),
## but we allow the rownames to be NULL for efficiency. This means that we
## need to store the number of rows (nrows).
setClass("XDataFrame",
         representation(
                        rownames = "characterORNULL",
                        nrows = "integer"
                        ),
         prototype(rownames = NULL,
                   ##rowset = NULL,
                   nrows = 0L,
                   compress = FALSE),
         contains = "AnnotatedList")

setClassUnion("XDataFrameORNULL", c("XDataFrame", "NULL"))

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Accessor methods.
###

setMethod("dim", "XDataFrame", function(x) as.integer(c(x@nrows, length(x))))

setMethod("dimnames", "XDataFrame",
          function(x) {
            rn <- x@rownames
            if (!is.null(rn)) {
              rn <- as.character(rn)
              ##rn <- rn[rowset(x)]
              rn[is.na(rn)] <- "NA"
              if (any(duplicated(rn)))
                rn <- make.unique(rn)                
            }
            list(rn, names(x))
          })


setReplaceMethod("dimnames", "XDataFrame",
                 function(x, value) {
                   rows <- value[[1]]
                   if (!is.null(rows)) {
                     if (any(is.na(rows)))
                       stop("missing values not allowed in rownames")
                     if (length(rows) != nrow(x))
                       stop("invalid rownames length")
                     if (any(duplicated(rows)))
                       stop("duplicate rownames not allowed")
                     if (!is(rows, "XStringSet"))
                       ##rows <- BStringSet(as.character(rows))
                       rows <- as.character(rows)
                   }
                   cols <- value[[2]]
                   if (!is.null(cols)) {
                     if (length(cols) > length(x))
                       stop("more column names than columns")
                     cols <- make.names(cols, unique=TRUE)
                   }
                   x@rownames <- rows
                   names(x) <- cols
                   x
                 })

## setGeneric("rowset", function(object, ...) standardGeneric("rowset"))
## setMethod("rowset", "XDataFrame",
##           function(object) {
##             if (is.null(object@rowset))
##               seq_len(nrow(object))
##             else as.integer(object@rowset)
##           })

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Validity.
###


.valid.XDataFrame.dim <- function(x)
{
  nr <- dim(x)[1]
  if (!length(nr) == 1)
    return("length of 'nrows' slot must be 1")
  if (nr < 0)
    return("number of rows must be non-negative")
  NULL
}

.valid.XDataFrame.rownames <- function(x)
{
  if (is.null(rownames(x)))
    return(NULL)
  if (length(rownames(x)) != nrow(x))
    return("number of row names and number of rows differ")
  NULL
}

.valid.XDataFrame.names <- function(x)
{
  if (is.null(names(x)))
    return(NULL)
  if (any(duplicated(names(x))))
    return("duplicate column names")
  if (length(names(x)) != ncol(x))
    return("number of columns and number of column names differ")
  NULL
}

.valid.XDataFrame <- function(x)
{
  c(.valid.XDataFrame.dim(x),
    .valid.XDataFrame.rownames(x),
    .valid.XDataFrame.names(x))
}

setValidity2("XDataFrame", .valid.XDataFrame)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Constructor.
###

XDataFrame <- function(..., row.names = NULL)
{
  ## build up elements list, with names from arguments
  elements <- list(...)
  nr <- 0
  varnames <- character()
  varlist <- vector("list", length(elements))
  if (length(elements) > 0) {
    if (is.null(names(elements)))
      names(elements) <- character(length(elements))
    emptynames <- !nzchar(names(elements))
    args <- as.list(substitute(list(...)))[-1]
    argdp <- sapply(args, function(arg) deparse(arg)[1])
    varnames <- as.list(names(elements))
    nrows <- ncols <- integer(length(varnames))
    for (i in seq_along(elements)) {
      element <- elements[[i]]
      ## if (is(element, "XDataFrame") && ## need to copy if subsetted
##           !all(rowset(element) == seq_len(nrow(element)))) {
##         message("copying XDataFrame argument")
##         element <- as.data.frame(element)
##       }
      element <- try(as(element, "XDataFrame"), silent = TRUE)
      if (inherits(element, "try-error"))
        stop("cannot coerce class \"", class(elements[[i]]),
             "\" to an XDataFrame")
      enames <- names(element)
      if (ncol(element) > 1) {
        if (is.null(enames))
          enames <- seq_len(ncol(element))
        if (emptynames[i])
          varnames[[i]] <- enames
        else varnames[[i]] <- paste(varnames[[i]], enames, sep = ".") 
      } else {
        if (!is.null(enames))
          varnames[[i]] <- enames
        else if (emptynames[i])
          varnames[[i]] <- argdp[i]
      }
      nrows[i] <- nrow(element)
      ncols[i] <- ncol(element)
      varlist[[i]] <- as.list(element, use.names = FALSE)
    }
    nr <- max(nrows)
    for (i in seq_along(elements)) {
      if (nrows[i] < nr && nrows[i] && (nr %% nrows[i]) == 0) {
        recycle <- rep(seq_len(nrows[i]), length = nr)
        varlist[[i]] <- lapply(varlist[[i]], `[`, recycle, drop=FALSE)
        nrows[i] <- nr
      }
    }
    if (!all(nrows == nr))
      stop("different row counts implied by arguments")
    varlist <- unlist(varlist, recursive = FALSE, use.names = FALSE)
    varnames <- make.names(unlist(varnames[ncols > 0L]), unique = TRUE)
  }
  
  if (!is.null(row.names)) {
    if (any(is.na(row.names)))
      stop("missing values in 'row.names'")
    if (length(varlist) && length(row.names) != nr)
      stop("invalid length of row names")
    if (any(duplicated(row.names)))
      stop("duplicate row names")
    ##row.names <- BStringSet(as.character(row.names))
    row.names <- as.character(row.names)
  }
  
  new("XDataFrame", elements=varlist, NAMES=varnames, rownames=row.names,
      nrows=as.integer(max(nr, length(row.names))))
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Subsetting.
###

setMethod("[[", "XDataFrame",
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
            els[[i]][]#[rowset(x)]
          }
          )

setReplaceMethod("[[", "XDataFrame",
                 function(x, i, j,..., value)
                 {
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
                   if (is.numeric(i) && (i < 1L || i > ncol(x)+1))
                     stop("subscript out of bounds")
                   if (!is.null(value) && nrow(x) != length(value)) {
                     stop("length of data must equal the number of rows")
                     ##if (length(value) < 1)
                     ##  stop("data length is positive, 'value' length is 0")
                     ##if (nrow(x) %% length(value) > 0)
                     ##  stop("data not a multiple of replacement length")
                     ##value <- rep(value, length = nrow(x))
                   }
                   x <- callNextMethod(x, i, value=value)
                   ## ensure unique, valid names
                   names(x) <- make.names(names(x), unique=TRUE)
                   x
                 })

setMethod("[", "XDataFrame",
          function(x, i, j, ..., drop)
          {
            if (length(list(...)) > 0)
              warning("parameters in '...' not supported")

            checkIndex <- function(i, row = FALSE) {
              if (row) {
                nms <- rownames(x)
                lx <- nrow(x)
              } else {
                nms <- names(x)
                lx <- length(x)
              }
              if (!is.atomic(i) && !is(i, "Rle"))
                return("invalid subscript type")
              if (!is.null(i) && any(is.na(i)))
                return("subscript contains NAs")
              if (is.numeric(i)) {
                nmi <- i[!is.na(i)]
                if (any(nmi < -lx) || any(nmi > lx))
                  return("subscript out of bounds")
                if (any(nmi < 0) && any(nmi > 0))
                  return("negative and positive indices cannot be mixed")
              } else if (is.logical(i)) {
                if (length(i) > lx)
                  return("subscript out of bounds")
              } else if (is.character(i)) {
                if (is.null(nms))
                  return("cannot subset by character when names are NULL")
                if (row)
                  m <- pmatch(i, nms, duplicates.ok = TRUE)
                else
                  m <- match(i, nms)
                if (!row && any(is.na(m)))
                  return("mismatching names")
              } else if (is(i, "Rle")) {
                if (length(i) > lx)
                  return("subscript out of bounds")
              } else if (!is.null(i)) {
                return("invalid subscript type")
              }
              NULL
            }

            ## no ',' -- forward to list
            ## NOTE: matrix-style subsetting by logical matrix not supported
            if ((nargs() - !missing(drop)) < 3) { 
              if (!missing(drop))
                warning("parameter 'drop' ignored by list-style subsetting")
              if (missing(i))
                return(x)
              prob <- checkIndex(i)
              if (!is.null(prob))
                stop("subsetting as list: ", prob)
              x <- callNextMethod(x, i)
              if (any(duplicated(names(x))))
                names(x) <- make.names(names(x))
              return(x)
            }

            dim <- dim(x)
            if (!missing(j)) {
              prob <- checkIndex(j)
              if (!is.null(prob))
                stop("selecting cols: ", prob)
              x <- callNextMethod(x, j)
              if (any(duplicated(names(x))))
                names(x) <- make.unique(names(x))
              dim[2] <- length(x)
            }
            
            if (!missing(i)) {
              prob <- checkIndex(i, row = TRUE)
              if (!is.null(prob))
                stop("selecting rows: ", prob)
              if (is.character(i))
                i <- pmatch(i, rownames(x), duplicates.ok = TRUE)
              else if (is.logical(i))
                i <- which(i)
              else if (is.null(i))
                i <- integer()
              else if (is(i, "Rle"))
                i <- which(i)
              x@elements <- lapply(as.list(x, use.names = FALSE), `[`, i, drop = FALSE)
              ## newset <- rowset(x)[i]
              ## x@rowset <- as(newset, "XInteger")
              ## NOTE: we do not support matrices here
              ##x@elements <- lapply(as.list(x, use.names = FALSE), "[", i)
              dim[1] <- length(seq(dim[1])[i]) # may have 0 cols, no rownames
              x@nrows <- dim[1]
              x@rownames <- rownames(x)[i]
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

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion.
###

setMethod("is.array", "XDataFrame", function(x) TRUE)

## Break XDataFrame into a normal R data.frame
setAs("XDataFrame", "data.frame",
      function(from) {
        as.data.frame(from)
      })

setMethod("as.data.frame", "XDataFrame",
          function(x, row.names=NULL, optional=FALSE, ...)
          {
            if (length(list(...)))
              stop("arguments in '...' ignored")
            l <- as(x, "list")
            if (is.null(row.names))
              row.names <- rownames(x)
            if (!length(l) && is.null(row.names))
              row.names <- seq_len(nrow(x))
            do.call(data.frame, c(l, list(row.names = row.names)))
          })

## take data.frames to XDataFrames
setAs("data.frame", "XDataFrame",
      function(from) {
        sequences <- from
        new("XDataFrame", elements = sequences, NAMES = names(from),
            nrows = nrow(from), rownames = rownames(from))
      })

setAs("matrix", "XDataFrame", # matrices just go through data.frame
      function(from) as(as.data.frame(from), "XDataFrame"))

setAs("list", "XDataFrame", function(from) do.call(XDataFrame, from))

## everything else
setAs("ANY", "XDataFrame",
      function(from) {
        new("XDataFrame", elements = list(from),
            nrows = as.integer(length(from)))
      })

## setAs("XSequence", "XDataFrame",
##       function(from) {
##         new("XDataFrame", elements = list(from),
##             nrows = as.integer(length(from)))
##       })

#setAs("numeric", "XDataFrame",
##       function(from) {
##         as(as(from, "XSequence"), "XDataFrame")
##       })

### FIXME: only exists due to annoying S4 warning due to its caching of
### coerce methods.
setAs("integer", "XDataFrame",
      function(from) {
        selectMethod("coerce", c("ANY", "XDataFrame"))(from)
      })

## fallback goes throw data.frame
## setAs("ANY", "XDataFrame",
##       function(from) {
##         as(as.data.frame(from, optional = TRUE), "XDataFrame")
##       })

## pull the external vectors into R
setMethod("as.list", "XDataFrame",
          function(x, use.names = TRUE) {
            lapply(callNextMethod(x, use.names = use.names),
                   function(xi) {
                     if (is(xi, "XSequence"))
                       xi[]
                     else 
                       xi
                   })
          })

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "show" method.
###

setMethod("show", "XDataFrame",
          function(object)
          {
            cat("XDataFrame object with", nrow(object), "rows and",
                ncol(object), "columns.\n")
            
            ##show(as(object, "data.frame"))
          })
