### =========================================================================
### XDataFrame objects
### -------------------------------------------------------------------------

## A data.frame-like interface that stores data as XSequences

##setClassUnion("XStringSetORNULL", c("XStringSet", "NULL"))
setClassUnion("XIntegerORNULL", c("XInteger", "NULL"))

## NOTE: Normal data.frames always have rownames (sometimes as integers),
## but we allow the rownames to be NULL for efficiency. This means that we
## need to store the dimensions ('dim').
setClass("XDataFrame",
         representation(
                        rownames = "characterORNULL",
                        ##rownames = "XStringSetORNULL",
                        rowset = "XIntegerORNULL",
                        DIM = "integer" # R doesn't like 'dim'
                        ),
         prototype(rownames = NULL, rowset = NULL),
         contains = "XList")

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Accessor methods.
###

setMethod("dim", "XDataFrame", function(x) x@DIM)

setMethod("dimnames", "XDataFrame",
          function(x) {
            if (!is.null(rownames(x))) {
              rownms <- as.character(rownames(x))
              rownms <- rownms[rowset(x)]
              rownms[is.na(rownms)] <- "NA"
              if (any(duplicated(rownms)))
                rownms <- make.unique(rownms)                
            } else rownms <- as.character(seq(nrow(x)))
            list(rownms, names(x))
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
                   }
                   x@rownames <- rows
                   names(x) <- cols
                   x
                 })

setGeneric("rowset", function(object, ...) standardGeneric("rowset"))
setMethod("rowset", "XDataFrame",
          function(object) {
            if (is.null(rowset))
              seq(nrow(object))
            else as.integer(x@rowset)
          })

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Validity.
###


.valid.XDataFrame.dim <- function(x)
{
  d <- dim(x)
  if (!all(d >= 0))
    return("dimensions must be non-negative")
  NULL
}

.valid.XDataFrame.rownames <- function(x)
{
  if (is.null(rownames(x)))
    return(NULL)
  if (length(rownames(x)) != nrow(x))
    return("number of rownames and number of rows differ")
  NULL
}

.valid.XDataFrame.rownames <- function(x)
{
  if (is.null(names(x)))
    return("column names cannot be NULL")
  if (any(duplicated(names(x))))
    return("duplicate column names")
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

### TODO: make this more like data.frame()
## - accept matrix-like parameters (matrix, data.frame, XDataFrame)
## - determine row.names from arguments
## - protection with I()

XDataFrame <- function(..., row.names = NULL)
{
  elements <- list(...)
  lens <- sapply(elements, length)
  if (length(lens) > 1 && !all(lens[1] == lens))
    stop("all columns must have identical lengths")
  if (!is.null(row.names)) {
    if (any(is.na(row.names)))
      stop("missing values in 'row.names'")
    ##row.names <- BStringSet(as.character(row.names))
    row.names <- as.character(row.names)
    if (any(duplicated(row.names)))
      stop("duplicate row names")
  }
  elements <- lapply(elements, as, "XSequence", strict = FALSE)
  mc <- as.list(match.call())[-1]
  mc <- mc[names(mc) != "row.names"]
  argdp <- lapply(mc, deparse)
  emptynames <- nchar(names(data_list)) == 0
  names(elements)[emptynames] <- make.names(argdp[emptynames], TRUE)
  new("XDataFrame", elements=elements, rownames=row.names)
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Subsetting.
###

setMethod("[[", "XDataFrame",
          function(x, i, j, ...)
          {
            if (!missing(j) || length(list(...)) > 0)
              stop("invalid subsetting")
            if (missing(i))
              stop("subscript is missing")
            if (!is.character(i) && !is.numeric(i))
              stop("invalid subscript type")
            if (length(i) < 1L)
              stop("attempt to select less than one element")
            if (length(i) > 1L)
              stop("attempt to select more than one element")
            if (i < 1L || i > length(x))
              stop("subscript out of bounds")
            callNextMethod(x, i)[rowset(x)]
          }
          )

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
              if (!is.atomic(i))
                return("invalid subscript type")
              if (any(is.na(i)))
                return("subscript contains NAs")
              if (is.numeric(i)) {
                if (any(i < -lx) || any(i > lx))
                  return("subscript out of bounds")
                if (any(i < 0) && any(i > 0))
                  return("negative and positive indices cannot be mixed")
              } else if (is.logical(i)) {
                if (length(i) > lx)
                  return("subscript out of bounds")
              } else if (is.character(i)) {
                if (is.null(nms))
                  return("cannot subset by character when names are NULL")
                if (row)
                  m <- pmatch(i, nms, duplicates.ok = TRUE)
                else m <- match(i, nms)
                if (any(!is.na(nms) & is.na(m)))
                  return("mismatching names")
              } else if (!is.null(i)) {
                return("invalid subscript type")
              }
              NULL
            }

            ## no ',' -- forward to list
            ## NOTE: matrix-style subsetting by logical matrix not supported
            if (nargs() - !missing(drop) < 3) { 
              if (!missing(drop))
                warning("parameter 'drop' ignored by list-style subsetting")
              if (missing(i))
                return(x)
              prob <- checkIndex(i, ncol(x), names(x))
              if (!is.null(prob))
                stop("subsetting as list: ", prob)
              x <- callNextMethod(x, i)
              if (any(duplicated(names(x))))
                names(x) <- make.names(names(x))
              return(x)
            }

            dim <- dim(x)
            if (!missing(j)) {
              prob <- checkIndex(i, ncol(x), names(x))
              if (!is.null(prob))
                stop("subsetting cols: ", prob)
              x <- callNextMethod(x, j)
              if (any(duplicated(names(x))))
                names(x) <- make.unique(names(x))
              dim[2] <- length(x)
            }
            
            if (!missing(i)) {
              prob <- checkIndex(i, dim[1], rownames(x))
              if (!is.null(prob))
                stop("subsetting rows: ", prob)
              x@rowset <- XInteger(rowset(x)[i])
              ## NOTE: we do not support matrices here
              ##x@elements <- lapply(elements(x), "[", i)
              dim[1] <- length(seq(dim[1])[i]) # may have 0 cols, no rownames 
            }

            if (missing(drop)) ## drop by default if only one column left
              drop <- dim[2] == 1
            if (drop) {
              ## one column left
              if (dim[2] == 1) 
                return(x[[1]])
              ## one row left
              if (dim[1] == 1)
                return(as(x, "XList"))
            }
            
            x@DIM <- dim
            x
          })

setReplaceMethod("[", c("XDataFrame", value = "XSequence"),
                 function(x, i, j, ..., value)
                 {
                   stop("attempt to modify the value of a ", class(x),
                        " instance")
                 })

## setReplaceMethod("[", "XDataFrame",
##                  function(x, i, j, ..., value)
##                  {
##                    value <- as(value, "XSequence", FALSE)
##                    callGeneric()
##                  })

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion.
###

### Break XDataFrame into a normal R data.frame
setAs("XDataFrame", "data.frame",
      function(from) {
        as.data.frame(as(from, "list"))
      })


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "show" method.
###

setMethod("show", "XDataFrame",
          function(object)
          {
            show(as(object, "data.frame"))
          })
