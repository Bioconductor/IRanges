### =========================================================================
### AtomicList object implementations
### -------------------------------------------------------------------------

## Possible optimizations for compressed lists:
## - order/sort: unlist, order by split factor first
## - cumsum: unlist, cumsum and subtract offsets

.ATOMIC_TYPES <- c("logical", "integer", "numeric", "complex",
                   "character", "raw")
setClassUnion("atomic", .ATOMIC_TYPES)

## A list that holds atomic objects

setClass("CompressedAtomicList",
         contains =  c("AtomicList", "CompressedList"),
         representation("VIRTUAL"))

setClass("SimpleAtomicList",
         contains =  c("AtomicList", "SimpleList"),
         representation("VIRTUAL"))
 
setClass("CompressedLogicalList",
         prototype = prototype(elementType = "logical",
                               unlistData = logical()),
         contains = c("LogicalList", "CompressedAtomicList"))
setClass("SimpleLogicalList",
         prototype = prototype(elementType = "logical"),
         contains = c("LogicalList", "SimpleAtomicList"))

setClass("CompressedIntegerList",
         prototype = prototype(elementType = "integer",
                               unlistData = integer()),
         contains = c("IntegerList", "CompressedAtomicList"))
setClass("SimpleIntegerList",
         prototype = prototype(elementType = "integer"),
         contains = c("IntegerList", "SimpleAtomicList"))

setClass("CompressedNumericList",
         prototype = prototype(elementType = "numeric",
                               unlistData = numeric()),
         contains = c("NumericList", "CompressedAtomicList"))
setClass("SimpleNumericList",
         prototype = prototype(elementType = "numeric"),
         contains = c("NumericList", "SimpleAtomicList"))

setClass("CompressedComplexList",
         prototype = prototype(elementType = "complex",
                               unlistData = complex()),
         contains = c("ComplexList", "CompressedAtomicList"))
setClass("SimpleComplexList",
         prototype = prototype(elementType = "complex"),
         contains = c("ComplexList", "SimpleAtomicList"))

setClass("CompressedCharacterList",
         prototype = prototype(elementType = "character",
                               unlistData = character()),
         contains = c("CharacterList", "CompressedAtomicList"))
setClass("SimpleCharacterList",
         prototype = prototype(elementType = "character"),
         contains = c("CharacterList", "SimpleAtomicList"))

setClass("CompressedRawList",
         prototype = prototype(elementType = "raw",
                               unlistData = raw()),
         contains = c("RawList", "CompressedAtomicList"))
setClass("SimpleRawList",
         prototype = prototype(elementType = "raw"),
         contains = c("RawList", "SimpleAtomicList"))

setClass("CompressedRleList",
         prototype = prototype(elementType = "Rle",
                               unlistData = new("Rle")),
         contains = c("RleList", "CompressedAtomicList"))
setClass("SimpleRleList",
         prototype = prototype(elementType = "Rle"),
         contains = c("RleList", "SimpleAtomicList"))

setClass("CompressedFactorList",
         prototype = prototype(elementType = "factor",
           unlistData = factor()),
         contains = c("FactorList", "CompressedAtomicList"))
setClass("SimpleFactorList",
         prototype = prototype(elementType = "factor"),
         contains = c("FactorList", "SimpleAtomicList"))


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Constructors
###

.dotargsAsList <- function(type, ...) {
  listData <- list(...)
  if (length(listData) == 1) {
      arg1 <- listData[[1]]
      if (is.list(arg1) || is(arg1, "List"))
        listData <- arg1
      else if (type == "integer" && class(arg1) == "character")
        listData <- strsplitAsListOfIntegerVectors(arg1) # weird special case
  }
  listData
}

AtomicListConstructor <- function(type, compress.default = TRUE) {
  constructor <- eval(substitute(function(..., compress = compress.default) {
    if (!isTRUEorFALSE(compress))
      stop("'compress' must be TRUE or FALSE")
    listData <- .dotargsAsList(type, ...)
    CompressedOrSimple <- if (compress) "Compressed" else "Simple"
    if (is(listData, S4Vectors:::listClassName(CompressedOrSimple, type)))
      listData
    else CoercerToList(type, compress)(listData)
  }, list(type = type)))
  formals(constructor)$compress <- compress.default
  constructor
}

LogicalList <- AtomicListConstructor("logical")
IntegerList <- AtomicListConstructor("integer")
NumericList <- AtomicListConstructor("numeric")
ComplexList <- AtomicListConstructor("complex")
CharacterList <- AtomicListConstructor("character")
RawList <- AtomicListConstructor("raw")
RleList <- AtomicListConstructor("Rle")
FactorList <- AtomicListConstructor("factor")


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion
###

setMethod("as.list", "CompressedAtomicList",
          function(x, use.names = TRUE) {
              if (is(x, "CompressedRleList")) {
                  callNextMethod(x, use.names = use.names)
              } else {
                  codes <- seq_len(length(x))
                  ans <-
                    split(x@unlistData,
                          structure(rep.int(codes, elementNROWS(x)),
                                    levels = as.character(codes),
                                    class = "factor"))
                  if (use.names) {
                      names(ans) <- names(x)
                  } else {
                      names(ans) <- NULL
                  }
                  ans
              }
          })

setAs("CompressedAtomicList", "list", function(from) as.list(from))

### Equivalent to 'as.vector(as.list(x), mode=mode)' but faster on
### CompressedAtomicList objects (10x, 75x, or more, depending on 'length(x)').
setMethod("as.vector", "AtomicList",
    function(x, mode="any")
    {
        valid_modes <- c("any", .ATOMIC_TYPES, "double", "list")
        mode <- match.arg(mode, valid_modes)
        if (mode %in% c("any", "list"))
            return(as.list(x))
        x_eltNROWS <- elementNROWS(x)
        if (any(x_eltNROWS > 1L))
            stop("coercing an AtomicList object to an atomic vector ",
                 "is supported only for\n",
                 "  objects with top-level elements of length <= 1")
        ans <- base::rep.int(as.vector(NA, mode=mode), length(x))
        ans[x_eltNROWS == 1L] <- as.vector(unlist(x, use.names=FALSE),
                                           mode=mode)
        ans
    }
)

as.matrix.AtomicList <- function(x, col.names=NULL, ...) {
    p <- PartitioningByEnd(x)
    vx <- decode(unlist(x, use.names=FALSE))
    if (is.null(col.names)) {
        col.names <- names(vx)
    }
    if (is.null(col.names) || is.character(col.names)) {
        col.ind <- as.integer(IRanges(1, width(p)))
    } else if (is.list(col.names) || is(col.names, "List")) {
        col.names <- unlist(col.names, use.names=FALSE)
        if (is.factor(col.names)) {
            col.ind <- as.integer(col.names)
            col.names <- levels(col.names)
        } else {
            col.ind <- selfmatch(col.names)
            col.names <- col.names[col.ind == seq_along(col.ind)]
        }
    } else {
        stop("'col.names' should be NULL, a character vector or list")
    }
    row.ind <- togroup(p)
    nc <- if (!is.null(col.names)) length(col.names) else max(width(p))
    m <- matrix(nrow=length(x), ncol=nc)
    m[cbind(row.ind, col.ind)] <- vx
    if (!is.null(col.names))
        colnames(m) <- col.names
    m
}
setMethod("as.matrix", "AtomicList", function(x, col.names=NULL)
    as.matrix.AtomicList(x, col.names))

setMethod("drop", "AtomicList", function(x) {
  x_eltNROWS <- elementNROWS(x)
  if (any(x_eltNROWS > 1))
    stop("All element lengths must be <= 1")
  x_dropped <- rep.int(NA, sum(x_eltNROWS))
  x_dropped[x_eltNROWS > 0] <- unlist(x, use.names = FALSE)
  names(x_dropped) <- names(x)
  x_dropped
})

CoercerToList <- function(type, compress) {
  .coerceToList <- if (compress)
                     coerceToCompressedList
                   else
                     S4Vectors:::coerceToSimpleList
  function(from) {
    .coerceToList(from, type)
  }
}

setListCoercions <- function(type) {
  CompressedClass <- S4Vectors:::listClassName("Compressed", type)
  SimpleClass <- S4Vectors:::listClassName("Simple", type)
  Class <- S4Vectors:::listClassName("", type)
  hasCompressedList <- CompressedClass != "CompressedList"
  if (hasCompressedList) {
    setAs("ANY", CompressedClass, CoercerToList(type, compress = TRUE))
  }
  setAs("ANY", SimpleClass, CoercerToList(type, compress = FALSE))
  setAs("ANY", Class, CoercerToList(type, compress = hasCompressedList))
  setAs("SimpleList", Class, CoercerToList(type, compress = FALSE))
  setAs("list", Class, CoercerToList(type, compress = FALSE))
}

setListCoercions("logical")
setListCoercions("integer")
setListCoercions("numeric")
setListCoercions("complex")
setListCoercions("character")
setListCoercions("raw")
setListCoercions("Rle")
setListCoercions("factor")


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### General methods
###

### Could actually be made the "table" method for List objects. Will work on
### any List object 'x' for which 'as.factor(unlist(x))' works.
setMethod("table", "AtomicList",
    function(...)
    {
        args <- list(...)
        if (length(args) != 1L)
            stop("\"table\" method for AtomicList objects ",
                 "can only take one input object")
        x <- args[[1L]]
        if (!pcompareRecursively(x)) {
            ## Not sure why callNextMethod() doesn't work. Is it because of
            ## dispatch on the ellipsis?
            #return(callNextMethod())
            return(selectMethod("table", "Vector")(...))
        }
        y1 <- togroup(PartitioningByWidth(x))
        attributes(y1) <- list(levels=as.character(seq_along(x)),
                               class="factor")
        y2 <- as.factor(unlist(x, use.names=FALSE))
        ans <- table(y1, y2)
        names(dimnames(ans)) <- NULL
        x_names <- names(x)
        if (!is.null(x_names))
            rownames(ans) <- x_names
        ans
    }
)

setMethod("table", "SimpleAtomicList", function(...)
{
    args <- list(...)
    if (length(args) != 1L)
        stop("\"table\" method for SimpleAtomicList objects ",
             "can only take one input object")
    x <- args[[1L]]
    levs <- sort(unique(unlist(lapply(x, function(xi) {
        if (!is.null(levels(xi))) levels(xi) else unique(xi)
    }), use.names=FALSE)))
    as.table(do.call(rbind,
                     lapply(x, function(xi) {
                         if (is(xi, "Rle"))
                             runValue(xi) <- factor(runValue(xi), levs)
                         else xi <- factor(xi, levs)
                         table(xi)
                     })))
})

setCompressedNumericalListMethod <-
    function(fun, def, where=topenv(parent.frame()))
{
    types <- c("Logical", "Integer", "Numeric")
    classNames <- paste0("Compressed", types, "List")
    lapply(classNames, function(className) {
               C_fun <- paste0(className, "_", sub(".", "_", fun, fixed=TRUE))
               body(def) <- eval(call("substitute", body(def)))
               setMethod(fun, className, def, where=where)
           })
}

setCompressedNumericalListMethod("is.unsorted",
                                 function(x, na.rm = FALSE, strictly=FALSE) {
                                     stopifnot(isTRUEorFALSE(na.rm))
                                     stopifnot(isTRUEorFALSE(strictly))
                                     .Call(C_fun, x, na.rm, strictly)
                                 })


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Rle methods
###

### 'use.names' is ignored.
setMethod("unlist", "SimpleRleList",
    function (x, recursive=TRUE, use.names=TRUE)
    {
        if (!identical(recursive, TRUE))
            stop("\"unlist\" method for SimpleRleList objects ",
                 "does not support the 'recursive' argument")
        ans_values <- unlist(lapply(x@listData, slot, "values"),
                             use.names=FALSE)
        ans_lengths <- unlist(lapply(x@listData, slot, "lengths"),
                              use.names=FALSE)
        Rle(ans_values, ans_lengths)
    }
)

setMethod("runValue", "RleList", function(x) {
  as(lapply(x, runValue), "List")
})

setMethod("runValue", "CompressedRleList",
    function(x)
    {
        rle <- unlist(x, use.names=FALSE)
        rlePart <- PartitioningByWidth(runLength(rle))
        listPart <- PartitioningByEnd(x)
        ## 'rlePart' cannot contain empty ranges so using
        ## Using 'hit.empty.query.ranges=TRUE' won't affect the result
        ## (because 'rlePart' cannot contain empty ranges) but it makes
        ## findOverlaps_Ranges_Partitioning() just a little bit faster.
        hits <- findOverlaps_Ranges_Partitioning(rlePart, listPart,
                                                 hit.empty.query.ranges=TRUE)
        ans_partitioning <- PartitioningByEnd(subjectHits(hits), NG=length(x))
        ans_unlistData <- runValue(rle)[queryHits(hits)]
        ans <- relist(ans_unlistData, ans_partitioning)
        names(ans) <- names(x)
        ans
    }
)

setReplaceMethod("runValue", "CompressedRleList",
                 function(x, value) {
                   if (!identical(elementNROWS(ranges(x)),
                                  elementNROWS(value)))
                     stop("elementNROWS() of 'x' and 'value' must match")
                   runValue(x@unlistData) <- unlist(value, use.names=FALSE)
                   x
                 })

setReplaceMethod("runValue", "SimpleRleList",
                 function(x, value) {
                   if (!identical(elementNROWS(ranges(x)),
                                  elementNROWS(value)))
                     stop("elementNROWS() of 'x' and 'value' must match")
                   x@listData <- mapply(function(rle, v) {
                     runValue(rle) <- v
                     rle
                   }, x, value, SIMPLIFY=FALSE)
                   x
                 })

setMethod("runLength", "RleList", function(x) {
  as(lapply(x, runLength), "IntegerList")
})

setMethod("runLength", "CompressedRleList", function(x) {
  width(ranges(x))
})

setMethod("ranges", "RleList", function(x) {
  as(lapply(x, ranges), "List")
})

diceRangesByList <- function(x, list) {
  listPart <- PartitioningByEnd(list)
  ## 'x' cannot contain empty ranges so using
  ## 'hit.empty.query.ranges=TRUE' won't affect the result but
  ## it makes findOverlaps_Ranges_Partitioning() just a little
  ## bit faster.
  hits <- findOverlaps_Ranges_Partitioning(x, listPart,
                                           hit.empty.query.ranges=TRUE)
  ans_partitioning <- PartitioningByEnd(subjectHits(hits), NG=length(list))
  ans_unlistData <- shift(ranges(hits, x, listPart),
                          1L - start(listPart)[subjectHits(hits)])
  ans <- relist(ans_unlistData, ans_partitioning)
  names(ans) <- names(list)
  ans
}

setMethod("ranges", "CompressedRleList",
    function(x)
    {
      rle <- unlist(x, use.names=FALSE)
      rlePart <- PartitioningByWidth(runLength(rle))
      diceRangesByList(rlePart, x)
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Factor methods
###

setMethod("levels", "FactorList", function(x) {
  CharacterList(lapply(x, levels))
})

setMethod("levels", "CompressedFactorList", function(x) {
  setNames(rep(CharacterList(levels(x@unlistData)), length(x)), names(x))
})

setMethod("unlist", "SimpleFactorList",
          function(x, recursive = TRUE, use.names = TRUE) {
            levs <- levels(x)
            if (length(x) > 1L &&
                !all(vapply(levs[-1L], identical, logical(1L), levs[[1L]]))) {
              stop("inconsistent level sets")
            }
            structure(callNextMethod(),
                      levels=as.character(levs[[1L]]),
                      class="factor")
          })


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "show" method.
###

.showAtomicList <- function(object, minLines, ...)
{
    len <- length(object)
    object_names <- names(object)
    k <- min(minLines, len)
    d <- len - minLines
    for (i in seq_len(k)) {
        if (is.null(object_names)) {
            label <- i
        } else {
            nm <- object_names[[i]]
            if (is.na(nm)) {
                label <- "NA"
            } else {
                label <- paste0("\"", nm, "\"")
            }
        }
        label <- paste0("[[", label, "]]")
        if (length(object[[i]]) == 0) {
            cat(label, " ", sep = "")
            print(object[[i]])
        } else {
            cat(S4Vectors:::labeledLine(label, object[[i]], labelSep = "",
                                        count = FALSE))
        }
    }
    if (d > 0)
        cat("...\n<", d,
            ifelse(d == 1,
                   " more element>\n", " more elements>\n"), sep="")
}

setMethod("show", "AtomicList",
          function(object) 
          {
              cat(classNameForDisplay(object), " of length ",
                  length(object), "\n", sep = "")
              .showAtomicList(object, 10) 
          }
)

setMethod("show", "RleList",
          function(object) {
              lo <- length(object)
              k <- min(5, length(object))
              diffK <- lo - 5
              cat(classNameForDisplay(object), " of length ", lo,
                  "\n", sep = "")
              show(as.list(head(object, k)))
              if (diffK > 0)
                  cat("...\n<", diffK,
                      ifelse(diffK == 1,
                             " more element>\n", " more elements>\n"),
                      sep="")
          })
