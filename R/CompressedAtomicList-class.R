### =========================================================================
### CompressedAtomicList objects
### -------------------------------------------------------------------------


## Possible optimizations for compressed lists:
## - order/sort: unlist, order by split factor first
## - cumsum: unlist, cumsum and subtract offsets

setClass("CompressedAtomicList",
         contains =  c("AtomicList", "CompressedList"),
         representation("VIRTUAL"))

setClass("CompressedLogicalList",
         prototype = prototype(elementType = "logical",
                               unlistData = logical()),
         contains = c("LogicalList", "CompressedAtomicList"))

setClass("CompressedIntegerList",
         prototype = prototype(elementType = "integer",
                               unlistData = integer()),
         contains = c("IntegerList", "CompressedAtomicList"))

setClass("CompressedNumericList",
         prototype = prototype(elementType = "numeric",
                               unlistData = numeric()),
         contains = c("NumericList", "CompressedAtomicList"))

setClass("CompressedComplexList",
         prototype = prototype(elementType = "complex",
                               unlistData = complex()),
         contains = c("ComplexList", "CompressedAtomicList"))

setClass("CompressedCharacterList",
         prototype = prototype(elementType = "character",
                               unlistData = character()),
         contains = c("CharacterList", "CompressedAtomicList"))

setClass("CompressedRawList",
         prototype = prototype(elementType = "raw",
                               unlistData = raw()),
         contains = c("RawList", "CompressedAtomicList"))

setClass("CompressedRleList",
         prototype = prototype(elementType = "Rle",
                               unlistData = new("Rle")),
         contains = c("RleList", "CompressedAtomicList"))

setClass("CompressedFactorList",
         prototype = prototype(elementType = "factor",
           unlistData = factor()),
         contains = c("FactorList", "CompressedAtomicList"))


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion
###

setListCoercions("logical")
setListCoercions("integer")
setListCoercions("numeric")
setListCoercions("complex")
setListCoercions("character")
setListCoercions("raw")
setListCoercions("Rle")
setListCoercions("factor")

setMethod("as.list", "CompressedAtomicList",
          function(x, use.names = TRUE) {
              if (is(x, "CompressedRleList")) {
                  callNextMethod(x, use.names = use.names)
              } else {
                  f <- S4Vectors:::map_inner_ROWS_to_list_elements(
                                       elementNROWS(x),
                                       as.factor=TRUE)
                  ans <- split(x@unlistData, f)
                  if (use.names) {
                      names(ans) <- names(x)
                  } else {
                      names(ans) <- NULL
                  }
                  ans
              }
          })

setAs("CompressedAtomicList", "list", function(from) as.list(from))

.from_IPosRanges_to_CompressedIntegerList <- function(from)
{
    ans <- relist(unlist_as_integer(from), from)
    metadata(ans) <- metadata(from)
    mcols(ans) <- mcols(from, use.names=FALSE)
    ans
}

### Propagate the names, metadata, and metadata columns.
setAs("IPosRanges", "CompressedIntegerList",
    .from_IPosRanges_to_CompressedIntegerList
)
setAs("IPosRanges", "IntegerList",
    .from_IPosRanges_to_CompressedIntegerList
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### General methods
###

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

setMethod("runValue", "CompressedRleList",
    function(x)
    {
        rle <- unlist(x, use.names=FALSE)
        rlePart <- PartitioningByWidth(runLength(rle))
        listPart <- PartitioningByEnd(x)
        ## 'rlePart' cannot contain empty ranges so using
        ## Using 'hit.empty.query.ranges=TRUE' won't affect the result
        ## (because 'rlePart' cannot contain empty ranges) but it makes
        ## findOverlaps_IntegerRanges_Partitioning() just a little bit faster.
        hits <- findOverlaps_IntegerRanges_Partitioning(
                    rlePart, listPart,
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

setMethod("runLength", "CompressedRleList", function(x) {
  width(ranges(x))
})

setMethod("ranges", "CompressedRleList",
    function(x, use.names=TRUE, use.mcols=FALSE)
    {
      rle <- unlist(x, use.names=FALSE)
      rlePart <- PartitioningByWidth(runLength(rle))
      diceRangesByList(rlePart, x)
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Factor methods
###

setMethod("levels", "CompressedFactorList", function(x) {
  setNames(rep(CharacterList(levels(x@unlistData)), length(x)), names(x))
})


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Iteration
###

setMethod("lapply", "CompressedAtomicList",
          function(X, FUN, ...)
          {
            FUN <- match.fun(FUN)
            if (is(X, "CompressedRleList")) {
              callNextMethod(X, FUN, ...)
            } else {
              lapply(as.list(X), FUN, ...)
            }
          })

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Group generic methods
###

doBinaryCompressedListOp <- function(OP, e1, e2, skeleton) {
  if (!missing(skeleton)) {
    n <- length(skeleton)
  } else {
    n <- max(length(e1), length(e2))
  }
  e1 <- recycleList(e1, n)
  e2 <- recycleList(e2, n)
  if (missing(skeleton)) {
    n1 <- elementNROWS(e1)
    n2 <- elementNROWS(e2)
    if (any(n1 != n2)) {
      en <- ifelse(n1 == 0L | n2 == 0L, 0L, pmax.int(n1, n2))
    } else {
      en <- NULL
    }
    nms <- names(e1)
    if (is.null(nms))
      nms <- names(e2)
  } else {
    en <- elementNROWS(skeleton)
    nms <- names(skeleton)
  }
  if (!is.null(en)) {
    e1 <- recycleListElements(e1, en)
    e2 <- recycleListElements(e2, en)
  }
  partitioning <- PartitioningByEnd(e1)
  names(partitioning) <- nms
  relist(OP(unlist(e1, use.names=FALSE), unlist(e2, use.names=FALSE)),
         partitioning)
}

setMethod("Ops",
          signature(e1 = "CompressedAtomicList", e2 = "CompressedAtomicList"),
          function(e1, e2)
          {
              if (length(e1) == 0L || length(e2) == 0L) {
                return(emptyOpsReturnValue(.Generic, e1, e2, compress = TRUE))
              }
              doBinaryCompressedListOp(function(x, y) {
                .Generic <- .Generic
                callGeneric(x, y)
              }, e1, e2)
          })

setMethod("Ops",
          signature(e1 = "SimpleAtomicList", e2 = "CompressedAtomicList"),
          function(e1, e2)
          {
              if (sum(as.numeric(elementNROWS(e1))) < .Machine$integer.max)
                  e1 <- as(e1, "CompressedList")
              else
                  e2 <- as(e2, "SimpleList")
              callGeneric(e1, e2)
          })

setMethod("Ops",
          signature(e1 = "CompressedAtomicList", e2 = "SimpleAtomicList"),
          function(e1, e2)
          {
              if (sum(as.numeric(elementNROWS(e2))) < .Machine$integer.max)
                  e2 <- as(e2, "CompressedList")
              else
                  e1 <- as(e1, "SimpleList")
              callGeneric(e1, e2)
          })

setMethod("Ops",
          signature(e1 = "CompressedAtomicList", e2 = "atomic"),
          function(e1, e2)
          {
              if (length(e2) > 1) {
                  e2 <- S4Vectors:::recycleVector(e2, length(e1))
                  e2 <- rep(e2, elementNROWS(e1))
              }
              relist(callGeneric(e1@unlistData, e2), e1)
          })

setMethod("Ops",
          signature(e1 = "atomic", e2 = "CompressedAtomicList"),
          function(e1, e2)
          {
              if (length(e1) > 1) {
                  e1 <- S4Vectors:::recycleVector(e1, length(e2))
                  e1 <- rep(e1, elementNROWS(e2))
              }
              relist(callGeneric(e1, e2@unlistData), e2)
          })

setMethod("Math", "CompressedAtomicList",
          function(x) {
              relist(callGeneric(x@unlistData), x)
          })

setMethod("cumsum", "CompressedAtomicList",
          function(x) {
              xunlist <- unlist(x, use.names=FALSE)
              xcumsum <- cumsum(as.numeric(xunlist))
              partition <- PartitioningByEnd(x)
              ans <- xcumsum - rep(xcumsum[start(partition)] -
                  xunlist[start(partition)], width(partition))
              relist(ans, x)
          })

setMethod("cumprod", "CompressedAtomicList",
          function(x) {
              as(lapply(x, .Generic), "CompressedList")
          })

setMethod("cummin", "CompressedAtomicList",
          function(x) {
              as(lapply(x, .Generic), "CompressedList")
          })

setMethod("cummax", "CompressedAtomicList",
          function(x) {
              as(lapply(x, .Generic), "CompressedList")
          })

setMethod("Math2", "CompressedAtomicList",
          function(x, digits)
          {
              if (missing(digits))
                  digits <- ifelse(.Generic == "round", 0, 6)
              relist(callGeneric(x@unlistData, digits = digits), x)
          })

setMethod("any", "CompressedAtomicList", function(x, na.rm = FALSE) {
              stopifnot(isTRUEorFALSE(na.rm))
              ans <- sum(x, na.rm=TRUE) > 0L
              if (!na.rm) {
                  ans[!ans & any(is.na(x), na.rm=TRUE)] <- NA
              }
              ans
          })

setMethod("all", "CompressedAtomicList", function(x, na.rm = FALSE) {
              stopifnot(isTRUEorFALSE(na.rm))
              ans <- !any(!x, na.rm=TRUE)
              if (!na.rm) {
                  ans[ans & any(is.na(x), na.rm=TRUE)] <- NA
              }
              ans
          })

setMethod("anyNA", "CompressedAtomicList", function(x, recursive=FALSE) {
    callNextMethod(x, recursive=FALSE) ## recursion will just slow us down
})

rowsumCompressedList <- function(x, ..., na.rm = FALSE) {
  x_flat <- unlist(x, use.names = FALSE)
  ans <- vector(class(x_flat), length(x))
  non_empty <- elementNROWS(x) > 0
  if (is.logical(x_flat))
    x_flat <- as.integer(x_flat)
  ans[non_empty] <- rowsum(x_flat, togroup(PartitioningByWidth(x)),
                           reorder = FALSE,
                           na.rm = na.rm)[,1]
  setNames(ans, names(x))
}

setCompressedListSummaryMethod <- function(fun, where=topenv(parent.frame()))
{
    setCompressedNumericalListMethod(fun, function(x, na.rm = FALSE) {
        stopifnot(isTRUEorFALSE(na.rm))
        .Call(C_fun, x, na.rm, PACKAGE="IRanges")
    }, where)
}

setCompressedListSummaryMethod("sum")
setCompressedListSummaryMethod("prod")
setCompressedListSummaryMethod("min")
setCompressedListSummaryMethod("max")

setMethods("range",
           list("CompressedLogicalList",
                "CompressedIntegerList",
                "CompressedNumericList",
                "CompressedRleList"),
           function(x, na.rm=FALSE) {
               stopifnot(isTRUEorFALSE(na.rm))
               cbind(min(x, na.rm=na.rm), max(x, na.rm=na.rm))
           })

setMethod("Summary", "CompressedRleList",
          function(x, ..., na.rm = FALSE) {
            toViewFun <- list(max = viewMaxs, min = viewMins, sum = viewSums)
            if (!is.null(viewFun <- toViewFun[[.Generic]])) {
              ans <- viewFun(as(x, "RleViews"), na.rm = na.rm)
              names(ans) <- names(x)
              ans
            } else if (.Generic %in% c("any", "all"))
                callNextMethod()
            else sapply(x, .Generic, na.rm = na.rm)
          })

setMethod("all", "CompressedRleList", function(x, ..., na.rm = FALSE) {
  args <- list(...)
  if (length(args) > 0L)
    stop("Only a single argument in '...' is supported for now")
  if (!isTRUEorFALSE(na.rm))
    stop("'na.rm' must be TRUE or FALSE")
  rv <- runValue(x)
  if (na.rm)
    rv <- rv[!is.na(rv)]
  rv_eltNROWS <- elementNROWS(rv)
  ans <- rv_eltNROWS == 0L
  singletons <- rv_eltNROWS == 1L
  ans[singletons] <- unlist(rv, use.names = FALSE)[singletons[togroup(PartitioningByWidth(rv))]]
  ans
})

setMethod("Complex", "CompressedAtomicList",
          function(z)
              relist(callGeneric(z@unlistData), z))

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### More list-ized methods
###

.setListMethod <- function(f,
                           inputClass,
                           outputBaseClass,
                           whichArg = 1L,
                           remainingSignature = character(),
                           mapply = FALSE,
                           endoapply = FALSE,
                           applyToUnlist = FALSE,
                           where = topenv(parent.frame()))
{
    fargs <- formals(args(get(f)))
    args <- sapply(names(fargs), as.name)
    names(args) <- sub("...", "", names(args), fixed = TRUE)
    if (applyToUnlist) {
        call2 <- as.call(c(as.name("@"), args[[whichArg]], "partitioning"))
        args[[whichArg]] <-
          as.call(c(as.name("@"), args[[whichArg]], "unlistData"))
        call1 <- as.call(c(as.name(f), args))
        call <-
          as.call(c(as.name("new2"), paste0("Compressed", outputBaseClass),
                    unlistData = call1, partitioning = call2, check = FALSE))
    } else {
        args <- c(args[[whichArg]], as.name(f), args[-whichArg])
        if (endoapply) {
            call <- as.call(c(as.name("endoapply"), args))
        } else if (missing(outputBaseClass)) {
            call <- as.call(c(as.name("sapply"), args, list(simplify = TRUE)))
        } else {
            if (mapply) {
                if (length(args) <= 3) {
                    call <-
                      as.call(c(as.name("mapply"), args[c(2:1,3L)],
                                SIMPLIFY = FALSE))
                } else {
                    call <-
                      as.call(c(as.name("mapply"),
                                args[c(2:1,3L)],
                                MoreArgs =
                                as.call(c(as.name("list"), tail(args, -3))),
                                SIMPLIFY = FALSE))
                }
            } else {
                call <- as.call(c(as.name("lapply"), args))
            }
            if (extends(inputClass, "SimpleList")) {
                call <-
                  as.call(c(as.name("new2"),
                            paste0("Simple", outputBaseClass),
                            listData = call, check = FALSE))
            } else {
                call <-
                  as.call(c(as.name(outputBaseClass), call, compress = TRUE))
            }
        }
    }
    def <- as.function(c(fargs, call))
    environment(def) <- parent.frame()
    setMethod(f, c(rep("ANY", whichArg - 1L), inputClass, remainingSignature),
              def, where)
}

.setAtomicListMethod <- function(f,
                                 inputBaseClass = "AtomicList",
                                 outputBaseClass,
                                 whichArg = 1L,
                                 remainingSignature = character(),
                                 mapply = FALSE,
                                 endoapply = FALSE,
                                 applyToUnlist = FALSE,
                                 addRleList = TRUE,
                                 rleListOutputBaseClass = "RleList",
                                 where = topenv(parent.frame()))
{
    if (missing(outputBaseClass)) {
        for (i in inputBaseClass)
            .setListMethod(f, i, whichArg = whichArg,
                           remainingSignature = remainingSignature,
                           endoapply = endoapply, where = where)
    } else if (endoapply) {
        .setListMethod(f, "AtomicList", whichArg = whichArg,
                       remainingSignature = remainingSignature,
                       endoapply = TRUE, where = where)
    } else {
        .setListMethod(f, paste0("Simple", inputBaseClass),
                       outputBaseClass = outputBaseClass, whichArg = whichArg,
                       remainingSignature = remainingSignature, mapply = mapply,
                       where = where)
        .setListMethod(f, paste0("Compressed", inputBaseClass),
                       outputBaseClass = outputBaseClass, whichArg = whichArg,
                       remainingSignature, mapply = mapply,
                       applyToUnlist = applyToUnlist, where = where)
        if (addRleList) {
            .setListMethod(f, "SimpleRleList",
                           outputBaseClass = rleListOutputBaseClass,
                           whichArg = whichArg,
                           remainingSignature = remainingSignature,
                           mapply = mapply, where = where)
            .setListMethod(f, "CompressedRleList",
                           outputBaseClass = rleListOutputBaseClass,
                           whichArg = whichArg,
                           remainingSignature = remainingSignature,
                           mapply = mapply, applyToUnlist = applyToUnlist,
                           where = where)
        }
    }
}

 
### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Logical methods
###

.setAtomicListMethod("which", inputBaseClass = "LogicalList",
                     outputBaseClass = "IntegerList",
                     rleListOutputBaseClass = "IntegerList")

setMethod("which", "CompressedLogicalList", function(x) {
  x.flat <- unlist(x, use.names = FALSE)
  part <- PartitioningByEnd(x)
  which.global <- which(x.flat)
  group <- findInterval(which.global, start(part))
  which.local <- which.global - start(part)[group] + 1L
  ans <- splitAsList(which.local, factor(group, seq_len(length(x))))
  names(ans) <- names(x)
  ans
})

setMethods("ifelse2", list(c("CompressedLogicalList", "ANY", "ANY"),
                           c("CompressedLogicalList", "ANY", "List"),
                           c("CompressedLogicalList", "List", "ANY"),
                           c("CompressedLogicalList", "List", "List")),
           function(test, yes, no) {
             doBinaryCompressedListOp(function(yes, no) {
               ifelse(unlist(test, use.names=FALSE), yes, no)
             }, as(yes, "List"), as(no, "List"), test)
           })

setMethods("ifelse2", list(c("SimpleLogicalList", "ANY", "ANY"),
                           c("SimpleLogicalList", "ANY", "List"),
                           c("SimpleLogicalList", "List", "ANY"),
                           c("SimpleLogicalList", "List", "List")),
           function(test, yes, no) {
             as(mapply(ifelse, test, yes, no, SIMPLIFY=FALSE), "List")
           })


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Numerical methods
###

setCompressedListWhichSummaryMethod <-
    function(fun, where=topenv(parent.frame()))
    {
        def <- function(x, global = FALSE) {
            stopifnot(isTRUEorFALSE(global))
            ans <- .Call(C_fun, x)
            if (global) {
                ans <- toglobal(ans, x)
            }
            ans
        }
        setCompressedNumericalListMethod(fun, def, where)
    }
setCompressedListWhichSummaryMethod("which.min")
setCompressedListWhichSummaryMethod("which.max")

setMethod("which.min", "CompressedRleList",
          function(x) {
            viewWhichMins(as(x, "RleViews"), na.rm=TRUE) -
              c(0L, head(cumsum(elementNROWS(x)), -1))
          })
setMethod("which.max", "CompressedRleList",
          function(x) {
            viewWhichMaxs(as(x, "RleViews"), na.rm=TRUE) -
              c(0L, head(cumsum(elementNROWS(x)), -1))
          })

for (i in c("IntegerList", "NumericList", "RleList")) {
    .setAtomicListMethod("diff", inputBaseClass = i, endoapply = TRUE)
}

setMethods("mean",
           list("CompressedLogicalList",
                "CompressedIntegerList",
                "CompressedNumericList",
                "CompressedRleList"),
           function(x, trim = 0, na.rm = FALSE) {
               stopifnot(isTRUEorFALSE(na.rm))
               stopifnot(isSingleNumber(trim))
               if (trim > 0) {
                   return(callNextMethod())
               }
               x_eltNROWS <- if (na.rm) sum(!is.na(x)) else elementNROWS(x)
               sum(x, na.rm=na.rm) / x_eltNROWS
           })

setMethod("median", "CompressedAtomicList", function(x, na.rm=FALSE) {
    stopifnot(isTRUEorFALSE(na.rm))
    sx <- sort(x)
    n <- lengths(sx)
    half <- (n + 1L)%/%2L
    even <- n%%2L != 1L
    ind <- IRanges(half, width=1L+even)
    NAs <- half == 0L
    ind <- relist(ind[!NAs], PartitioningByWidth(as.integer(!NAs)))
    ## ind <- as(half, "IntegerList")
    ## ind[even] <- ind[even] + as(0:1, "IntegerList")
    ans <- mean(sx[ind])
    if (!na.rm) {
        NAs <- NAs | anyNA(x)
    }
    if (any(NAs)) {
        ans[NAs] <- as(NA, elementType(x))
    }
    ans
})

setMethod("diff", "CompressedAtomicList",
           function(x, lag = 1L, differences = 1L) {
               stopifnot(isSingleNumber(lag))
               stopifnot(isSingleNumber(differences))
               r <- x
               for (i in seq_len(differences))
                   r <- tails(r, -lag) - heads(r, -lag)
               r
           })


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Running window statistic methods
###

.setAtomicListMethod("smoothEnds", inputBaseClass = "IntegerList",
                     outputBaseClass = "NumericList",
                     addRleList = FALSE)

.setAtomicListMethod("smoothEnds", inputBaseClass = "NumericList",
                     endoapply = TRUE)

.setAtomicListMethod("smoothEnds", inputBaseClass = "RleList",
                     endoapply = TRUE)

setMethod("runmed", "CompressedIntegerList",
          function(x, k, endrule = c("median", "keep", "constant"),
                   algorithm = NULL, print.level = 0)
              NumericList(lapply(x, runmed, k = k, endrule = match.arg(endrule),
                                 algorithm = algorithm,
                                 print.level = print.level)))

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Character
###

nchar_CompressedList <- function(x, type="chars", allowNA=FALSE)
{
        unlisted_x <- unlist(x, use.names=FALSE)
        unlisted_ans <- nchar(unlisted_x, type=type, allowNA=allowNA)
        relist(unlisted_ans, x)
}

### H.P. (Feb 5, 2018): Does not seem right to output a CompressedList object
### when the input is SimpleList!
setMethod("nchar", "CompressedCharacterList", nchar_CompressedList)
setMethod("nchar", "SimpleCharacterList", nchar_CompressedList)  # not good!

setMethod("nchar", "CompressedRleList", nchar_CompressedList)
setMethod("nchar", "SimpleRleList", nchar_CompressedList)  # not good!


setMethod("paste", "CompressedAtomicList",
          function(..., sep=" ", collapse=NULL) {
              args <- lapply(list(...), as, "CharacterList")
              x_eltNROWS <- do.call(pmax, lapply(args, elementNROWS))
              args <- lapply(args, recycleListElements, x_eltNROWS)
              unlisted <- lapply(args, unlist, use.names=FALSE)
              ans <- relist(do.call(paste, c(unlisted, sep=sep)),
                            PartitioningByWidth(x_eltNROWS))
              if (!is.null(collapse)) {
                  ans <- unstrsplit(ans, collapse)
              }
              ans
          })

## need vectorized start, end
##.setAtomicListMethod("substr")
##.setAtomicListMethod("substring")
.setAtomicListMethod("chartr", inputBaseClass = "CharacterList",
                     outputBaseClass = "CharacterList", whichArg = 3L,
                     applyToUnlist = TRUE)

.setAtomicListMethod("tolower", inputBaseClass = "CharacterList",
                     outputBaseClass = "CharacterList",  applyToUnlist = TRUE)

.setAtomicListMethod("toupper", inputBaseClass = "CharacterList",
                     outputBaseClass = "CharacterList", applyToUnlist = TRUE)

.setAtomicListMethod("sub", inputBaseClass = "CharacterList",
                     outputBaseClass = "CharacterList", whichArg = 3L,
                     applyToUnlist = TRUE)

.setAtomicListMethod("gsub", inputBaseClass = "CharacterList",
                     outputBaseClass = "CharacterList", whichArg = 3L,
                     applyToUnlist = TRUE)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Comparison / sorting
###

setMethod("selfmatch", "CompressedAtomicList", function(x, global=FALSE) {
    g <- subgrouping(x)
    first <- unlist(g)[start(PartitioningByEnd(g))]
    ux <- unlist(x, use.names=FALSE)
    ux[unlist(g)] <- rep(first, lengths(g))
    ans <- relist(ux, x)
    if (!global) {
        ans <- ans - start(ans) + 1L
    }
    ans
})

.duplicated.CompressedAtomicList <- function(x, incomparables=FALSE,
                                             fromLast=FALSE, nmax=NA, ...)
{
    if (!identical(incomparables, FALSE))
        stop("\"duplicated\" method for CompressedList objects ",
             "does not support the 'incomparables' argument")
    if (length(list(...)) > 0L) {
        stop("arguments in '...' are not supported")
    }
    stopifnot(isTRUEorFALSE(fromLast))
    g <- subgrouping(x)
    p <- PartitioningByEnd(g)
    first <- unlist(g)[if (fromLast) end(p) else start(p)]
    v <- rep(TRUE, length(unlist(g)))
    v[first] <- FALSE
    relist(v, x)
}
setMethod("duplicated", "CompressedAtomicList",
          .duplicated.CompressedAtomicList)

setMethod("rank", "CompressedAtomicList",
          function (x, na.last = TRUE,
                    ties.method = c("average", "first",
                                    "last", "random", "max", "min"))
          {
              stopifnot(isTRUE(na.last))
              ties.method <- match.arg(ties.method)
              if (ties.method == "last" || ties.method == "random")
                  stop("'ties.method' last/random not yet supported")
              p <- PartitioningByEnd(x)
              o <- order(togroup(p), unlist(x, use.names=FALSE))
              r <- unlist_as_integer(IRanges(1L, width=width(p)))
              gp <- PartitioningByEnd(end(Rle(unlist(x, use.names=FALSE)[o])))
              v <- switch(ties.method,
                          average=(r[start(gp)] + r[end(gp)])/2,
                          first=r,
                          ## last=,
                          ## random=,
                          max=r[end(gp)],
                          min=r[start(gp)])
              if (ties.method != "first")
                  v <- rep(v, width(gp))
              r[o] <- v
              relist(r, x)
          })

setMethod("order", "CompressedAtomicList",
          function (..., na.last = TRUE, decreasing = FALSE,
                    method = c("auto", "shell", "radix"))
{
    args <- list(...)
    if (length(args) != 1L)
        stop("\"order\" method for CompressedAtomicList objects ",
             "can only take one input object")
    x <- args[[1L]]
    p <- PartitioningByEnd(x)
    ux <- unlist(x, use.names=FALSE)
    o <- order(togroup(p), ux, na.last=na.last,
               decreasing=decreasing, method=method)
    skeleton <- if (is.na(na.last) && anyNA(ux)) {
        skeleton <- PartitioningByWidth(width(p) - sum(is.na(x)))
    } else p
    relist(o, skeleton) - start(p) + 1L
})


