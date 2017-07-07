### =========================================================================
### Common operations on AtomicList objects
### -------------------------------------------------------------------------


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Iteration
###

setMethod("lapply", "CompressedAtomicList",
          function(X, FUN, ...)
          {
            if (is(X, "CompressedRleList")) {
              callNextMethod(X, FUN, ...)
            } else {
              lapply(as.list(X), FUN, ...)
            }
          })


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Group generic methods
###

emptyOpsReturnValue <- function(.Generic, e1, e2, compress) {
  dummy.vector <- do.call(.Generic,
                          list(vector(e1@elementType), vector(e2@elementType)))
  CoercerToList(NULL, compress)(dummy.vector)
}

recycleList <- function(x, length.out) {
  if (length.out %% length(x) > 0L)
    warning("shorter object is not a multiple of longer object length")
  rep(x, length.out = length.out)
}

setMethod("Ops",
          signature(e1 = "SimpleAtomicList", e2 = "SimpleAtomicList"),
          function(e1, e2)
          {
              if (length(e1) == 0L || length(e2) == 0L) {
                return(emptyOpsReturnValue(.Generic, e1, e2, compress = FALSE))
              }
              n <- max(length(e1), length(e2))
              e1 <- recycleList(e1, n)
              e2 <- recycleList(e2, n)
              as(Map(.Generic, e1, e2), "List")
          })

repLengthOneElements <- function(x, times) {
  x@unlistData <- rep(x@unlistData, times)
  x@partitioning@end <- cumsum(times)
  x
}

recycleListElements <- function(x, newlen) {
  x_eltNROWS <- elementNROWS(x)
  if (identical(x_eltNROWS, newlen)) {
    return(x)
  }
  if (all(x_eltNROWS == 1L)) {
    ans <- repLengthOneElements(x, newlen)
  } else {
    ans <- rep(x, newlen / x_eltNROWS)
    if (length(unlist(ans, use.names=FALSE)) != sum(newlen)) {
      stop("Some element lengths are not multiples of their corresponding ",
           "element length in ", deparse(substitute(x)))
    }
  }
  ans
}

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
          signature(e1 = "AtomicList", e2 = "atomic"),
          function(e1, e2)
          {
              e2 <- as(e2, "List")
              callGeneric(e1, e2)
          })

setMethod("Ops",
          signature(e1 = "atomic", e2 = "AtomicList"),
          function(e1, e2)
          {
              e1 <- as(e1, "List")
              callGeneric(e1, e2)
          })

setMethod("Ops",
          signature(e1 = "SimpleAtomicList", e2 = "atomic"),
          function(e1, e2)
          {
              e2 <- as(e2, "SimpleList")
              callGeneric(e1, e2)
          })

setMethod("Ops",
          signature(e1 = "atomic", e2 = "SimpleAtomicList"),
          function(e1, e2)
          {
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

setMethod("Math", "SimpleAtomicList",
          function(x) as(lapply(x@listData, .Generic), "List"))

setMethod("Math2", "CompressedAtomicList",
          function(x, digits)
          {
              if (missing(digits))
                  digits <- ifelse(.Generic == "round", 0, 6)
              relist(callGeneric(x@unlistData, digits = digits), x)
          })

setMethod("Math2", "SimpleAtomicList",
          function(x, digits)
          {
              if (missing(digits))
                  digits <- ifelse(.Generic == "round", 0, 6)
              as(lapply(x@listData, .Generic, digits = digits), "List")
          })

setMethod("Summary", "AtomicList",
          function(x, ..., na.rm = FALSE) {
            sapply(x, .Generic, na.rm = na.rm)
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
                "CompressedNumericList"),
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
            }
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

setMethod("Complex", "SimpleAtomicList",
          function(z) as(lapply(z@listData, .Generic), "List"))

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### More list-ized methods
###

setListMethod <- function(f,
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
          as.call(c(as.name("new2"), paste("Compressed", outputBaseClass, sep=""),
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
                            paste("Simple", outputBaseClass, sep=""),
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

setAtomicListMethod <- function(f,
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
            setListMethod(f, i, whichArg = whichArg,
                          remainingSignature = remainingSignature,
                          endoapply = endoapply, where = where)
    } else if (endoapply) {
        setListMethod(f, "AtomicList", whichArg = whichArg,
                      remainingSignature = remainingSignature, endoapply = TRUE,
                      where = where)
    } else {
        setListMethod(f, paste("Simple", inputBaseClass, sep = ""),
                      outputBaseClass = outputBaseClass, whichArg = whichArg,
                      remainingSignature = remainingSignature, mapply = mapply,
                      where = where)
        setListMethod(f, paste("Compressed", inputBaseClass, sep = ""),
                      outputBaseClass = outputBaseClass, whichArg = whichArg,
                      remainingSignature, mapply = mapply,
                      applyToUnlist = applyToUnlist, where = where)
        if (addRleList) {
            setListMethod(f, "SimpleRleList",
                          outputBaseClass = rleListOutputBaseClass,
                          whichArg = whichArg,
                          remainingSignature = remainingSignature,
                          mapply = mapply, where = where)
            setListMethod(f, "CompressedRleList",
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

setAtomicListMethod("which", inputBaseClass = "LogicalList",
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

ifelseReturnValue <- function(yes, no, len) {
  proto <- function(x)
    new(if (is.atomic(x)) class(x) else x@elementType)
  v <- logical()
  v[1L] <- proto(yes)[1L]
  v[1L] <- proto(no)[1L]
  v
  compress <- is(yes, "CompressedList") || is(no, "CompressedList")
  as(rep(v, length.out = len),
    if(compress) "CompressedList" else "SimpleList")
}

setGeneric("ifelse2", function(test, yes, no) standardGeneric("ifelse2"))

setMethods("ifelse2", list(c("ANY", "ANY", "List"),
                           c("ANY", "List", "List"),
                           c("ANY", "List", "ANY")),      
           function(test, yes, no) {
             ans <- ifelseReturnValue(yes, no, length(test))
             ok <- !(nas <- is.na(test))
             if (any(test[ok])) 
               ans[test & ok] <- rep(yes, length.out = length(ans))[test & ok]
             if (any(!test[ok])) 
               ans[!test & ok] <- rep(no, length.out = length(ans))[!test & ok]
             ans[nas] <- NA
             names(ans) <- names(test)
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

### which.min() and which.max()
setMethods("which.min", list("IntegerList", "NumericList", "RleList"),
    function(x) setNames(as.integer(lapply(x, which.min)), names(x))
)
setMethods("which.max", list("IntegerList", "NumericList", "RleList"),
    function(x) setNames(as.integer(lapply(x, which.max)), names(x))
)

toglobal <- function(i, x) {
    start(PartitioningByEnd(x)) + i - 1L
}

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
    setAtomicListMethod("diff", inputBaseClass = i, endoapply = TRUE)
    setMethod("pmax", i, function(..., na.rm = FALSE)
                  mendoapply(pmax, ..., MoreArgs = list(na.rm = na.rm)))
    setMethod("pmin", i, function(..., na.rm = FALSE)
                  mendoapply(pmin, ..., MoreArgs = list(na.rm = na.rm)))
    setMethod("pmax.int", i, function(..., na.rm = FALSE)
                  mendoapply(pmax.int, ..., MoreArgs = list(na.rm = na.rm)))
    setMethod("pmin.int", i, function(..., na.rm = FALSE)
                  mendoapply(pmin.int, ..., MoreArgs = list(na.rm = na.rm)))
}

setMethod("mean", "AtomicList",
    function(x, ...) sapply(x, mean, ...)
)

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

setMethod("var", c("AtomicList", "missing"),
    function(x, y=NULL, na.rm=FALSE, use)
    {
        if (missing(use))
            use <- ifelse(na.rm, "na.or.complete", "everything")
        sapply(x, var, na.rm=na.rm, use=use)
    }
)
setMethod("var", c("AtomicList", "AtomicList"),
    function(x, y=NULL, na.rm=FALSE, use)
    {
        if (missing(use))
            use <- ifelse(na.rm, "na.or.complete", "everything")
        mapply(var, x, y, MoreArgs=list(na.rm=na.rm, use=use))
    }
)

setMethod("cov", c("AtomicList", "AtomicList"),
    function(x, y=NULL,
             use="everything", method=c("pearson", "kendall", "spearman"))
        mapply(cov, x, y, MoreArgs=list(use=use, method=match.arg(method)))
)

setMethod("cor", c("AtomicList", "AtomicList"),
    function(x, y=NULL,
             use="everything", method=c("pearson", "kendall", "spearman"))
        mapply(cor, x, y, MoreArgs=list(use=use, method=match.arg(method)))
)

setMethod("sd", "AtomicList",
    function(x, na.rm=FALSE) sapply(x, sd, na.rm=na.rm)
)

setMethod("median", "AtomicList",
    function(x, na.rm=FALSE) sapply(x, median, na.rm=na.rm)
)

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

setMethod("quantile", "AtomicList",
    function(x, ...) sapply(x, quantile, ...)
)

setMethod("mad", "AtomicList",
    function(x, center=median(x), constant=1.4826, na.rm=FALSE,
                low=FALSE, high=FALSE)
    {
        if (!missing(center))
            stop("'center' argument is not supported")
        sapply(x, mad, constant=constant, na.rm=na.rm, low=low, high=high)
    }
)

setMethod("IQR", "AtomicList",
    function(x, na.rm=FALSE, type=7) sapply(x, IQR, na.rm=na.rm, type=type)
)

diff.AtomicList <- function(x, ...) diff(x, ...)

setMethod("diff", "CompressedAtomicList",
           function(x, lag = 1L, differences = 1L) {
               stopifnot(isSingleNumber(lag))
               stopifnot(isSingleNumber(differences))
               r <- x
               for (i in seq_len(differences))
                   r <- ptail(r, -lag) - phead(r, -lag)
               r
           })

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Running window statistic methods
###

setAtomicListMethod("smoothEnds", inputBaseClass = "IntegerList",
                    outputBaseClass = "NumericList",
                    addRleList = FALSE)
setAtomicListMethod("smoothEnds", inputBaseClass = "NumericList",
                    endoapply = TRUE)
setAtomicListMethod("smoothEnds", inputBaseClass = "RleList",
                    endoapply = TRUE)
setMethod("runmed", "CompressedIntegerList",
          function(x, k, endrule = c("median", "keep", "constant"),
                   algorithm = NULL, print.level = 0)
              NumericList(lapply(x, runmed, k = k, endrule = match.arg(endrule),
                                 algorithm = algorithm,
                                 print.level = print.level)))
setMethod("runmed", "SimpleIntegerList",
          function(x, k, endrule = c("median", "keep", "constant"),
                   algorithm = NULL, print.level = 0)
              NumericList(lapply(x, runmed, k = k, endrule = match.arg(endrule),
                                 algorithm = algorithm,
                                 print.level = print.level), compress = FALSE))
setMethod("runmed", "NumericList",
          function(x, k, endrule = c("median", "keep", "constant"),
                   algorithm = NULL, print.level = 0)
              endoapply(x, runmed, k = k, endrule = match.arg(endrule),
                        algorithm = algorithm, print.level = print.level))
setMethod("runmed", "RleList",
          function(x, k, endrule = c("median", "keep", "constant"),
                   algorithm = NULL, print.level = 0)
              endoapply(x, runmed, k = k, endrule = match.arg(endrule)))
setMethod("runmean", "RleList",
          function(x, k, endrule = c("drop", "constant"), na.rm = FALSE)
              endoapply(x, runmean, k = k, endrule = match.arg(endrule),
                        na.rm = na.rm))
setMethod("runsum", "RleList",
          function(x, k, endrule = c("drop", "constant"), na.rm = FALSE)
              endoapply(x, runsum, k = k, endrule = match.arg(endrule),
                        na.rm = na.rm))
setMethod("runwtsum", "RleList",
          function(x, k, wt, endrule = c("drop", "constant"), na.rm = FALSE)
              endoapply(x, runwtsum, k = k, wt = wt,
                        endrule = match.arg(endrule), na.rm = na.rm))
setMethod("runq", "RleList",
          function(x, k, i, endrule = c("drop", "constant"), na.rm = FALSE)
              endoapply(x, runq, k = k, i = i, endrule = match.arg(endrule),
                        na.rm = na.rm))

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Character
###

nchar_CompressedList <- function(x, type="chars", allowNA=FALSE)
{
        unlisted_x <- unlist(x, use.names=FALSE)
        unlisted_ans <- nchar(unlisted_x, type=type, allowNA=allowNA)
        relist(unlisted_ans, x)
}

setMethod("nchar", "CompressedCharacterList", nchar_CompressedList)
setMethod("nchar", "SimpleCharacterList", nchar_CompressedList)

setMethod("nchar", "CompressedRleList", nchar_CompressedList)
setMethod("nchar", "SimpleRleList", nchar_CompressedList)


## need vectorized start, end
##setAtomicListMethod("substr")
##setAtomicListMethod("substring")
setAtomicListMethod("chartr", inputBaseClass = "CharacterList",
                    outputBaseClass = "CharacterList", whichArg = 3L,
                    applyToUnlist = TRUE)
setAtomicListMethod("tolower", inputBaseClass = "CharacterList",
                    outputBaseClass = "CharacterList",  applyToUnlist = TRUE)
setAtomicListMethod("toupper", inputBaseClass = "CharacterList",
                    outputBaseClass = "CharacterList", applyToUnlist = TRUE)
setAtomicListMethod("sub", inputBaseClass = "CharacterList",
                    outputBaseClass = "CharacterList", whichArg = 3L,
                    applyToUnlist = TRUE)
setAtomicListMethod("gsub", inputBaseClass = "CharacterList",
                    outputBaseClass = "CharacterList", whichArg = 3L,
                    applyToUnlist = TRUE)
### TODO: grep, grepl

setMethod("unstrsplit", "CharacterList",
    function(x, sep="") unstrsplit(as.list(x), sep=sep)
)

setMethod("unstrsplit", "RleList",
          function(x, sep="") unstrsplit(CharacterList(x, compress=FALSE),
                                         sep=sep)
          )

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

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Sorting
###

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
              r <- as.integer(IRanges(1L, width=width(p)))
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
                    method = c("shell", "radix"))
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
    
### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Set/comparison methods
###

subgrouping <- function(x) {
    g <- grouping(togroup(PartitioningByEnd(x)), unlist(x, use.names=FALSE))
    as(g, "ManyToOneGrouping")
}

.unique.RleList <- function(x, incomparables=FALSE, ...)
    unique(runValue(x), incomparables=incomparables, ...)
setMethod("unique", "RleList", .unique.RleList)

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

