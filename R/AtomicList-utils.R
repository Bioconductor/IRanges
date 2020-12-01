### =========================================================================
### Common operations on AtomicList objects
### -------------------------------------------------------------------------


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
          signature(e1 = "AtomicList", e2 = "AtomicList"),
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

setMethod("Ops",
          signature(e1 = "AtomicList", e2 = "atomic"),
          function(e1, e2)
          {
              e2 <- as(e2, class(e1))
              callGeneric(e1, e2)
          })

setMethod("Ops",
          signature(e1 = "atomic", e2 = "AtomicList"),
          function(e1, e2)
          {
              e1 <- as(e1, class(e2))
              callGeneric(e1, e2)
          })

### Only to make unary + and - work (i.e. '+x' and '-x').
setMethod("Ops", c("AtomicList", "missing"),
    function(e1, e2) callGeneric(0L, e1)
)

setMethod("Math", "AtomicList",
          function(x) as(lapply(x, .Generic), "List"))

setMethod("Math2", "AtomicList",
          function(x, digits)
          {
              if (missing(digits))
                  digits <- ifelse(.Generic == "round", 0, 6)
              as(lapply(x, .Generic, digits = digits), "List")
          })

setMethod("Summary", "AtomicList",
          function(x, ..., na.rm = FALSE) {
            sapply(x, .Generic, na.rm = na.rm)
        })

setMethod("Complex", "AtomicList",
          function(z) as(lapply(x, .Generic), "List"))


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Logical methods
###

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

for (i in c("IntegerList", "NumericList", "RleList")) {
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


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Running window statistic methods
###

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

### TODO: paste, ...

setMethod("unstrsplit", "CharacterList",
    function(x, sep="") unstrsplit(as.list(x), sep=sep)
)

setMethod("unstrsplit", "RleList",
          function(x, sep="") unstrsplit(CharacterList(x, compress=FALSE),
                                         sep=sep)
          )

setMethods("nchar", c("CharacterList", "RleList"),
           function(x) {
               relist(nchar(unlist(x, use.names=FALSE)), x)
           })

setMethods("substring", c("CharacterList", "RleList"),
           function(text, first, last = 1000000L) {
               relist(substring(unlist(text, use.names=FALSE), first, last),
                      text)
           })

setMethods("substr", c("CharacterList", "RleList"),
           function(x, start, stop) {
               relist(substr(unlist(x, use.names=FALSE), start, stop), x)
           })

setMethods("chartr", list(c(old = "ANY", new = "ANY", x = "CharacterList"),
                          c(old = "ANY", new = "ANY", x = "RleList")),
           function(old, new, x) {
               relist(chartr(old, new, unlist(x, use.names=FALSE)), x)
           })

setMethods("toupper", c("CharacterList", "RleList"),
           function(x) {
               relist(toupper(unlist(x, use.names=FALSE)), x)
           })

setMethods("tolower", c("CharacterList", "RleList"),
           function(x) {
               relist(tolower(unlist(x, use.names=FALSE)), x)
           })

setMethods("sub", list(c("ANY", "ANY", "CharacterList"),
                       c("ANY", "ANY", "RleList")),
           function(pattern, replacement, x, ignore.case = FALSE,
                    perl = FALSE, fixed = FALSE, useBytes = FALSE) {
               relist(sub(pattern, replacement, unlist(x, use.names=FALSE),
                          ignore.case, perl, fixed, useBytes),
                      x)
           })

setMethods("gsub", list(c("ANY", "ANY", "CharacterList"),
                        c("ANY", "ANY", "RleList")),
           function(pattern, replacement, x, ignore.case = FALSE,
                    perl = FALSE, fixed = FALSE, useBytes = FALSE) {
               relist(gsub(pattern, replacement, unlist(x, use.names=FALSE),
                           ignore.case, perl, fixed, useBytes),
                      x)
           })

setMethods("grepl", list(c("ANY", "CharacterList"),
                         c("ANY", "RleList")),
           function(pattern, x, ignore.case = FALSE, perl = FALSE,
                    fixed = FALSE, useBytes = FALSE) {
               relist(grepl(pattern, unlist(x, use.names=FALSE),
                            ignore.case, perl, fixed, useBytes),
                      x)
           })

setMethods("grep", list(c("ANY", "CharacterList"),
                        c("ANY", "RleList")),
           function(pattern, x, ignore.case = FALSE, perl = FALSE,
                    value = FALSE,  fixed = FALSE, useBytes = FALSE,
                    invert = FALSE) {
               stopifnot(isTRUEorFALSE(invert), isTRUEorFALSE(value))
               g <- grepl(pattern, x, ignore.case, perl, fixed, useBytes)
               if (invert) {
                   g <- !g
               }
               if (value) {
                   x[g]
               } else {
                   which(g)
               }
           })

setMethods("startsWith", list(c("CharacterList", "ANY"), c("RleList", "ANY")),
           function(x, prefix) {
               relist(startsWith(unlist(x, use.names = FALSE), prefix), x)
           })

setMethods("endsWith", list(c("CharacterList", "ANY"), c("RleList", "ANY")),
           function(x, suffix) {
               relist(endsWith(unlist(x, use.names = FALSE), suffix), x)
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

