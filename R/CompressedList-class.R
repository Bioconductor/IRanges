### =========================================================================
### CompressedList objects
### -------------------------------------------------------------------------

setClass("CompressedList",
         contains="List",
         representation(
                        "VIRTUAL",
                        partitioning="PartitioningByEnd",
                        unlistData="ANY"
                       )
         )

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Accessor methods.
###

setMethod("elementLengths", "CompressedList",
    function(x)
    {
        ans <- elementLengths(x@partitioning)
        names(ans) <- names(x)
        ans
    }
)

### A CompressedList object is considered empty iff all its elements are empty.
setMethod("isEmpty", "CompressedList", function(x) all(elementLengths(x) == 0L))

setMethod("length", "CompressedList", function(x) length(x@partitioning))

setMethod("names", "CompressedList", function(x) names(x@partitioning))

setReplaceMethod("names", "CompressedList",
                 function(x, value)
                 {
                     names(x@partitioning) <- value
                     x
                 })

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Constructor.
###

newCompressedList0 <- function(Class, unlistData, partitioning)
{
    ans <- new2(Class, unlistData=unlistData,
                partitioning=partitioning, check=FALSE)
    reconcileMetadatacols(ans)
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Validity.
###

.valid.CompressedList.partitioning <- function(x)
{
    dataLength <- NROW(x@unlistData)
    if (nobj(x@partitioning) != dataLength)
        "improper partitioning"
    else NULL
}
.valid.CompressedList.unlistData <- function(x)
{
    ## FIXME: workaround to support CompressedNormalIRangesList
    ## elementTypeX <- elementType(x)
    elementTypeX <- elementType(new(class(x)))
    if (!extends(class(x@unlistData), elementTypeX))
        paste("the 'unlistData' slot must be of class", elementTypeX)
    else NULL
}
.valid.CompressedList <- function(x)
{
    c(.valid.CompressedList.unlistData(x),
      .valid.CompressedList.partitioning(x))
}
setValidity2("CompressedList", .valid.CompressedList)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Subsetting.
###

### Supported 'i' types: numeric, character, logical, NULL and missing.
setMethod("[", "CompressedList",
          function(x, i, j, ..., drop)
          {
              if (!missing(j) || length(list(...)) > 0)
                  stop("invalid subsetting")
              if (!missing(i)) {
                  if (is(i, "RangesList") || is(i, "RleList") ||
                      is(i, "LogicalList") ||
                      (is(i, "IntegerList") && !is(i, "Ranges"))) {
                      x <- seqselect(x, i)
                  } else {
                      irInfo <-
                        .bracket.Index(i, length(x), names(x), asRanges = TRUE)
                      if (!is.null(irInfo[["msg"]]))
                          stop(irInfo[["msg"]])
                      if (irInfo[["useIdx"]]) {
                          i <- irInfo[["idx"]]
                          ir <-
                            IRanges(end =
                                    seqselect(end(x@partitioning), i),
                                    width =
                                    seqselect(width(x@partitioning), i))
                          x <-
                            initialize(x,
                                       elementMetadata =
                                       seqselect(x@elementMetadata, i),
                                       unlistData = seqselect(x@unlistData, ir),
                                       partitioning =
                                       new2("PartitioningByEnd",
                                            end = cumsum(width(ir)),
                                            NAMES = seqselect(names(x), i),
                                            check=FALSE))
                      }
                  }
              }
              x
          })

### The code is generic and does nothing specific to CompressedList.
### Could be made the method for Vector objects.
setReplaceMethod("[", "CompressedList",
    function(x, i, j, ..., value)
    {
        if (!missing(j) || length(list(...)) > 0L)
            stop("invalid subsetting")
        if (missing(i))
            i <- seq_len(length(x))
        else if (is.list(i) || is(i, "List"))
            return(subsetListByList_replace(x, i, value))
        else
            i <- normalizeSingleBracketSubscript(i, x)
        li <- length(i)
        if (li == 0L) {
            ## Surprisingly, in that case, `[<-` on standard vectors does not
            ## even look at 'value'. So neither do we...
            return(x)
        }
        lv <- length(value)
        if (lv == 0L)
            stop("replacement has length zero")
        if (!is(value, class(x)))
            value <- mk_singleBracketReplacementValue(x, value)
        if (li != lv) {
            if (li %% lv != 0L)
                warning("number of items to replace is not a multiple ",
                        "of replacement length")
            ## Assuming that rep() works on 'value' and also replicates its
            ## names.
            value <- rep(value, length.out = li)
        }
        ## Assuming that c() works on objects of class 'class(x)'.
        ans <- c(x, value)
        idx <- seq_len(length(x))
        idx[i] <- length(x) + seq_len(length(value))
        ## Assuming that [ works on objects of class 'class(x)'.
        ans <- ans[idx]
        ## Restore the original decoration.
        metadata(ans) <- metadata(x)
        names(ans) <- names(x)
        mcols(ans) <- mcols(x)
        ans
    }
)

setMethod("seqselect", "CompressedList",
          function(x, start=NULL, end=NULL, width=NULL)
          {
              lx <- length(x)
              if ((lx > 0) && is.null(end) && is.null(width) &&
                  !is.null(start) && !is(start, "Ranges")) {
                  if (lx != length(start))
                      stop("'length(start)' must equal 'length(x)' when ",
                           "'end' and 'width' are NULL")
                  if (is.list(start)) {
                      if (is.logical(start[[1L]]))
                          start <- LogicalList(start)
                      else if (is.numeric(start[[1L]]))
                          start <- IntegerList(start)
                  } else if (is(start, "RleList")) {
                      start <- IRangesList(start)
                  }
                  if (is(start, "RangesList")) {
                      unlistData <-
                        seqselect(x@unlistData,
                                  shift(unlist(start, use.names = FALSE),
                                        rep.int(start(x@partitioning) - 1L,
                                                elementLengths(start))))
                      partitionEnd <- cumsum(unlist(sum(width(start)),
                                                    use.names = FALSE))
                  } else if (is(start, "LogicalList")) {
                      xeltlen <- elementLengths(x)
                      whichRep <- which(xeltlen != elementLengths(start))
                      for (i in whichRep)
                          start[[i]] <- rep(start[[i]], length.out = xeltlen[i])
                      unlistData <- seqselect(x@unlistData, unlist(start))
                      partitionEnd <-
                        cumsum(unlist(lapply(start, sum), use.names = FALSE))
                  } else if (is(start, "IntegerList")) {
                      i <-
                        unlist(start +
                               relist(start(x@partitioning) - 1L,
                                      PartitioningByEnd(seq_len(lx))))
                      unlistData <- seqselect(x@unlistData, i)
                      partitionEnd <- cumsum(unname(elementLengths(start)))
                  } else {
                      stop("unrecognized 'start' type")
                  }
                  x <-
                    initialize(x,
                               unlistData = unlistData,
                               partitioning = 
                                 new2("PartitioningByEnd",
                                      end = unname(partitionEnd), NAMES = names(x),
                                      check=FALSE))
              } else {
                  if (!is.null(end) || !is.null(width))
                      start <- IRanges(start = start, end = end, width = width)
                  irInfo <- .bracket.Index(start, lx, names(x), asRanges = TRUE)
                  if (!is.null(irInfo[["msg"]]))
                      stop(irInfo[["msg"]])
                  if (irInfo[["useIdx"]])
                      x <- x[irInfo[["idx"]]]
              }
              x
          })

setReplaceMethod("seqselect", "CompressedList",
                 function(x, start = NULL, end = NULL, width = NULL, value)
                 {
                     lx <- length(x)
                     if ((lx > 0) && is.null(end) && is.null(width) &&
                         !is.null(start) && !is(start, "Ranges")) {
                         if (lx != length(start))
                             stop("'length(start)' must equal 'length(x)' ",
                                  "when 'end' and 'width' are NULL")
                         if (is.list(start)) {
                             if (is.logical(start[[1L]]))
                                 start <- LogicalList(start)
                             else if (is.numeric(start[[1L]]))
                                 start <- IntegerList(start)
                         } else if (is(start, "RleList")) {
                             start <- IRangesList(start)
                         }
                         if (is(start, "RangesList")) {
                             start <-
                               shift(unlist(start),
                                     rep.int(start(x@partitioning) - 1L,
                                             elementLengths(start)))
                         } else if (is(start, "LogicalList")) {
                             xeltlen <- elementLengths(x)
                             whichRep <- which(xeltlen != elementLengths(start))
                             for (i in whichRep) {
                                 start[[i]] <-
                                   rep(start[[i]], length.out = xeltlen[i])
                             }
                             start <- unlist(start)
                         } else if (is(start, "IntegerList")) {
                             i <-
                               unlist(start +
                                      relist(start(x@partitioning) - 1L,
                                             PartitioningByEnd(seq_len(length(x)))))
                             start <- rep.int(FALSE, sum(elementLengths(x)))
                             start[i] <- TRUE
                         } else {
                             stop("unrecognized 'start' type")
                         }
                         seqselect(x@unlistData, start) <-
                           unlist(value, use.names=FALSE)
                     } else {
                         x <- callNextMethod()
                     }
                     x
                 })

.CompressedList.list.subscript <-
function(X, INDEX, USE.NAMES = TRUE, COMPRESS = missing(FUN), FUN = identity,
         ...) {
    k <- length(INDEX)
    nonZeroLength <- elementLengths(X)[INDEX] > 0L
    whichNonZeroLength <- which(nonZeroLength)
    kOK <- length(whichNonZeroLength)
    if ((k > 0) && all(nonZeroLength)) {
        zeroLengthElt <- NULL
    } else {
        zeroLengthElt <- FUN(extractROWS(X@unlistData, integer(0)), ...)
    }
    useFastSubset <- (is.vector(X@unlistData) || is(X@unlistData, "Vector"))
    if (!COMPRESS && (k == 0)) {
        elts <- list()
    } else if (COMPRESS && (kOK == 0)) {
        elts <- zeroLengthElt
    } else if(COMPRESS && missing(FUN) && useFastSubset) {
        nzINDEX <- INDEX[whichNonZeroLength]
        elts <-
          seqselect(X@unlistData,
                    start = start(X@partitioning)[nzINDEX],
                    width = width(X@partitioning)[nzINDEX])
    } else {
        elts <- rep(list(zeroLengthElt), k)
        if (kOK > 0) {
            nzINDEX <- INDEX[whichNonZeroLength]
            eltStarts <- start(X@partitioning)[nzINDEX]
            eltEnds <- end(X@partitioning)[nzINDEX]
            oldValidityStatus <- disableValidity()
            disableValidity(TRUE)
            on.exit(disableValidity(oldValidityStatus))
            if (useFastSubset) {
                elts[whichNonZeroLength] <-
                  lapply(seq_len(kOK), function(j)
                         FUN(window(X@unlistData, start = eltStarts[j],
                                    end = eltEnds[j]), ...))
            } else {
                elts[whichNonZeroLength] <-
                  lapply(seq_len(kOK), function(j)
                         FUN(extractROWS(X@unlistData,
                                         eltStarts[j]:eltEnds[j]), ...))
            }
            disableValidity(oldValidityStatus)
        }
        if (COMPRESS) {
            elts <- compress_listData(elts)
        } else {
            for (i in seq_len(length(elts))) {
                obj <- elts[[i]]
                if (isS4(obj) && !isTRUE(validObject(obj, test = TRUE)))
                    stop("invalid output element of class \"", class(obj), "\"")
            }
            if (USE.NAMES)
                names(elts) <- names(X)[INDEX]
        }
    }
    elts
}

setMethod("[[", "CompressedList",
          function(x, i, j, ...) {
              dotArgs <- list(...)
              if (length(dotArgs) > 0)
                  dotArgs <- dotArgs[names(dotArgs) != "exact"]
              if (!missing(j) || length(dotArgs) > 0)
                  stop("incorrect number of subscripts")
              ## H.P.: Do we really need to support subsetting by NA? Other
              ## "[[" methods for other Vector subtypes don't support it.
              if (is.vector(i) && length(i) == 1L && is.na(i))
                  return(NULL)
              index <- checkAndTranslateDbleBracketSubscript(x, i,
                           error.if.nomatch=FALSE)
              if (is.na(index))
                  return(NULL)
              .CompressedList.list.subscript(X = x, INDEX = index,
                                             USE.NAMES = FALSE)
          })

setReplaceMethod("[[", "CompressedList",
                 function(x, i, j,..., value)
                 {
                     nameValue <- if (is.character(i)) i else ""
                     i <- normargSubset2_iOnly(x, i, j, ...,
                              .conditionPrefix="[[<-,CompressedList-method: ")
                     if (is.null(value)) {
                         if (i <= length(x)) # if name did not exist, could be +1
                             x <- x[-i]
                     } else {
                         value <- try(as(value, elementType(x)), silent = TRUE)
                         if (inherits(value, "try-error"))
                             stop("cannot coerce 'value' to a ", elementType(x),
                                  " instance")
                         listData <- as.list(x, use.names = FALSE)
                         listData[[i]] <- value
                         widths <- elementLengths(x)
                         names(widths) <- NULL
                         widths[i] <- NROW(value)
                         if ((i == length(x) + 1L) &&
                             (!is.null(names(x)) || nchar(nameValue) > 0)) {
                             NAMES <- names(x)
                             if (is.null(NAMES))
                                 NAMES <- rep.int("", length(x))
                             NAMES[i] <- nameValue
                         } else {
                             NAMES <- names(x)
                         }
                         slot(x, "unlistData", check=FALSE) <-
                           compress_listData(listData)
                         slot(x, "partitioning", check=FALSE) <-
                           new2("PartitioningByEnd", end = cumsum(widths),
                                NAMES = NAMES, check=FALSE)
                         if (i > length(x))
                           x <- rbindRowOfNAsToMetadatacols(x)
                         x
                     }
                 })

setReplaceMethod("$", "CompressedList",
                 function(x, name, value) {
                     x[[name]] <- value
                     x
                 })


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Combining and splitting.
###

## NOTE: while the 'c' function does not have an 'x', the generic does
## c() is a primitive, so 'x' can be missing; dispatch is by position,
## although sometimes this does not work so well, so it's best to keep
## names off the parameters whenever feasible.

setMethod("c", "CompressedList",
          function(x, ..., recursive = FALSE) {
              if (recursive)
                  stop("'recursive' mode is not supported")
              if (missing(x))
                  tls <- unname(list(...))
              else
                  tls <- unname(list(x, ...))
              if (!all(sapply(tls, is, "CompressedList")))
                  stop("all arguments in '...' must be CompressedList objects")
              ecs <- sapply(tls, elementType)
              if (!all(sapply(ecs, extends, ecs[[1L]])))
                  stop("all arguments in '...' must have an element class ",
                       "that extends that of the first argument")
              if (length(dim(tls[[1L]]@unlistData)) < 2)
                  unlistData <- do.call(c, lapply(tls, slot, "unlistData"))
              else
                  unlistData <- do.call(rbind, lapply(tls, slot, "unlistData"))
              ans_mcols <- do.call(rbind.mcols, tls)
              rownames(ans_mcols) <- NULL
              partitionEnd <-
                cumsum(do.call(c,
                               lapply(tls, function(y) {
                                          z <- elementLengths(y)
                                          names(z) <- NULL
                                          z
                                      })))
              ans_names <-
                do.call(c,
                        lapply(tls, function(y) {
                                   nms <- names(y)
                                   if (is.null(nms))
                                       nms <- rep.int("", length(y))
                                   nms
                               }))
              if (any(nchar(ans_names) != 0L))
                  names(partitionEnd) <- ans_names
              ans <- relist(unlistData, PartitioningByEnd(partitionEnd))
              mcols(ans) <- ans_mcols
              ans
          })

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Looping.
###

setMethod("lapply", "CompressedList",
          function(X, FUN, ...)
          {
              if (length(X) == 0)
                  list()
              else
                  .CompressedList.list.subscript(X = X,
                                                 INDEX = seq_len(length(X)),
                                                 USE.NAMES = TRUE,
                                                 COMPRESS = FALSE,
                                                 FUN = match.fun(FUN), ...)
          })

setMethod("aggregate", "CompressedList",
          function(x, by, FUN, start = NULL, end = NULL, width = NULL,
                   frequency = NULL, delta = NULL, ..., simplify = TRUE)
          {
              if (!missing(by) && is(by, "RangesList")) {
                  if (length(x) != length(by))
                      stop("for Ranges 'by', 'length(x) != length(by)'")
                  y <- as.list(x)
                  result <-
                    lapply(structure(seq_len(length(x)), names = names(x)),
                           function(i)
                               aggregate(y[[i]], by = by[[i]], FUN = FUN,
                                         frequency = frequency, delta = delta,
                                         ..., simplify = simplify))
                  ans <- try(SimpleAtomicList(result), silent = TRUE)
                  if (inherits(ans, "try-error"))
                      ans <- newList("SimpleList", result)
              } else {
                  ans <- callNextMethod()
              }
              ans
          })

.updateCompressedList <- function(X, listData) {
    elementTypeX <- elementType(X)
    if (!all(sapply(listData,
                    function(Xi) extends(class(Xi), elementTypeX))))
        stop("'FUN' must return elements of class ", elementTypeX)
    if (length(listData) == 0) {
        end <- integer(0)
    } else {
        end <- cumsum(unlist(lapply(listData, NROW), use.names = FALSE))
    }
    initialize(X,
               unlistData = compress_listData(listData),
               partitioning = 
               new2("PartitioningByEnd", end = end, NAMES = names(X),
                    check=FALSE))
}

setMethod("endoapply", "CompressedList",
          function(X, FUN, ...) {
              .updateCompressedList(X,
                                    .CompressedList.list.subscript(X = X,
                                              INDEX = seq_len(length(X)),
                                              USE.NAMES = FALSE,
                                              COMPRESS = FALSE,
                                              FUN = match.fun(FUN), ...))
          })

setMethod("mendoapply", "CompressedList",
          function(FUN, ..., MoreArgs = NULL) {
              .updateCompressedList(list(...)[[1L]],
                                    mapply(FUN = FUN, ..., MoreArgs = MoreArgs,
                                           SIMPLIFY = FALSE))
          })

setMethod("revElements", "CompressedList",
    function(x, i)
    {
        if (missing(i))
            i <- seq_len(length(x))
        else
            i <- normalizeSingleBracketSubscript(i, x)
        if (length(x) == 0L)
            return(x)
        elt_lens <- elementLengths(x)
        offset <- cumsum(c(0L, elt_lens[-length(elt_lens)]))
        rev <- logical(length(x))
        rev[i] <- TRUE
        ii <- fancy_mseq(elt_lens, offset=offset, rev=rev)
        x@unlistData <- x@unlistData[ii]
        x
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion.
###

setMethod("as.list", "CompressedList",
          function(x, use.names = TRUE) {
              .CompressedList.list.subscript(X = x,
                                             INDEX = seq_len(length(x)),
                                             USE.NAMES = use.names,
                                             COMPRESS = FALSE)
          })

setMethod("unlist", "CompressedList",
          function(x, recursive = TRUE, use.names = TRUE) {
              if (!missing(recursive))
                  warning("'recursive' argument currently ignored")
              ans <- x@unlistData
              if (length(x) > 0) {
                  if (length(dim(ans)) < 2 && use.names) {
                      if (!is.null(names(x))) {
                          nms <- rep.int(names(x), elementLengths(x))
                          nms <- make.unlist.result.names(nms, names(ans))
                          res <- try(names(ans) <- nms, silent=TRUE)
                          if (is(res, "try-error"))
                              warning("failed to set names on the result ",
                                      "of unlisting a ", class(x), " object")
                      }
                  } else {
                      if (!use.names)
                          rownames(ans) <- NULL
                  }
              }
              ans
          })

