### =========================================================================
### List objects
### -------------------------------------------------------------------------
###
### List objects are Vector objects with "[[", "elementType" and
### "elementLengths" methods.
###

setClass("List",
    contains="Vector",
    representation(
        "VIRTUAL",
        elementType="character"
    ),
    prototype(elementType="ANY")
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Accessor methods.
###

setGeneric("elementType", function(x, ...) standardGeneric("elementType"))
setMethod("elementType", "List", function(x) x@elementType)
setMethod("elementType", "vector", function(x) mode(x))

setGeneric("elementLengths", function(x) standardGeneric("elementLengths"))

setMethod("elementLengths", "ANY",
    function(x)
    {
        x <- as.list(x)
        ans <-
          try(.Call2("listofvectors_lengths", x, PACKAGE="IRanges"), silent=TRUE)
        if (!inherits(ans, "try-error")) {
            names(ans) <- names(x)
            return(ans)
        }
        ## From here, 'length(x)' is guaranteed to be != 0
        return(sapply(x, length))
    }
)

setMethod("elementLengths", "List",
    function(x)
    {
        y <- as.list(x)
        if (length(y) == 0L) {
            ans <- integer(0)
            ## We must return a named integer(0) if 'x' is named
            names(ans) <- names(x)
            return(ans)
        }
        if (length(dim(y[[1L]])) < 2L)
            return(elementLengths(y))
        return(sapply(y, nrow))
    }
)

setGeneric("isEmpty", function(x) standardGeneric("isEmpty"))
setMethod("isEmpty", "ANY",
          function(x)
          {
              if (is.atomic(x))
                  return(length(x) == 0L)
              if (!is.list(x) && !is(x, "List"))
                  stop("isEmpty() is not defined for objects of class ",
                       class(x))
              ## Recursive definition
              if (length(x) == 0)
                  return(logical(0))
              sapply(x, function(xx) all(isEmpty(xx)))
          })


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### List-like API: Element extraction.
###

### Supported types for 'i' are: numeric or character vector.
### If 'i' is a single string with no match in 'names(x)', then raises an
### error by default (i.e. if 'error.if.nomatch=TRUE'), otherwise returns
### NA_integer_.
checkAndTranslateDbleBracketSubscript <- function(x, i, error.if.nomatch=TRUE)
{
    if (!is.numeric(i) && !is.character(i))
        stop("invalid subscript type '", class(i), "'")
    if (length(i) < 1L)
        stop("attempt to extract less than one element")
    if (length(i) > 1L)
        stop("attempt to extract more than one element")
    if (is.na(i))
        stop("invalid subscript NA")
    if (is.numeric(i)) {
        if (!is.integer(i))
            i <- as.integer(i)
        if (i < 1L || length(x) < i)
            stop("subscript out of bounds")
        return(i)
    }
    ## 'i' is a character string
    x_names <- names(x)
    if (is.null(x_names)) {
        if (error.if.nomatch)
            stop("attempt to extract by name when elements have no names")
        return(NA_integer_)
    }
    #if (i == "")
    #    stop("invalid subscript \"\"")
    ans <- match(i, x_names)
    if (is.na(ans) && error.if.nomatch)
        stop("subscript \"", i, "\" matches no name")
    ans
}

setMethod("$", "List", function(x, name) x[[name, exact=FALSE]])

### Fancy subsetting of a List object by a list-like subscript.
### NOT efficient because it loops over the elements of 'i'.
subsetListByList_replace <- function(x, i, value, byrow=FALSE)
{
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
    if (is.null(names(i))) {
        if (length(i) > length(x))
            stop("list-like subscript is longer than ",
                 "list-like object to subset")
        for (ii in seq_len(li)) {
            xx <- x[[ii]]
            if (byrow)
                xx[i[[ii]], ] <- value[[ii]]
            else
                xx[i[[ii]]] <- value[[ii]]
            x[[ii]] <- xx
        }
        return(x)
    }
    if (is.null(names(x)))
        stop("cannot subscript an unnamed list-like object ",
             "by a named list-like object")
    j <- match(names(i), names(x))
    if (anyMissing(j))
        stop("list-like subscript has names not in list-like object to subset")
    for (ii in seq_len(li)) {
        xx <- x[[j[ii]]]
        if (byrow)
            xx[i[[ii]], ] <- value[[ii]]
        else
            xx[i[[ii]]] <- value[[ii]]
        x[[j[ii]]] <- xx
    }
    return(x)
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Looping methods.
###

setMethod("lapply", "List",
          function(X, FUN, ...)
          {
              FUN <- match.fun(FUN)
              ii <- seq_len(length(X))
              names(ii) <- names(X)
              lapply(ii, function(i) FUN(X[[i]], ...))
          })

.sapplyDefault <- base::sapply
environment(.sapplyDefault) <- topenv()
setMethod("sapply", "List", .sapplyDefault)

.mapply_List <- function(FUN, ..., MoreArgs = NULL, SIMPLIFY = TRUE,
                         USE.NAMES = TRUE)
{
    seqs <- list(...)
    isListOrVector <- function(x) is.vector(x) | is(x, "List")
    if (any(!sapply(seqs, isListOrVector)))
        stop("all objects in ... should be a vector or 'List'")
    elens <- elementLengths(seqs)
    if (any(elens == 0L))
      return(list())
    N <- max(elens)
    if (any(N %% elens != 0L))
        stop("all object lengths must be multiple of longest object length")
    recycleExtract <- function(x, i) x[[(i - 1L) %% length(x) + 1L]]
    FUNprime <- function(.__INDEX__, ...) {
        do.call(FUN, c(lapply(seqs, recycleExtract, .__INDEX__), ...))
    }
    nms <- names(seqs[[1]])
    if (is.null(nms) && is.character(seqs[[1]]))
      nms <- seqs[[1]]
    mapply(FUNprime, structure(seq_len(N), names = nms),
           MoreArgs = MoreArgs, SIMPLIFY = SIMPLIFY,
           USE.NAMES = USE.NAMES)
}

setMethod("mapply", "List", .mapply_List)

setMethod("endoapply", "List",
          function(X, FUN, ...) {
              elementTypeX <- elementType(X)
              FUN <- match.fun(FUN)
              for (i in seq_len(length(X))) {
                  elt <- FUN(X[[i]], ...)
                  if (!extends(class(elt), elementTypeX))
                      stop("'FUN' must return elements of class ", elementTypeX)
                  X[[i]] <- elt
              }
              X
          })

setMethod("mendoapply", "List",
          function(FUN, ..., MoreArgs = NULL) {
              X <- list(...)[[1L]]
              elementTypeX <- elementType(X)
              listData <-
                mapply(FUN = FUN, ..., MoreArgs = MoreArgs, SIMPLIFY = FALSE)
              for (i in seq_len(length(listData))) {
                  if (!extends(class(listData[[i]]), elementTypeX))
                      stop("'FUN' must return elements of class ", elementTypeX)
                  X[[i]] <- listData[[i]]
              }
              X
          })

castList <- function(x, ...) {
  if (is(x, "List"))
    return(x)
  if (!is.list(x))
    stop("'x' must be a 'list'")
  cl <- lapply(x, class)
  clnames <- unique(unlist(cl, use.names=FALSE))
  cons <- SimpleList
  if (length(clnames) == 1L) {
    cl <- cl[[1]]
    pkg <- packageSlot(cl)
  } else if (length(clnames)) {
    contains <- lapply(cl, function(x) getClass(x)@contains)
    clnames <- c(clnames,
                 unlist(lapply(contains, names), use.names=FALSE))
    contab <- table(factor(clnames, unique(clnames)))
    cl <- names(contab)[contab == length(x)]
    if (length(cl))
      pkg <- sapply(do.call(c, unname(contains))[cl], packageSlot)
  }
  if (length(cl)) {
    constructorName <- function(x) {
      substring(x, 1, 1) <- toupper(substring(x, 1, 1))
      paste(x, "List", sep = "")
    }
    if (is.null(pkg))
      ns <- topenv()
    else ns <- getNamespace(pkg[1])
    consym <- constructorName(cl[1])
    if (exists(consym, ns))
      cons <- get(consym, ns)
    else {
      if (length(cl) == 1L) {
        contains <- getClass(cl)@contains
        cl <- names(contains)
        pkg <- sapply(contains, packageSlot)
      } else {
        cl <- tail(cl, -1)
        pkg <- tail(pkg, -1)
      }
      if (!length(pkg))
        ns <- list(topenv())
      connms <- constructorName(cl)
      ns <- lapply(pkg, getNamespace)
      coni <- head(which(mapply(exists, connms, ns)), 1)
      if (length(coni))
        cons <- get(connms[coni], ns[[coni]])
    }
  }
  cons(x, ...)
}

seqapply <- function(X, FUN, ...) {
  castList(lapply(X, FUN, ...))
}

mseqapply <- function(FUN, ..., MoreArgs = NULL, USE.NAMES = TRUE) {
  castList(.mapply_List(FUN, ..., MoreArgs = MoreArgs, SIMPLIFY = FALSE,
                        USE.NAMES = USE.NAMES))
}

tseqapply <- function(X, INDEX, FUN = NULL, ...) {
  castList(tapply(X, INDEX, FUN, ..., simplify = FALSE))
}

seqsplit <- function(x, f, drop=FALSE) {
  cl <- class(x)
  cl <- c(cl, names(getClass(cl)@contains))
  substring(cl, 1, 1) <- toupper(substring(cl, 1, 1))
  compressedClass <- paste("Compressed", cl, "List", sep = "")
  clExists <- which(sapply(compressedClass,
                           function(ccl) !is.null(getClassDef(ccl))))
  if (length(clExists))
    newCompressedList(compressedClass[clExists[1L]], x, splitFactor = f,
                      drop = drop)
  else castList(split(x, f, drop))
}

seqby <- function(data, INDICES, FUN, ...) {
  castList(by(data, INDICES, FUN, ..., simplify = FALSE))
}

## mcseqapply <- function(X, FUN, ..., mc.preschedule = TRUE,
##                        mc.set.seed = TRUE, 
##                        mc.silent = FALSE, mc.cores = getOption("cores"),
##                        mc.cleanup = TRUE)
## {
##   castList(mclapply(X, FUN, ..., mc.preschedule = mc.preschedule,
##            mc.set.seed = mc.set.seed, 
##            mc.silent = mc.silent, mc.cores = mc.cores,
##            mc.cleanup = mc.cleanup))
## }

setGeneric("revElements", signature="x",
    function(x, i) standardGeneric("revElements")
)

### This method explains the concept of revElements() but is NOT efficient
### because endoapply() loops over the elements of 'i'.
### There is a fast method for CompressedList objects though.
setMethod("revElements", "List",
    function(x, i)
    {
        if (missing(i))
            i <- seq_len(length(x))
        x[i] <- endoapply(x[i], rev)
        x
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion.
###

setAs("List", "list", function(from) as.list(from))

setMethod("as.list", "List", function(x, ...) lapply(x, identity))

setGeneric("as.env", function(x, ...) standardGeneric("as.env"))

setMethod("as.env", "List",
          function(x, enclos = parent.frame()) {
              nms <- names(x)
              if (is.null(nms))
                  stop("cannot convert to environment when names are NULL")
              env <- new.env(parent = enclos)
              lapply(nms,
                     function(col) {
                         colFun <- function() {
                             val <- x[[col]]
                             rm(list=col, envir=env)
                             assign(col, val, env)
                             val
                         }
                         makeActiveBinding(col, colFun, env)
                     })
              env
          })

### NOT exported. Assumes 'names1' is not NULL.
make.unlist.result.names <- function(names1, names2)
{
    if (is.null(names2))
        return(names1)
    idx2 <- names2 != "" | is.na(names2)
    idx1 <- names1 != "" | is.na(names1)
    idx <- idx1 & idx2
    if (any(idx))
        names1[idx] <- paste(names1[idx], names2[idx], sep = ".")
    idx <- !idx1 & idx2
    if (any(idx))
        names1[idx] <- names2[idx]
    names1
}

setMethod("unlist", "List",
    function(x, recursive=TRUE, use.names=TRUE)
    {
        if (!identical(recursive, TRUE))
            warning("'recursive' argument currently ignored")
        if (!isTRUEorFALSE(use.names))
            stop("'use.names' must be TRUE or FALSE")
        if (length(x) == 0L)
            return(NULL)
        x_names <- names(x)
        if (!is.null(x_names))
            names(x) <- NULL
        xx <- as.list(x)
        if (length(dim(xx[[1L]])) < 2L) {
            ans <- do.call(c, xx)
            ans_names0 <- names(ans)
            if (use.names) {
                if (!is.null(x_names)) {
                    ans_names <- rep.int(x_names, elementLengths(x))
                    ans_names <- make.unlist.result.names(ans_names, ans_names0)
                    try_result <- try(names(ans) <- ans_names, silent=TRUE)
                    if (inherits(try_result, "try-error"))
                        warning("failed to set names on the result ",
                                "of unlisting a ", class(x), " object")
                }
            } else {
                if (!is.null(ans_names0))
                    names(ans) <- NULL
            }
        } else {
            ans <- do.call(rbind, xx)
            if (!use.names)
                rownames(ans) <- NULL
        }
        ans
    }
)

setMethod("relist", signature(skeleton = "List"),
          function(flesh, skeleton) {
            list <- seqsplit(flesh, tofactor(skeleton))
            names(list) <- names(skeleton)
            list
          })

setMethod("unsplit", "List", function(value, f, drop = FALSE) {
  value_flat <- unlist(value, use.names = FALSE)
  if (length(value_flat) != length(f))
    stop("Length of 'unlist(value)' must equal length of 'f'")
  seqsplit(value_flat, f, drop = drop) <- value
  value_flat
})

.stack.ind <- function(x, indName = "space") {
  if (length(names(x)) > 0) {
    spaceLabels <- names(x)
  } else {
    spaceLabels <- seq_len(length(x))
  }
  ind <- Rle(factor(spaceLabels, levels = unique(spaceLabels)),
             elementLengths(x))
  do.call(DataFrame, structure(list(ind), names = indName))
}

setMethod("stack", "List",
          function(x, indName = "space", valuesName = "values")
          {
            df <- DataFrame(.stack.ind(x, indName),
                            as(unlist(x, use.names=FALSE), "DataFrame"))
            colnames(df)[2] <- valuesName
            df
          })


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Functional Programming.
###

#.ReduceDefault <- base::Reduce
#environment(.ReduceDefault) <- topenv()
.ReduceDefault <- function (f, x, init, right = FALSE, accumulate = FALSE) 
{
    mis <- missing(init)
    len <- length(x)
    if (len == 0L) 
        return(if (mis) NULL else init)
    f <- match.fun(f)
#    if (!is.vector(x) || is.object(x)) 
#        x <- as.list(x)
    ind <- seq_len(len)
    if (mis) {
        if (right) {
            init <- x[[len]]
            ind <- ind[-len]
        }
        else {
            init <- x[[1L]]
            ind <- ind[-1L]
        }
    }
    if (!accumulate) {
        if (right) {
            for (i in rev(ind)) init <- f(x[[i]], init)
        }
        else {
            for (i in ind) init <- f(init, x[[i]])
        }
        init
    }
    else {
        len <- length(ind) + 1L
        out <- vector("list", len)
        if (mis) {
            if (right) {
                out[[len]] <- init
                for (i in rev(ind)) {
                    init <- f(x[[i]], init)
                    out[[i]] <- init
                }
            }
            else {
                out[[1L]] <- init
                for (i in ind) {
                    init <- f(init, x[[i]])
                    out[[i]] <- init
                }
            }
        }
        else {
            if (right) {
                out[[len]] <- init
                for (i in rev(ind)) {
                    init <- f(x[[i]], init)
                    out[[i]] <- init
                }
            }
            else {
                for (i in ind) {
                    out[[i]] <- init
                    init <- f(init, x[[i]])
                }
                out[[len]] <- init
            }
        }
        if (all(sapply(out, length) == 1L)) 
            out <- unlist(out, recursive = FALSE)
        out
    }
}

setMethod("Reduce", "List", .ReduceDefault)
  
.FilterDefault <- base::Filter
environment(.FilterDefault) <- topenv()
setMethod("Filter", "List", .FilterDefault)

.FindDefault <- base::Find
environment(.FindDefault) <- topenv()
setMethod("Find", "List", .FindDefault)

.MapDefault <- base::Map
environment(.MapDefault) <- topenv()
setMethod("Map", "List", .MapDefault)
 
setMethod("Position", "List",
    function(f, x, right = FALSE, nomatch = NA_integer_)
    {
        ## In R-2.12, base::Position() was modified to use seq_along()
        ## internally. The problem is that seq_along() was a primitive
        ## that would let the user define methods for it (otherwise it
        ## would have been worth defining a "seq_along" method for Vector
        ## objects). So we need to redefine seq_along() locally in order
        ## to make base_Position() work.
        seq_along <- function(along.with) seq_len(length(along.with))
        base_Position <- base::Position
        environment(base_Position) <- environment()
        base_Position(f, x, right = right, nomatch = nomatch)
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Evaluating.
###
  
setMethod("eval", c("expression", "List"),
    function(expr, envir, enclos = parent.frame())
        eval(expr, as.env(envir), enclos)
)

setMethod("eval", c("language", "List"),
    function(expr, envir, enclos = parent.frame())
        eval(expr, as.env(envir), enclos)
)

setMethod("with", "List",
          function(data, expr, ...)
          {
              eval(substitute(expr), data, parent.frame(2))
          })

setMethod("within", "List",
          function(data, expr, ...)
          {
            ## cannot use active bindings here, as they break for replacement
            e <- list2env(as.list(data))
            ##e <- as.env(data)
            eval(substitute(expr), e, parent.frame(2))
            l <- mget(ls(e), e)
            l <- l[!sapply(l, is.null)]
            nD <- length(del <- setdiff(names(data), (nl <- names(l))))
            for (nm in nl)
              data[[nm]] <- l[[nm]]
            for (nm in del) 
              data[[nm]] <- NULL
            data
          })
