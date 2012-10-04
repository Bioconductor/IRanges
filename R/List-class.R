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
          try(.Call2("sapply_NROW", x, PACKAGE="IRanges"), silent=TRUE)
        if (!inherits(ans, "try-error")) {
            names(ans) <- names(x)
            return(ans)
        }
        ## From here, 'length(x)' is guaranteed to be != 0
        return(sapply(x, NROW))
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
        return(sapply(y, NROW))
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
### Constructors.
###

compress_listData <- function(x) {
    if (length(x) > 0L) {
        if (length(dim(x[[1L]])) < 2L) {
            x <- do.call(c, unname(x))
        } else {
            x <- do.call(rbind, unname(x))
        }
    }
    x
}

reconcileMetadatacols <- function(x) {
  x_mcols <- mcols(x)
  if (is(x_mcols, "DataFrame") &&
      nrow(x_mcols) == 0L && ncol(x_mcols) == 0L)
    {
      x_mcols <- new("DataFrame", nrows=length(x))
      mcols(x) <- x_mcols
    }
  x
}

### NOT exported.
### Value for elementMetadata slot can be passed either with
###   newList(..., elementMetadata=somestuff)
### or with
###   newList(..., mcols=somestuff)
### The latter is the new recommended form.
newList <- function(Class, listData, ..., mcols)
{
    if (!extends(Class, "SimpleList") && !extends(Class, "CompressedList"))
        stop("class ", Class, " must extend SimpleList or CompressedList")
    if (!is.list(listData))
        stop("'listData' must be a list object")
    if (is.array(listData)) { # drop any unwanted dimensions
        tmp_names <- names(listData)
        dim(listData) <- NULL # clears the names
        names(listData) <- tmp_names
    }
    class(listData) <- "list"
    ans_elementType <- elementType(new(Class))
    if (!all(sapply(listData,
                    function(x) extends(class(x), ans_elementType))))
        stop("all elements in 'listData' must be ", ans_elementType, " objects")
    if (extends(Class, "SimpleList")) {
        if (missing(mcols))
            return(new2(Class, listData=listData, ..., check=FALSE))
        return(new2(Class, listData=listData, ..., elementMetadata=mcols,
                    check=FALSE))
    }
    ans_partitioning <- PartitioningByEnd(listData)
    if (length(listData) == 0L) {
        if (missing(mcols))
            return(new2(Class, partitioning=ans_partitioning, ..., check=FALSE))
        return(new2(Class, partitioning=ans_partitioning, ...,
                    elementMetadata=mcols, check=FALSE))
    }
    ans_unlistData <- compress_listData(listData)
    if (missing(mcols)) {
        ans <- new2(Class, unlistData=ans_unlistData,
                    partitioning=ans_partitioning, ...,
                    check=FALSE)
    } else {
        ans <- new2(Class, unlistData=ans_unlistData,
                    partitioning=ans_partitioning, ...,
                    elementMetadata=mcols,
                    check=FALSE)
    }
    reconcileMetadatacols(ans)
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "show" method.
###

setMethod("show", "List",
          function(object)
          {
              lo <- length(object)
              cat(class(object), " of length ", lo, "\n", sep = "")
              if (!is.null(names(object)))
                  cat(labeledLine("names", names(object)))
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
    elens <- sapply(seqs, length) ## elementLengths uses NROW, inappropriate
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
    contains <- lapply(cl, function(x) getClass(x, TRUE)@contains)
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
        contains <- getClass(cl, TRUE)@contains
        cl <- names(contains)
        pkg <- sapply(contains, packageSlot)
      } else {
        cl <- tail(cl, -1)
        pkg <- tail(pkg, -1)
      }
      if (length(cl)) {
        if (!length(pkg))
          ns <- list(topenv())
        connms <- constructorName(cl)
        ns <- lapply(pkg, getNamespace)
        coni <- head(which(mapply(exists, connms, ns)), 1)
        if (length(coni))
          cons <- get(connms[coni], ns[[coni]])
      }
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
  ans_class <- try(splitAsListReturnedClass(x), silent=TRUE)
  if (inherits(ans_class, "try-error"))
    return(castList(split(x, f, drop)))
  splitAsList(x, f, drop=drop)
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

listClassName <- function(impl, element.type) {
  if (is.null(impl))
    impl <- ""
  if (!is.null(element.type)) {
    cl <- c(element.type, names(getClass(element.type)@contains))
    cl <- capitalize(cl)
  } else {
    cl <- ""
  }
  listClass <- c(paste0(impl, cl, "List"), paste0(cl, "List"))
  clExists <- which(sapply(listClass, isClass) &
                    sapply(listClass, extends, paste0(impl, "List")))
  if (length(clExists) == 0L) {
    stop("Could not find a '", impl,
         "List' subclass for values of type '", cl, "'")
  }
  listClass[clExists[1L]]
}

coerceToList <- function(from, element.type = NULL, ...) {
  if (is(from, listClassName(NULL, element.type)))
    return(from)
  if (is.list(from) || is(from, "List")) {
    if (is.list(from)) {
      v <- compress_listData(from)
    } else {
      v <- unlist(from, use.names = FALSE)
    }
    part <- PartitioningByEnd(from)
  } else {
    v <- from
    part <- PartitioningByEnd(seq_len(length(from)))
  }
  if (!is.null(element.type)) {
    v <- coercerToClass(element.type)(v, ...)
  }
  to <- relist(v, part)
  names(to) <- names(from)
  to
}

setAs("ANY", "List", function(from) {
  coerceToList(from)
})

## Special cased, because integer extends ANY (somehow) and numeric,
## so ambiguities are introduced due to method caching.
setAs("integer", "List", getMethod(coerce, c("ANY", "List")))

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

setMethod("unsplit", "List", function(value, f, drop = FALSE) {
  value_flat <- unlist(value, use.names = FALSE)
  if (length(value_flat) != length(f))
    stop("Length of 'unlist(value)' must equal length of 'f'")
  seqsplit(value_flat, f, drop = drop) <- value
  value_flat
})

.stack.ind <- function(x, index.var = "name") {
  if (length(names(x)) > 0) {
    spaceLabels <- names(x)
  } else {
    spaceLabels <- seq_len(length(x))
  }
  ind <- Rle(factor(spaceLabels, levels = unique(spaceLabels)),
             elementLengths(x))
  do.call(DataFrame, structure(list(ind), names = index.var))
}

setMethod("stack", "List",
          function(x, index.var = "name", value.var = "value")
          {
            df <- DataFrame(.stack.ind(x, index.var),
                            as(unlist(x, use.names=FALSE), "DataFrame"))
            colnames(df)[2] <- value.var
            df
          })


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Evaluating.
###
  
setMethod("eval", c("expression", "List"),
    function(expr, envir, enclos = parent.frame())
          eval(expr, as.env(envir, enclos))
)

setMethod("eval", c("language", "List"),
    function(expr, envir, enclos = parent.frame())
          eval(expr, as.env(envir, enclos))
)

setMethod("with", "List",
          function(data, expr, ...)
          {
            eval(substitute(expr), data, parent.frame())
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
