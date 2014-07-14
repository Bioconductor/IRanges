### =========================================================================
### seqapply() and family
### -------------------------------------------------------------------------
###

## FIXME: these functions should probably be deprecated; we no longer
## use the term 'sequence' in the Vector API, and the wrappers are
## trivially simple.

.asList <- function(x, ...) {
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
  .Deprecated("as(lapply(X, FUN, ...), 'List')")
  .asList(lapply(X, FUN, ...))
}

mseqapply <- function(FUN, ..., MoreArgs = NULL, USE.NAMES = TRUE) {
  .Deprecated("as(mapply(FUN, ...), 'List')")
  .asList(mapply_List(FUN, ..., MoreArgs = MoreArgs, SIMPLIFY = FALSE,
                      USE.NAMES = USE.NAMES))
}

tseqapply <- function(X, INDEX, FUN = NULL, ...) {
  .Deprecated("as(tapply(X, INDEX, FUN, ...), 'List')")
  .asList(tapply(X, INDEX, FUN, ..., simplify = FALSE))
}

seqsplit <- function(x, f, drop=FALSE) {
  .Deprecated("splitAsList")
  ans_class <- try(relistToClass(x), silent=TRUE)
  if (inherits(ans_class, "try-error"))
    return(.asList(split(x, f, drop)))
  splitAsList(x, f, drop=drop)
}

seqby <- function(data, INDICES, FUN, ...) {
  .Deprecated("as(by(X, INDICES, FUN, ...), 'List')")
  .asList(by(data, INDICES, FUN, ..., simplify = FALSE))
}

### These are excluded from the deprecation

## NOT exported.
`splitAsList<-` <- function(x, f, drop = FALSE, ..., value) {
  if (!isTRUEorFALSE(drop))
    stop("'drop' must be TRUE or FALSE")
  if (length(x) != length(f))
    stop("Length of 'f' must equal the length of 'x'")
  ind <- splitAsList(seq_len(length(x)), f, drop = drop)
  if (length(ind) != length(value))
    stop("Length of 'value' must equal the length of a split on 'f'")
  x[unlist(ind, use.names=FALSE)] <- unlist(value, use.names = FALSE)
  x
}

## 2 wrappers to `seqsplit<-`.

setMethod("unsplit", "List", function(value, f, drop = FALSE) {
  value_flat <- unlist(value, use.names = FALSE)
  if (length(value_flat) != length(f))
    stop("Length of 'unlist(value)' must equal length of 'f'")
  splitAsList(value_flat, f, drop = drop) <- value
  value_flat
})

setReplaceMethod("split", "Vector", function(x, f, drop = FALSE, ..., value) {
  splitAsList(x, f, drop = drop, ...) <- value
  x
})

