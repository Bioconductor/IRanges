### =========================================================================
### RangedData utilities
### -------------------------------------------------------------------------

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Applying
###

setMethod("lapply", "RangedData", function(X, FUN, ...) {
  FUN <- match.fun(FUN)
  inds <- seq(length(X))
  names(inds) <- names(X)
  lapply(inds, function(i) FUN(X[i], ...))
})

setMethod("endomorph", "RangedData",
          function(X, FUN, ...) {
              elementTypeX <- elementType(X)
              FUN <- match.fun(FUN)
              ii <- seq_len(length(X))
              names(ii) <- names(X)
              for (i in ii) {
                  elt <- FUN(X[i], ...)
                  if (!extends(class(elt), elementTypeX))
                      stop("'FUN' must return elements of class ", elementTypeX)
                  X[i] <- elt
              }
              X
          })

setGeneric("rdapply", function(x, ...) standardGeneric("rdapply"))

setMethod("rdapply", "RDApplyParams", function(x) {
  rd <- rangedData(x)
  applyFun <- applyFun(x)
  applyParams <- applyParams(x)
  rules <- filterRules(x)
  simplify <- simplify(x)
  reducerFun <- reducerFun(x)
  reducerParams <- reducerParams(x)
  ### FIXME: parent.frame() is useless, so the search will just hit .GlobalEnv
  enclos <- parent.frame() 
  inds <- seq(length(rd))
  names(inds) <- names(rd)
  ##   if (length(excludePattern)) {
  ##     excludePattern <- grep(excludePattern, names(rd))
  ##     if (length(excludePattern))
  ##       inds <- inds[-excludePattern]
  ##   }
  ans <- sapply(inds, function(i) {
    rdi <- rd[i]
    if (length(rules)) {
      filter <- eval(rules, rdi, enclos)
      rdi <- rdi[filter,]
    }
    do.call(applyFun, c(list(rdi), applyParams))
  }, simplify = simplify)
  if (!is.null(reducerFun))
    ans <- do.call(reducerFun, c(list(ans), reducerParams))
  ans
})

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Overlap.
###

setMethod("overlap", c("RangedData", "RangedData"),
          function(object, query, maxgap = 0, multiple = TRUE)
          {
            object <- ranges(object)
            query <- ranges(query)
            callGeneric()
          })
setMethod("overlap", c("RangesList", "RangedData"),
          function(object, query, maxgap = 0, multiple = TRUE)
          {
            query <- ranges(query)
            callGeneric()
          })
setMethod("overlap", c("RangedData", "RangesList"),
          function(object, query, maxgap = 0, multiple = TRUE)
          {
            object <- ranges(object)
            callGeneric()
          })

setMethod("%in%", c("RangedData", "RangedData"),
          function(x, table) ranges(x) %in% ranges(table))
setMethod("%in%", c("RangesList", "RangedData"),
          function(x, table) x %in% ranges(table))
setMethod("%in%", c("RangedData", "RangesList"),
          function(x, table) ranges(x) %in% table)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Merging (TODO, don't export)
###

setGeneric("merge", function(x, y, ...) standardGeneric("merge"))

setMethod("merge", "RangedData",
          function(x, y, by = 1, all = FALSE, all.x = all, all.y = all,
                   resolver = intersect, sort = TRUE, suffixes = c(".x",".y"))
          {
            
          })

