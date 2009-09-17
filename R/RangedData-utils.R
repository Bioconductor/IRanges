### =========================================================================
### RangedData utilities
### -------------------------------------------------------------------------

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Applying
###

setMethod("lapply", "RangedData",
          function(X, FUN, ...)
          {
            FUN <- match.fun(FUN)
            inds <- structure(seq(length(X)), names = names(X))
            lapply(inds, function(i) FUN(X[i], ...))
          })

setMethod("endoapply", "RangedData",
          function(X, FUN, ...) {
            ans <- try(do.call(c, lapply(X, FUN, ...)), silent = TRUE)
            if (inherits(ans, "try-error") || (class(ans) != class(X)))
              stop("'FUN' did not produce an endomorphism")
            ans
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
### findOverlaps()
###

setMethod("findOverlaps", c("RangedData", "RangedData"),
          function(query, subject, maxgap = 0, multiple = TRUE)
          {
            subject <- ranges(subject)
            query <- ranges(query)
            callGeneric()
          })
setMethod("findOverlaps", c("RangedData", "RangesList"),
          function(query, subject, maxgap = 0, multiple = TRUE)
          {
            query <- ranges(query)
            callGeneric()
          })
setMethod("findOverlaps", c("RangesList", "RangedData"),
          function(query, subject, maxgap = 0, multiple = TRUE)
          {
            subject <- ranges(subject)
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

