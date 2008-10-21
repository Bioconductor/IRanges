### =========================================================================
### RangedData utilities
### -------------------------------------------------------------------------

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Applying
###

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
### Evaluating
###

setMethod("eval", c("expressionORlanguage", "RangedData"),
          function(expr, envir, enclos = parent.frame())
          {
            env <- new.env(parent = enclos)
            makeActiveBinding("ranges", function() {
              val <- unlist(ranges(envir))
              rm(list="ranges", envir=env)
              assign("ranges", val, env) ## cache for further use
              val
            }, env)
            for (col in colnames(envir))
              makeActiveBinding(col, function() {
                val <- envir[[col]]
                rm(list=col, envir=env)
                assign(col, val, env)
                val
              }, env)
            eval(expr, env)
          })


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Merging (TODO, don't export)
###

setGeneric("merge", function(x, y, ...) standardGeneric("merge"))

setMethod("merge", "RangedData",
          function(x, y, by = 1, all = FALSE, all.x = all, all.y = all,
                   resolver = intersect, sort = TRUE, suffixes = c(".x",".y"))
          {
            
          })
