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
            ans <- try(do.call(c, unname(lapply(X, FUN, ...))), silent = TRUE)
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
  enclos <- parent.frame(3) 
  inds <- seq(length(rd))
  names(inds) <- names(rd)
  ##   if (length(excludePattern)) {
  ##     excludePattern <- grep(excludePattern, names(rd))
  ##     if (length(excludePattern))
  ##       inds <- inds[-excludePattern]
  ##   }
  forEachSpace <- function(i) {
    rdi <- rd[i]
    if (length(rules)) {
      filter <- eval(rules, rdi, enclos)
      rdi <- rdi[filter,]
    }
    do.call(applyFun, c(list(rdi), applyParams))
  }
  iteratorFun <- iteratorFun(x)
  if ("simplify" %in% names(formals(iteratorFun)))
    ans <- iteratorFun(inds, forEachSpace, simplify = simplify)
  else ans <- iteratorFun(inds, forEachSpace)
  if (!is.null(reducerFun))
    ans <- do.call(reducerFun, c(list(ans), reducerParams))
  ans
})

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### within()
###

setMethod("within", "RangedData",
          function(data, expr, ...)
          {
            e <- list2env(as.list(as(data, "DataFrame")))
            e$ranges <- ranges(data)
            eval(substitute(expr), e, parent.frame())
            reserved <- c("ranges", "start", "end", "width", "space")
            l <- mget(setdiff(ls(e), reserved), e)
            l <- l[!sapply(l, is.null)]
            nD <- length(del <- setdiff(colnames(data), (nl <- names(l))))
            for (nm in nl)
              data[[nm]] <- l[[nm]]
            for (nm in del) 
              data[[nm]] <- NULL
            if (!identical(ranges(data), e$ranges))
              ranges(data) <- e$ranges
            else {
              if (!identical(start(data), e$start))
                start(data) <- e$start
              if (!identical(end(data), e$end))
                end(data) <- e$end
              if (!identical(width(data), e$width))
                width(data) <- e$width
            }
            data
          })

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Merging (TODO)
###

#setMethod("merge", "RangedData",
#          function(x, y, by = 1, all = FALSE, all.x = all, all.y = all,
#                   resolver = intersect, sort = TRUE, suffixes = c(".x",".y"))
#          {
#            
#          })

