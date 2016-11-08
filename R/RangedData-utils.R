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
            FUN <- match.fun(FUN)
            ans <- try(do.call(c, unname(lapply(X, FUN, ...))), silent = TRUE)
            if (inherits(ans, "try-error") || (class(ans) != class(X)))
              stop("'FUN' did not produce an endomorphism")
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
            S4Vectors:::safeEval(substitute(expr), e, S4Vectors:::top_prenv(expr))
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

