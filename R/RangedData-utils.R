### =========================================================================
### RangedData utilities
### -------------------------------------------------------------------------

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Applying
###

setMethod("lapply", "RangedData",
          function(X, FUN, ...)
          {
            what <- "\"lapply\" method for RangedData objects"
            .Deprecated(msg=wmsg(RangedData_method_is_deprecated_msg(what)))
            FUN <- match.fun(FUN)
            inds <- structure(seq(length(X)), names = names(X))
            lapply(inds, function(i) FUN(X[i], ...))
          })

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### within()
###

setMethod("within", "RangedData",
          function(data, expr, ...)
          {
            what <- "\"within\" method for RangedData objects"
            .Deprecated(msg=wmsg(RangedData_method_is_deprecated_msg(what)))
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

