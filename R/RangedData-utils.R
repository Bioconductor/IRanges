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
### findOverlaps()
###

setMethod("findOverlaps", c("RangedData", "RangedData"),
          function(query, subject, maxgap = 0L, minoverlap = 1L,
                   type = c("any", "start", "end", "within", "equal"),
                   select = c("all", "first", "last", "arbitrary"),
                   multiple = TRUE, drop = FALSE)
          {
            if (!missing(multiple) && !multiple) {
              if (!isTRUEorFALSE(multiple))
                stop("'multiple' must be TRUE or FALSE")
              warning("argument 'multiple' is deprecated; use 'select'.")
              select <- "arbitrary"
            }
            findOverlaps(ranges(query), ranges(subject), maxgap = maxgap,
                         minoverlap = minoverlap, type = match.arg(type),
                         select = match.arg(select), drop = drop)
          })
setMethod("findOverlaps", c("RangedData", "RangesList"),
          function(query, subject, maxgap = 0L, minoverlap = 1L,
                   type = c("any", "start", "end", "within", "equal"),
                   select = c("all", "first", "last", "arbitrary"),
                   multiple = TRUE, drop = FALSE)
          {
            if (!missing(multiple) && !multiple) {
              if (!isTRUEorFALSE(multiple))
                stop("'multiple' must be TRUE or FALSE")
              warning("argument 'multiple' is deprecated; use 'select'.")
              select <- "arbitrary"
            }
            findOverlaps(ranges(query), subject, maxgap = maxgap,
                         minoverlap = minoverlap, type = match.arg(type),
                         select = match.arg(select), drop = drop)
          })
setMethod("findOverlaps", c("RangesList", "RangedData"),
          function(query, subject, maxgap = 0L, minoverlap = 1L,
                   type = c("any", "start", "end", "within", "equal"),
                   select = c("all", "first", "last", "arbitrary"),
                   multiple = TRUE, drop = FALSE)
          {
            if (!missing(multiple) && !multiple) {
              if (!isTRUEorFALSE(multiple))
                stop("'multiple' must be TRUE or FALSE")
              warning("argument 'multiple' is deprecated; use 'select'.")
              select <- "arbitrary"
            }
            findOverlaps(query, ranges(subject), maxgap = maxgap,
                         minoverlap = minoverlap, type = match.arg(type),
                         select = match.arg(select), drop = drop)
          })

setMethod("countOverlaps", c("RangedData", "RangedData"),
          function(query, subject, maxgap = 0L, minoverlap = 1L,
                   type = c("any", "start", "end", "within", "equal"))
          {
              countOverlaps(ranges(query), ranges(subject), maxgap = maxgap,
                            minoverlap = minoverlap, type = match.arg(type))
          })
setMethod("countOverlaps", c("RangedData", "RangesList"),
          function(query, subject, maxgap = 0L, minoverlap = 1L,
                   type = c("any", "start", "end", "within", "equal"))
          {
              countOverlaps(ranges(query), subject, maxgap = maxgap,
                            minoverlap = minoverlap, type = match.arg(type))
          })
setMethod("countOverlaps", c("RangesList", "RangedData"),
          function(query, subject, maxgap = 0L, minoverlap = 1L,
                   type = c("any", "start", "end", "within", "equal"))
          {
              countOverlaps(query, ranges(subject), maxgap = maxgap,
                            minoverlap = minoverlap, type = match.arg(type))
          })

setMethod("subsetByOverlaps", c("RangedData", "RangedData"),
          function(query, subject, maxgap = 0L, minoverlap = 1L,
                   type = c("any", "start", "end", "within", "equal"))
          {
              query[unlist(!is.na(findOverlaps(ranges(query), ranges(subject),
                                               maxgap = maxgap,
                                               minoverlap = minoverlap,
                                               type = match.arg(type),
                                               select = "arbitrary")),
                           use.names=FALSE),]
          })
setMethod("subsetByOverlaps", c("RangedData", "RangesList"),
          function(query, subject, maxgap = 0L, minoverlap = 1L,
                   type = c("any", "start", "end", "within", "equal"))
          {
              query[unlist(!is.na(findOverlaps(ranges(query), subject,
                                               maxgap = maxgap,
                                               minoverlap = minoverlap,
                                               type = match.arg(type),
                                               select = "arbitrary")),
                           use.names=FALSE),]
          })
setMethod("subsetByOverlaps", c("RangesList", "RangedData"),
          function(query, subject, maxgap = 0L, minoverlap = 1L,
                  type = c("any", "start", "end", "within", "equal"))
          {
              query[!is.na(findOverlaps(query, ranges(subject),
                                        maxgap = maxgap,
                                        minoverlap = minoverlap,
                                        type = match.arg(type),
                                        select = "arbitrary"))]
          })

setMethod("%in%", c("RangedData", "RangedData"),
          function(x, table) ranges(x) %in% ranges(table))
setMethod("%in%", c("RangesList", "RangedData"),
          function(x, table) x %in% ranges(table))
setMethod("%in%", c("RangedData", "RangesList"),
          function(x, table) ranges(x) %in% table)

setMethod("match", c("RangedData", "RangedData"),
          function(x, table, nomatch = NA_integer_, incomparables = NULL)
          {
              match(ranges(x), ranges(table), nomatch = nomatch,
                    incomparables = incomparables)
          })
setMethod("match", c("RangedData", "RangesList"),
          function(x, table, nomatch = NA_integer_, incomparables = NULL)
          {
              match(ranges(x), table, nomatch = nomatch,
                    incomparables = incomparables)
          })
setMethod("match", c("RangesList", "RangedData"),
          function(x, table, nomatch = NA_integer_, incomparables = NULL)
          {
              match(x, ranges(table), nomatch = nomatch,
                    incomparables = incomparables)
          })

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Reducing
###

setMethod("reduce", "RangedData",
          function(x, by, drop.empty.ranges=FALSE, min.gapwidth=1L,
                   with.inframe.attrib=FALSE)
          {
            if (!isTRUEorFALSE(drop.empty.ranges))
                stop("'drop.empty.ranges' must be TRUE or FALSE")
            if (!isSingleNumber(min.gapwidth))
                stop("'min.gapwidth' must be a single integer")
            if (!is.integer(min.gapwidth))
                min.gapwidth <- as.integer(min.gapwidth)
            if (min.gapwidth < 0L)
                stop("'min.gapwidth' must be non-negative")
            FUN <- function(y) {
              name <- names(y)
              ranges <- ranges(y)[[1L]]
              values <- values(y)[[1L]]
              inds <-
                unname(split(seq_len(nrow(values)), lapply(values, as.vector)))
              rlist <-
                lapply(inds, function(i) {
                         rngs <-
                           reduce(ranges[i],
                                  drop.empty.ranges=drop.empty.ranges,
                                  min.gapwidth=min.gapwidth,
                                  with.inframe.attrib=with.inframe.attrib)
                         list(ranges = rngs,
                              values =
                              values[rep(i, length.out = length(rngs)), ,
                                     drop=FALSE])
                       })
              ranges <-
                IRangesList(do.call(c, lapply(rlist, "[[", "ranges")))
              names(ranges) <- name
              values <-
                SplitDataFrameList(do.call(rbind,
                                           lapply(rlist, "[[", "values")))
              names(values) <- name
              new2(class(y), ranges = ranges, values = values, check = FALSE)
            }
            if (ncol(x) == 0) {
              ranges <-
                reduce(ranges(x),
                       drop.empty.ranges = drop.empty.ranges,
                       min.gapwidth = min.gapwidth,
                       with.inframe.attrib = with.inframe.attrib)
              initialize(x,
                         ranges = ranges,
                         values =
                         newCompressedList("CompressedSplitDataFrameList",
                                           new2("DataFrame",
                                                nrows = sum(elementLengths(ranges)),
                                                check = FALSE),
                                           end = cumsum(elementLengths(ranges)),
                                           NAMES = names(ranges)))
            } else {
              endoapply(x[,by], FUN)
            }
          })

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### within()
###

setMethod("within", "RangedData",
          function(data, expr, ...)
          {
            e <- as.env(data)
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
### Merging (TODO, don't export)
###

setGeneric("merge", function(x, y, ...) standardGeneric("merge"))

setMethod("merge", "RangedData",
          function(x, y, by = 1, all = FALSE, all.x = all, all.y = all,
                   resolver = intersect, sort = TRUE, suffixes = c(".x",".y"))
          {
            
          })

