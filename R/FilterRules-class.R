### =========================================================================
### FilterRules objects
### -------------------------------------------------------------------------

setClassUnion("expressionORfunction", c("expression", "function"))

setClass("FilterRules", representation(active = "logical"),
         prototype(elementType = "expressionORfunction"),
         contains = "SimpleList")

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Accessors.
###

setMethod("active", "FilterRules", function(x) {
  a <- x@active
  names(a) <- names(x)
  a
})

setReplaceMethod("active", "FilterRules", function(x, value) {
  if (is.numeric(value)) {
    value <- as.integer(value)[!is.na(value)]
    if (any(value < 1) || any(value > length(x)))
      stop("filter index out of range")
    value <- names(x)[value]
  }
  if (is.character(value)) {
    value <- value[!is.na(value)] ## NA's are dropped
    filterNames <- names(x)
    if (length(filterNames) == 0)
      stop("there are no filter names")
    if (any(!(value %in% filterNames)))
      stop("'value' contains invalid filter names")
    x@active <- filterNames %in% value
    x
  } else if (is.logical(value)) {
    nfilters <- length(x)
    if (length(value) > nfilters)
      stop("length of 'value' must not be greater than that of 'filters'")
    if (anyMissing(value))
      stop("'value' cannot contain NA's")
    if (nfilters && (nfilters %% length(value) != 0))
      stop("number of filters not a multiple of 'value' length")
    x@active <- rep(value, length.out = nfilters)
    x
  } else stop("unsupported type of 'value'")
})

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Constructor.
###

FilterRules.parseRule <- function(expr) {
  if (is.character(expr)) {
    expr <- try(parse(text = expr, srcfile = NULL), silent = TRUE)
    if (is.character(expr))
      stop("failed to parse filter expression: ", expr)
    expr
  } else if (is.language(expr) || is.logical(expr))
    as.expression(expr)
  else if (is.function(expr))
    new("FilterClosure", expr)
  else stop("would not evaluate to logical: ", expr)
}

## takes logical expressions, or character vectors to parse

FilterRules <- function(exprs = list(), ..., active = TRUE) {
  exprs <- c(as.list(substitute(list(...)))[-1L], exprs)
  if (length(names(exprs)) == 0) {
    names(exprs) <- unlist(lapply(exprs, deparse))
    chars <- unlist(sapply(exprs, is.character))
    names(exprs)[chars] <- unlist(exprs[chars])
  }
  names(exprs) <- make.names(names(exprs), unique = TRUE)

  exprs <- lapply(exprs, FilterRules.parseRule)

  active <- rep(active, length.out = length(exprs))

  if (!is.logical(active) || anyMissing(active))
    stop("'active' must be logical without any missing values")
  if (length(active) > length(exprs))
    stop("length of 'active' is greater than number of rules")
  if (length(exprs) && length(exprs) %% length(active) > 0)
    stop("number of rules must be a multiple of length of 'active'")

  ans <- newList("FilterRules", exprs, active = active)
  validObject(ans)
  ans
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Subsetting.
###

setReplaceMethod("[[", "FilterRules",
                 function(x, i, j, ..., value)
                 {
                   if (!missing(j) || length(list(...)) > 0)
                     warning("arguments beyond 'i' ignored")
                   if (missing(i))
                     stop("subscript is missing")
                   rule <- FilterRules.parseRule(value)
                   x <- callNextMethod(x, i, value = rule)
                   if (is.numeric(i) && is.character(value))
                     names(x)[i] <- value
                   active <- x@active ## in case we expanded
                   names(active) <- names(x)[seq_along(active)]
                   active[[i]] <- TRUE
                   names(active) <- NULL
                   x@active <- active
                   names(x) <- make.names(names(x), unique = TRUE)
                   x
                 })

setMethod("[", "FilterRules",
          function(x, i, j, ..., drop)
          {
            if (!missing(j) || length(list(...)) > 0)
              stop("invalid subsetting")
            if (!missing(i)) {
              x@active <- setNames(setNames(x@active, names(x))[i], NULL)
              x <- callNextMethod(x, i)
            }
            x
          })

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Validity.
###

.valid.FilterRules.active <- function(x) {
  if (length(active(x)) != length(x))
    "length of 'active' must match length of 'filters'"
  else if (!identical(names(active(x)), names(x)))
    "names of 'active' must match those of 'filters'"
  else if (anyMissing(active(x)))
    "'active' cannot contain NA's"
  else NULL
}

.valid.FilterRules.rules <- function(x) {
  unlist(lapply(x, function(rule) {
    if (is.function(rule) && length(formals(rule)) < 1)
      "function rule must take at least one parameter"
    else NULL
  }))
}

.valid.FilterRules <- function(x)
  c(.valid.FilterRules.active(x), .valid.FilterRules.rules(x))

setValidity2("FilterRules", .valid.FilterRules)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Combining
###

setMethod("append", c("FilterRules", "FilterRules"),
          function(x, values, after=length(x))
          {
            if (!isSingleNumber(after))
              stop("'after' must be a single number")
            ans <-
              FilterRules(append(as.list(x, use.names = TRUE),
                                 as.list(values, use.names = TRUE),
                                 after = after))
            active(ans) <-
              structure(append(active(x), active(values), after),
                        names = names(ans))
            mcols(ans) <- rbind(mcols(x), mcols(values))
            ans
          })

setMethod("c", "FilterRules",
          function(x, ..., recursive = FALSE) {
            if (recursive)
              stop("'recursive' mode is not supported")
            if (missing(x))
              args <- unname(list(...))
            else
              args <- unname(list(x, ...))
            args <- lapply(args, as, "FilterRules")              
            ans <-
              FilterRules(unlist(lapply(args,
                                        function(x) {
                                          elts <- as.list(x)
                                          names(elts) <- names(x)
                                          elts
                                        }), recursive = FALSE))
            active(ans) <-
              structure(unlist(lapply(args, active), use.names = FALSE),
                        names = names(ans))
            mcols(ans) <- do.call(rbind, lapply(args, mcols))
            ans
          })

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Evaluating
###

subsetFirstDim <- function(x, i) {
  ndim <- max(length(dim(x)), 1L)
  args <- rep(alist(foo=), ndim)
  names(args) <- NULL
  args[[1]] <- i
  args <- c(list(x), args, list(drop = FALSE))
  do.call(`[`, args)
}

setMethod("eval", signature(expr="FilterRules", envir="ANY"),
          function(expr, envir = parent.frame(),
                   enclos = if(is.list(envir) || is.pairlist(envir))
                   parent.frame() else baseenv())
          {
            result <- rep.int(TRUE, NROW(envir))
            for (rule in as.list(expr)[active(expr)]) {
              if (is.expression(rule))
                val <- eval(rule, envir, enclos)
              else val <- rule(envir)
              if (!is.logical(val))
                stop("filter rule evaluated to non-logical: ", rule)
              if ((NROW(envir) == 0L && length(val) > 0L) ||
                  (NROW(envir) > 0L &&
                   (max(NROW(envir), length(val)) %%
                    min(NROW(envir), length(val)) != 0)))
                stop("filter rule evaluated to inconsistent length: ", rule)
              envir <- subsetFirstDim(envir, val)
              result[result] <- val
            }
            result
          })

setGeneric("evalSeparately",
           function(expr, envir = parent.frame(),
                    enclos = if (is.list(envir) ||
                      is.pairlist(envir)) parent.frame() else baseenv(),
                    ...)
           standardGeneric("evalSeparately"))

setMethod("evalSeparately", "FilterRules",
          function(expr, envir = parent.frame(),
                   enclos = if (is.list(envir) ||
                     is.pairlist(envir)) parent.frame() else baseenv(),
                   serial = FALSE)
          {
            if (!isTRUEorFALSE(serial))
              stop("'serial' must be TRUE or FALSE")
            inds <- seq_len(length(expr))
            names(inds) <- names(expr)
            passed <- rep.int(TRUE, length(envir))
            do.call(cbind, lapply(inds, function(i) {
              result <- eval(expr[i], envir = envir, enclos = enclos)
              if (serial) {
                passed <<- passed & result
                passed
              } else result
            }))
          })

setGeneric("subsetByFilter",
           function(x, filter, ...) standardGeneric("subsetByFilter"))

setMethod("subsetByFilter", c("ANY", "FilterRules"), function(x, filter) {
  subsetFirstDim(x, eval(filter, x))
})

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Summary
###

setMethod("summary", "FilterRules",
          function(object, subject, serial = FALSE, discarded = FALSE,
                   percent = FALSE)
          {
            if (!isTRUEorFALSE(serial))
              stop("'serial' must be TRUE or FALSE")
            if (!isTRUEorFALSE(discarded))
              stop("'discarded' must be TRUE or FALSE")
            if (!isTRUEorFALSE(percent))
              stop("'percent' must be TRUE or FALSE")
            mat <- evalSeparately(object, subject, serial = serial)
            counts <- c("<initial>" = length(subject), colSums(mat),
                        "<final>" = sum(rowSums(mat) == ncol(mat)))
            if (discarded) {
              counts <- length(subject) - counts
            }
            if (percent) {
              round(counts / length(subject), 3)
            } else counts
          })

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### FilterRule closures
###

setClass("FilterClosure", contains = "function")

setGeneric("params", function(x, ...) standardGeneric("params"))

setMethod("params", "FilterClosure", 
          function(x) {
            as.list(environment(x))
          })

setMethod("show", "FilterClosure", function(object) {
  p <- params(object)
  cat("filter (",
      paste(names(p), "=", sapply(p, deparse, control = NULL),
            collapse = ", "),
      ")\n", sep = "")
  print(body(object))
})
