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
    expr
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

  ans <- newSimpleList("FilterRules", exprs, active = active)
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
                   value <- FilterRules.parseRule(value)
                   x <- callNextMethod(x, i, value = value)
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
              x@active <- x@active[i]
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
            elementMetadata(ans) <-
              rbind(elementMetadata(x), elementMetadata(values))
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
            if (!all(sapply(args, is, "FilterRules")))
              stop("all arguments in '...' must be FilterRules objects")
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
            elementMetadata(ans) <-
              do.call(rbind, lapply(args, elementMetadata))
            ans
          })

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Evaluating
###

setMethod("eval", signature(expr="FilterRules", envir="ANY"),
          function(expr, envir = parent.frame(),
                   enclos = if(is.list(envir) || is.pairlist(envir))
                   parent.frame() else baseenv())
          {
            result <- TRUE
            for (rule in as.list(expr)[active(expr)]) {
              if (is.expression(rule))
                val <- eval(rule, envir, enclos)
              else val <- rule(envir)
              if (!is.logical(val))
                stop("filter rule evaluated to non-logical: ", rule)
              if (max(length(result), length(val)) %%
                  min(length(result), length(val)) != 0)
                stop("filter rule evaluated to inconsistent length: ", rule)
              result <- val & result
            }
            result
          })

