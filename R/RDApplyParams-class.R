### =========================================================================
### RDApplyParams objects
### -------------------------------------------------------------------------

setClassUnion("functionORNULL", c("function", "NULL"))

setClass("RDApplyParams",
         representation(rangedData = "RangedData", applyFun = "function",
                        applyParams = "list", ##excludePattern = "character",
                        filterRules = "FilterRules", simplify = "logical",
                        reducerFun = "functionORNULL", reducerParams = "list",
                        iteratorFun = "function"),
         prototype(applyFun = function(rd) NULL, simplify = FALSE,
                   iteratorFun = sapply))

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Accessor methods.
###

setGeneric("rangedData", function(x, ...) standardGeneric("rangedData"))
setMethod("rangedData", "RDApplyParams", function(x) x@rangedData)

setGeneric("rangedData<-",
           function(x, ..., value) standardGeneric("rangedData<-"))
setReplaceMethod("rangedData", "RDApplyParams", function(x, value) {
  x@rangedData <- value
  validObject(x)
  x
})

setGeneric("applyFun", function(x, ...) standardGeneric("applyFun"))
setMethod("applyFun", "RDApplyParams", function(x) x@applyFun)

setGeneric("applyFun<-", function(x, ..., value) standardGeneric("applyFun<-"))
setReplaceMethod("applyFun", "RDApplyParams", function(x, value) {
  x@applyFun <- value
  validObject(x)
  x
})

setGeneric("applyParams", function(x, ...) standardGeneric("applyParams"))
setMethod("applyParams", "RDApplyParams", function(x) x@applyParams)

setGeneric("applyParams<-",
           function(x, ..., value) standardGeneric("applyParams<-"))
setReplaceMethod("applyParams", "RDApplyParams", function(x, value) {
  x@applyParams <- value
  validObject(x)
  x
})

## setGeneric("excludePattern",
##            function(x, ...) standardGeneric("excludePattern"))
## setMethod("excludePattern", "RDApplyParams", function(x) x@excludePattern)

## setGeneric("excludePattern<-",
##            function(x, ..., value) standardGeneric("excludePattern<-"))
## setReplaceMethod("excludePattern", "RDApplyParams", function(x, value) {
##   x@excludePattern <- value
##   validObject(x)
##   x
## })

setGeneric("filterRules", function(x, ...) standardGeneric("filterRules"))
setMethod("filterRules", "RDApplyParams", function(x) x@filterRules)

setGeneric("filterRules<-",
           function(x, ..., value) standardGeneric("filterRules<-"))
setReplaceMethod("filterRules", "RDApplyParams", function(x, value) {
  x@filterRules <- value
  validObject(x)
  x
})

setGeneric("simplify", function(x, ...) standardGeneric("simplify"))
setMethod("simplify", "RDApplyParams", function(x) x@simplify)

setGeneric("simplify<-", function(x, ..., value) standardGeneric("simplify<-"))
setReplaceMethod("simplify", "RDApplyParams", function(x, value) {
  x@simplify <- value
  validObject(x)
  x
})

setGeneric("reducerFun", function(x, ...) standardGeneric("reducerFun"))
setMethod("reducerFun", "RDApplyParams", function(x) x@reducerFun)

setGeneric("reducerFun<-",
           function(x, ..., value) standardGeneric("reducerFun<-"))
setReplaceMethod("reducerFun", "RDApplyParams", function(x, value) {
  x@reducerFun <- value
  validObject(x)
  x
})

setGeneric("reducerParams", function(x, ...) standardGeneric("reducerParams"))
setMethod("reducerParams", "RDApplyParams", function(x) x@reducerParams)

setGeneric("reducerParams<-", function(x, ..., value)
           standardGeneric("reducerParams<-"))
setReplaceMethod("reducerParams", "RDApplyParams", function(x, value) {
  x@reducerParams <- value
  validObject(x)
  x
})

setGeneric("iteratorFun", function(x, ...) standardGeneric("iteratorFun"))
setMethod("iteratorFun", "RDApplyParams", function(x) x@iteratorFun)

setGeneric("iteratorFun<-", function(x, ..., value)
           standardGeneric("iteratorFun<-"))
setReplaceMethod("iteratorFun", "RDApplyParams", function(x, value) {
  x@iteratorFun <- value
  validObject(x)
  x
})

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Constructor
###

## Simple convenience constructor around RDApplyParams's initializer
RDApplyParams <- function(rangedData, applyFun, applyParams, #excludePattern,
                          filterRules, simplify, reducerFun, reducerParams,
                          iteratorFun)
{
  params <- new("RDApplyParams", applyFun = applyFun,
                applyParams = applyParams, filterRules = filterRules,
                simplify = simplify, reducerFun = reducerFun,
                reducerParams = reducerParams, iteratorFun = iteratorFun)
  params@rangedData <- rangedData ## set rangedData last for efficiency
  params
}

## get the defaults from the class prototype
formals(RDApplyParams) <- structure(lapply(slotNames("RDApplyParams"),
                                           function(x) {
                                             slot(new("RDApplyParams"), x)
                                           }),
                                    names = slotNames("RDApplyParams")) 
                                    

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Validity.
###

.valid.RDApplyParams.applyFun <- function(x) {
  formals <- formals(applyFun(x))
  if (anyDuplicated(names(applyParams(x))))
    "apply parameters have duplicated names"
  else if (!("..." %in% names(formals))) {
    if (length(formals) < (length(applyParams(x)) + 1))
      "'applyFun' does not take enough parameters"
    else {
      nms <- names(applyParams(x))
      if (!is.null(nms) && !all(nms[nchar(nms) > 0] %in% names(formals)))
        "mismatch between names of 'applyParams' and formals of 'applyFun'"
      else NULL
    }
  } else NULL
}

## .valid.RDApplyParams.excludePattern <- function(x) {
##   if (!isSingleString(excludePattern(x)))
##     "'excludePattern' must be a single, non-missing string"
##   else NULL
## }

.valid.RDApplyParams.simplify <- function(x) {
  if (!isTRUEorFALSE(simplify(x)))
    "'simplify' must be TRUE or FALSE"
  else if (!is.null(reducerFun(x)) && simplify(x))
    "'simplify' must be FALSE for there to be a 'reducerFun'"
  else NULL
}

.valid.RDApplyParams.reducerParams <- function(x) {
  if (length(reducerParams(x)) && is.null(reducerFun(x)))
    return("there must be a 'reducerFun' for there to be 'reducerParams'")
  else if (anyDuplicated(names(reducerParams(x))))
    return("reducer parameters have duplicated names")
  else if (!is.null(reducerFun(x))) {
    formals <- formals(reducerFun(x))
    if (!("..." %in% names(formals))) {
      if (length(formals) < (length(reducerParams(x)) + 1))
        return("'reducerFun' does not take enough parameters")
      else {
        nms <- names(reducerParams(x))
        if (!is.null(nms) && !all(nms[nchar(nms) > 0] %in% names(formals)) &&
            !("..." %in% names(formals)))
          return("mismatch b/w 'reducerParams' names and 'reducerFun' formals")
      }
    }
  }
  NULL
}

.valid.RDApplyParams.iteratorFun <- function(x) {
  formals <- formals(iteratorFun(x))
  if (length(formals) < 2)
    "'iteratorFun' must take at least two parameters"
  else if ("simplify" %in% names(formals) && length(formals) < 3)
    "'iteratorFun' must take at least three parameters if one is 'simplify'"
  else NULL
}

.valid.RDApplyParams <- function(x)
  c(##.valid.RDApplyParams.rangedData(x),
    .valid.RDApplyParams.applyFun(x),
    ##.valid.RDApplyParams.excludePattern(x),
    .valid.RDApplyParams.simplify(x), .valid.RDApplyParams.reducerParams(x),
    .valid.RDApplyParams.iteratorFun(x))

setValidity2("RDApplyParams", .valid.RDApplyParams)
