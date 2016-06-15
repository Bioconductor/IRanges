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
setMethod("rangedData", "RDApplyParams", function(x)
  .Defunct(msg="RDApplyParams objects and rdapply() are defunct")
)

setGeneric("rangedData<-",
           function(x, ..., value) standardGeneric("rangedData<-"))
setReplaceMethod("rangedData", "RDApplyParams", function(x, value) {
  x@rangedData <- value
  validObject(x)
  x
})

setGeneric("applyFun", function(x, ...) standardGeneric("applyFun"))
setMethod("applyFun", "RDApplyParams", function(x)
  .Defunct(msg="RDApplyParams objects and rdapply() are defunct")
)

setGeneric("applyFun<-", function(x, ..., value) standardGeneric("applyFun<-"))
setReplaceMethod("applyFun", "RDApplyParams", function(x, value) {
  x@applyFun <- value
  validObject(x)
  x
})

setGeneric("applyParams", function(x, ...) standardGeneric("applyParams"))
setMethod("applyParams", "RDApplyParams", function(x)
  .Defunct(msg="RDApplyParams objects and rdapply() are defunct")
)

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

setMethod("filterRules", "RDApplyParams", function(x)
  .Defunct(msg="RDApplyParams objects and rdapply() are defunct")
)

setGeneric("filterRules<-",
           function(x, ..., value) standardGeneric("filterRules<-"))
setReplaceMethod("filterRules", "RDApplyParams", function(x, value) {
  x@filterRules <- value
  validObject(x)
  x
})

setGeneric("simplify", function(x, ...) standardGeneric("simplify"))
setMethod("simplify", "RDApplyParams", function(x)
  .Defunct(msg="RDApplyParams objects and rdapply() are defunct")
)

setGeneric("simplify<-", function(x, ..., value) standardGeneric("simplify<-"))
setReplaceMethod("simplify", "RDApplyParams", function(x, value) {
  x@simplify <- value
  validObject(x)
  x
})

setGeneric("reducerFun", function(x, ...) standardGeneric("reducerFun"))
setMethod("reducerFun", "RDApplyParams", function(x)
  .Defunct(msg="RDApplyParams objects and rdapply() are defunct")
)

setGeneric("reducerFun<-",
           function(x, ..., value) standardGeneric("reducerFun<-"))
setReplaceMethod("reducerFun", "RDApplyParams", function(x, value) {
  x@reducerFun <- value
  validObject(x)
  x
})

setGeneric("reducerParams", function(x, ...) standardGeneric("reducerParams"))
setMethod("reducerParams", "RDApplyParams", function(x)
  .Defunct(msg="RDApplyParams objects and rdapply() are defunct")
)

setGeneric("reducerParams<-", function(x, ..., value)
           standardGeneric("reducerParams<-"))
setReplaceMethod("reducerParams", "RDApplyParams", function(x, value) {
  x@reducerParams <- value
  validObject(x)
  x
})

setGeneric("iteratorFun", function(x, ...) standardGeneric("iteratorFun"))
setMethod("iteratorFun", "RDApplyParams", function(x)
  .Defunct(msg="RDApplyParams objects and rdapply() are defunct")
)

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
  .Defunct(msg="RDApplyParams objects and rdapply() are defunct")

## get the defaults from the class prototype
formals(RDApplyParams) <- structure(lapply(slotNames("RDApplyParams"),
                                           function(x) {
                                             slot(new("RDApplyParams"), x)
                                           }),
                                    names = slotNames("RDApplyParams")) 
                                    

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Validity.
###

.valid.RDApplyParams <- function(x)
  .Defunct(msg="RDApplyParams objects and rdapply() are defunct")

setValidity2("RDApplyParams", .valid.RDApplyParams)
