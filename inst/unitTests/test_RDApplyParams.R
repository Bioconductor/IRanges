test_RDApplyParams_construct <- function() {
  fun <- function(rd) NULL
  ranges <- IRanges(c(1,2,3),c(4,5,6))
  filter <- c(1L, 0L, 1L)
  applyParams <- list(x = 2)
  excludePattern <- "[XY]"

  rd <- RangedData(ranges, filter)

  ## meaningless defaults
  checkTrue(validObject(RDApplyParams()))
  checkTrue(validObject(RDApplyParams(rd)))

  ## finally does something
  params <- RDApplyParams(rd, fun)
  checkTrue(validObject(params))
  checkIdentical(rangedData(params), rd)
  checkIdentical(applyFun(params), fun)

  ## make sure function and parameters compatible

  ## applyFun needs 1 param
  checkException(applyFun(params) <- function() NULL, silent = TRUE) 
  checkException(RDApplyParams(rd, function() NULL), silent = TRUE)

  ## needs 2 params
  checkException(RDApplyParams(rd, fun, applyParams), silent = TRUE)
  checkException(applyParams(params) <- applyParams, silent = TRUE)

  ## parameter name mismatch
  checkException(RDApplyParams(rd, function(rd, y) NULL, applyParams),
                 silent = TRUE)
  applyFun(params) <- function(rd, y) NULL
  checkTrue(validObject(params))
  checkException(applyParams(params) <- applyParams, silent = TRUE)

  ## ok with ...
  applyFun(params) <- function(...) NULL 
  checkTrue(validObject(params))
  checkIdentical(applyFun(params), function(...) NULL)
  applyParams(params) <- applyParams
  checkTrue(validObject(params))
  checkIdentical(applyParams(params), applyParams)
  params <- RDApplyParams(rd, function(...) NULL, applyParams) 
  checkTrue(validObject(params))
  checkIdentical(applyParams(params), applyParams)

  ## check for duplicate params
  checkException(applyParams(params) <- rep(applyParams,2), silent = TRUE)
  checkException(RDApplyParams(rd, function(...) NULL, rep(applyParams,2)),
                 silent = TRUE)

  ## exclude pattern -- length 1 character vector
  ## excludePattern(params) <- excludePattern
##   checkTrue(validObject(params))
##   checkIdentical(excludePattern(params), excludePattern)
##   params <- RDApplyParams(rd, function(...) NULL,
##                           excludePattern = excludePattern)
##   checkTrue(validObject(params))
##   checkIdentical(excludePattern(params), excludePattern)
##   checkException(RDApplyParams(rd, function(...) NULL,
##                                excludePattern = rep(excludePattern,2)))
##   checkExcpetion(excludePattern(params)) <- rep(excludePattern, 2)

  ## filters
  filterRules <- FilterRules()
  filterRules(params) <- filterRules
  checkIdentical(filterRules(params), filterRules)
  params <- RDApplyParams(rd, function(...) NULL, filterRules = filterRules)
  checkTrue(validObject(params))
  checkIdentical(filterRules(params), filterRules)
  filterRules <- FilterRules(list(basic = "filter",
                                  advanced = function(rd) NULL))
  filterRules(params) <- filterRules
  checkTrue(validObject(params))
  
  ## simplify -- length 1 logical
  simplify(params) <- TRUE
  checkTrue(validObject(params))
  checkIdentical(simplify(params), TRUE)
  params <- RDApplyParams(rd, function(...) NULL, simplify = TRUE)
  checkTrue(validObject(params))
  checkIdentical(simplify(params), TRUE)
  checkException(RDApplyParams(rd, function(...) NULL,
                               simplify = rep(TRUE,2)), silent = TRUE)
  checkException(simplify(params) <- rep(FALSE, 2), silent = TRUE)

  ## reducer
  reducer <- function(rd) NULL
  ## oops, simplify is TRUE
  checkException(reducerFun(params) <- reducer, silent = TRUE) 
  checkException(RDApplyParams(rd, function(...) NULL, simplify = TRUE,
                               reducerFun = reducer), silent = TRUE)
  simplify(params) <- FALSE
  reducerFun(params) <- reducer
  checkTrue(validObject(params))
  checkIdentical(reducerFun(params), reducer)
  params <- RDApplyParams(rd, function(...) NULL, reducerFun = reducer)
  checkTrue(validObject(params))
  checkIdentical(reducerFun(params), reducer)
  reducerFun(params) <- NULL ## don't reduce
  checkTrue(validObject(params))
  checkIdentical(reducerFun(params), NULL)
  
  ## reducer params
  ## NOTE: for some reason, new() becomes confused if we use 'applyParams' here
  reducerParams <- applyParams
  checkException(RDApplyParams(rd, function(...) NULL, ## oops, no reducer
                               reducerParams = reducerParams), silent = TRUE)
  checkException(reducerParams(params) <- reducerParams, silent = TRUE)

  ## conflicts between reducer and its params

  ## needs 1 param
  checkException(reducerFun(params) <- function() NULL, silent = TRUE) 
  checkException(RDApplyParams(rd, reducerFun = function() NULL), silent = TRUE)
  
  checkException(RDApplyParams(rd, fun, reducerFun = reducer,
                               reducerParams = reducerParams),
                 silent = TRUE) # needs 2 params
  reducerFun(params) <- function(rd) NULL
  checkException(reducerParams(params) <- reducerParams, silent = TRUE)

  ## parameter name mismatch
  checkException(RDApplyParams(rd, function(rd) NULL,
                               reducerFun = function(rd, y) NULL,
                               reducerParams = reducerParams), silent = TRUE)
  reducerFun(params) <- function(rd, y) NULL
  checkTrue(validObject(params))
  checkException(reducerParams(params) <- reducerParams, silent = TRUE)

  ## ok with ...
  reducerFun(params) <- function(...) NULL 
  checkTrue(validObject(params))
  checkIdentical(reducerFun(params), function(...) NULL)
  reducerParams(params) <- reducerParams
  checkTrue(validObject(params))
  checkIdentical(reducerParams(params), reducerParams)
  params <- RDApplyParams(rd, function(rd) NULL,
                          reducerFun = function(...) NULL,
                          reducerParams = reducerParams) 
  checkTrue(validObject(params))
  checkIdentical(reducerParams(params), reducerParams)

  checkException(reducerParams(params) <- rep(reducerParams,2), silent = TRUE)
  checkException(reducerParams(params) <- rep(reducerParams,2), silent = TRUE) 
  checkException(RDApplyParams(rd, function(...) NULL, reducerFun = reducer,
                               reducerParams = rep(reducerParams,2)),
                 silent = TRUE)

  ## iteratorFun
  params <- RDApplyParams(rd, iteratorFun = lapply)
  checkTrue(validObject(params))
  checkIdentical(iteratorFun(params), lapply)
  iteratorFun(params) <- sapply
  checkTrue(validObject(params))
  checkIdentical(iteratorFun(params), sapply)
  checkException(iteratorFun(params) <- function(x) NULL, silent=TRUE)
  checkException(iteratorFun(params) <- function(x, simplify) NULL, silent=TRUE)
}

test_RDApplyParams_rdapply <- function() {
  ranges <- IRanges(c(1,2,3),c(4,5,6))
  filter <- c(2L, 0L, 1L)
  rd <- RangedData(ranges, filter, space = c("chr1","chr2","chr1"))
  fun <- function(rd) NULL
  countrows <- function(rd) nrow(rd)
  applyParams <- list(x = 2)
  excludePattern <- "[XY]"
  
  ## a single function
  params <- RDApplyParams(rd, fun)
  checkIdentical(rdapply(params), list(chr1 = NULL, chr2 = NULL))

  ## with a parameter
  params <- RDApplyParams(rd, function(rd, x) x, list(x = 2))
  checkIdentical(rdapply(params), list(chr1 = 2, chr2 = 2))

  ## add a filter
  cutoff <- 0
  cutoffFun <- function(rd) rd[["filter"]] > cutoff
  rules <- FilterRules(list(filter = cutoffFun))
  params <- RDApplyParams(rd, countrows, filterRules = rules)
  checkIdentical(rdapply(params), list(chr1 = 2L, chr2 = 0L))
  rules <- FilterRules(list(fun = function(rd) rd[["filter"]] < 2,
                            filter = cutoffFun))
  params <- RDApplyParams(rd, countrows, filterRules = rules)
  checkIdentical(rdapply(params), list(chr1 = 1L, chr2 = 0L))
  active(filterRules(params))["filter"] <- FALSE
  checkIdentical(rdapply(params), list(chr1 = 1L, chr2 = 1L))

  ## simplify
  params <- RDApplyParams(rd, countrows, simplify = TRUE)
  checkIdentical(rdapply(params), c(chr1 = 2L, chr2 = 1L))

  ## reducing
  params <- RDApplyParams(rd, fun, reducerFun = unlist)
  checkIdentical(rdapply(params), NULL)
  params <- RDApplyParams(rd, countrows, reducerFun = unlist,
                          reducerParams = list(use.names = FALSE))
  checkIdentical(rdapply(params), c(2L, 1L))  
}
