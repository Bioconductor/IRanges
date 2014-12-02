### =========================================================================
### IntervalTree utilities
### -------------------------------------------------------------------------

## internal generic
setGeneric("processSelfMatching",  # not exported
           function(x, select = c("all", "first", "last", "arbitrary"),
                    ignoreSelf = FALSE, ignoreRedundant = FALSE)
           standardGeneric("processSelfMatching"))

setMethod("processSelfMatching", "Hits",
          function(x, select = c("all", "first", "last", "arbitrary"),
                   ignoreSelf = FALSE, ignoreRedundant = FALSE)
          {
            if (ignoreSelf) {
              self_idx <- which(queryHits(x) == subjectHits(x))
              if (length(self_idx) != 0L)
                  x <- x[-self_idx]
            }
            if (ignoreRedundant) {
              redundant_idx <- which(S4Vectors:::duplicatedIntegerPairs(
                                       pmin.int(queryHits(x), subjectHits(x)),
                                       pmax.int(queryHits(x), subjectHits(x))))
              if (length(redundant_idx) != 0L)
                  x <- x[-redundant_idx]
            }
            selectHits(x, select=select)
          })

setMethod("processSelfMatching", "HitsList",
          function(x, select = c("all", "first", "last", "arbitrary"),
                   ignoreSelf = FALSE, ignoreRedundant = FALSE)
          {
            select <- match.arg(select)
            ans <- lapply(x, processSelfMatching, select, ignoreSelf,
                          ignoreRedundant)
            if (select != "all")
              IntegerList(ans)
            else
              S4Vectors:::new_SimpleList_from_list("HitsList",
                                        ans,
                                        subjectOffsets = x@subjectOffsets)
          })

setMethod("processSelfMatching", "CompressedHitsList",
          function(x, select = c("all", "first", "last", "arbitrary"),
                   ignoreSelf = FALSE, ignoreRedundant = FALSE)
          {
            select <- match.arg(select)
            ans <- processSelfMatching(x@unlistData, select = select, 
                          ignoreSelf = ignoreSelf, ignoreRedundant = ignoreRedundant)

            if (select != "all")
              new2("CompressedIntegerList", unlistData=ans, partitioning=x@partitioning)
            else
              new2("CompressedHitsList", unlistData=ans, partitioning=x@partitioning)
          })

          
## not for exporting, just a debugging utility
IntervalTreeDump <- function(object) {
  IntervalTreeCall(object, "dump")
}
