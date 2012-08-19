### =========================================================================
### IntervalTree utilities
### -------------------------------------------------------------------------

## internal generic
setGeneric("processSelfMatching",
           function(x, select = c("all", "first", "last", "arbitrary"),
                    ignoreSelf = FALSE, ignoreRedundant = FALSE)
           standardGeneric("processSelfMatching"))

setMethod("processSelfMatching", "Hits",
          function(x, select = c("all", "first", "last", "arbitrary"),
                   ignoreSelf = FALSE, ignoreRedundant = FALSE)
          {
            mat <- as.matrix(x)
            if (ignoreSelf)
              mat <- mat[mat[,1L] != mat[,2L],,drop=FALSE]
            if (ignoreRedundant) {
              norm_mat <- cbind(pmin.int(mat[,1L], mat[,2L]),
                                pmax.int(mat[,1L], mat[,2L]))
              mat <- mat[!duplicated(norm_mat),,drop=FALSE]
            }
            if (select != "all") { # relies on 'mat' sorted by subject
              if (select == "last")
                mat <- mat[seq(nrow(mat), 1),,drop=FALSE]
              .hitsMatrixToVector(mat, queryLength(x))
            } else {
              ## unname() required because in case 'm' has only 1 row
              ## 'm[ , 1L]' and 'm[ , 2L]' will return a named atomic vector
              x@queryHits <- unname(mat[ , 1L])
              x@subjectHits <- unname(mat[ , 2L])
              x
            }
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
              newList("HitsList", ans, subjectOffsets = x@subjectOffsets)
          })

## not for exporting, just a debugging utility
IntervalTreeDump <- function(object) {
  .IntervalTreeCall(object, "dump")
}
