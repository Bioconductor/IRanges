### =========================================================================
### IntervalTree utilities
### -------------------------------------------------------------------------

setMethod("findOverlaps", c("Ranges", "IntervalTree"),
          function(query, subject, maxgap = 0, multiple = TRUE)
          {
            query <- as(query, "IRanges")
            query_ord <- NULL
            if (is.unsorted(start(query))) { ## query must be sorted
              query_ord <- order(query)
              query <- query[query_ord]
            }
            if (!isTRUEorFALSE(multiple))
              stop("'multiple' must be logical of length 1")
            if (maxgap != 0) {
              if (!isSingleNumber(maxgap) || maxgap < 0)
                stop("'maxgap' must be a single, non-negative, non-NA number")
              ## Another option would be to do
              ##   query <- unsafe.update(start = start(query) - maxgap,
              ##                          end = end(query) + maxgap)
              ## instead of the 2 lines below. More readable? (but maybe not as
              ## efficient)
              query <- shift(query, -maxgap)
              width(query) <- width(query) + 2*maxgap # adds to end (weird...)
            }
            fun <- "overlap"
            if (multiple)
              fun <- paste(fun, "_multiple", sep = "")
            validObject(query)
            result <- .IntervalTreeCall(subject, fun, query)
            if (!is.null(query_ord)) {
              if (multiple) {
                mat <- matchMatrix(result)
                mat[,1] <- query_ord[mat[,1]]
                result@matchMatrix <- mat
              } else {
                query_rev_ord <- integer(length(query_ord))
                query_rev_ord[query_ord] <- seq_along(query_ord)
                result <- result[query_rev_ord]
              }
            }
            result
          })

setMethod("findOverlaps", c("Ranges", "Ranges"),
          function(query, subject, maxgap = 0, multiple = TRUE) {
            findOverlaps(query, IntervalTree(subject), maxgap, multiple)
          })

setMethod("findOverlaps", c("Ranges", "missing"),
          function(query, subject, maxgap = 0, multiple = TRUE)
          {
            result <- findOverlaps(query, query, maxgap, TRUE)
            ### FIXME: perhaps support a "simplify" option that does this:
            ## mat <- matchMatrix(result)            
            ## mat <- mat[mat[,1] != mat[,2],]
            ## norm_mat <- cbind(pmin(mat[,1], mat[,2]), pmax(mat[,1], mat[,2]))
            ## mat <- mat[!duplicated(norm_mat),]
            ## result@matchMatrix <- mat
            result
          })

setMethod("findOverlaps", c("integer", "Ranges"),
          function(query, subject, maxgap = 0, multiple = TRUE)
          findOverlaps(IRanges(query, query), subject, maxgap, multiple)
          )

## not for exporting, just a debugging utility
IntervalTreeDump <- function(object) {
  .IntervalTreeCall(object, "dump")
}
