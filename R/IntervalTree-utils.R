### =========================================================================
### IntervalTree utilities
### -------------------------------------------------------------------------

setMethod("overlap", c("IntervalTree", "Ranges"),
          function(object, query, maxgap = 0, multiple = TRUE)
          {
            query <- as(query, "IRanges")
            if (is.unsorted(start(query))) { ## query must be sorted
              stop("query must be in sorted order by start position")
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
            result <- .IntervalTreeCall(object, fun, query)
            ## if (multiple) {
            ##   mm <- matchMatrix(result)
            ##   if (is(mm, "ngCMatrix")) {
            ##     iord <- order(mm@i)
            ##     p <- rep(seq_len(ncol(mm)), diff(mm@p))
            ##     mm@i <- mm@i[iord][order(p[iord])]
            ##     result@matchMatrix <- mm
            ##   }
            ## }
            result
          })

setMethod("overlap", c("Ranges", "Ranges"),
          function(object, query, maxgap = 0, multiple = TRUE) {
            overlap(IntervalTree(object), query, maxgap, multiple)
          })

setMethod("overlap", c("Ranges", "missing"),
    function(object, query, maxgap = 0, multiple = TRUE)
        overlap(object, object, maxgap, multiple)
)

setMethod("overlap", c("Ranges", "integer"),
          function(object, query, maxgap = 0, multiple = TRUE)
          overlap(object, IRanges(query, query), maxgap, multiple)
          )

## not for exporting, just a debugging utility
IntervalTreeDump <- function(object) {
  .IntervalTreeCall(object, "dump")
}
