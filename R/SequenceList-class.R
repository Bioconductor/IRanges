### =========================================================================
### SequenceList object class
### -------------------------------------------------------------------------

setClass("SequenceList", representation("VIRTUAL"),
         prototype = prototype(elementType = "vector"),
         contains = "Sequence")


setMethod("seqextract", "SequenceList",
          function(x, start=NULL, end=NULL, width=NULL)
          {
              if (!is.null(start) && is.null(end) && is.null(width) &&
                  (length(x) > 0)) {
                  if (length(x) != length(start))
                      stop("'length(start)' must equal 'length(x)' when ",
                           "'end' and 'width' are NULL")
                  if (is.list(start)) {
                      if (is.logical(start[[1]]))
                          start <- LogicalList(start)
                      else if (is.numeric(start[[1]]))
                          start <- IntegerList(start)
                  }
                  if (is(start, "RangesList") || is(start, "LogicalList") ||
                      is(start, "IntegerList")) {
                      ans <- mendoapply(seqextract, x, start)
                  } else {
                      stop("unrecognized 'start' type")
                  }
              } else {
                  ans <- callNextMethod()
              }
              ans
          })

setMethod("aggregate", "SequenceList",
          function(x, by, FUN, start = NULL, end = NULL, width = NULL,
                   frequency = NULL, delta = NULL, ..., simplify = TRUE)
          {
              if (!missing(by) && is(by, "RangesList")) {
                  if (length(x) != length(by))
                      stop("for Ranges 'by', 'length(x) != length(by)'")
                  ans <-
                    newSimpleList("SimpleList",
                                  lapply(structure(seq_len(length(x)),
                                                   names = names(x)),
                                         function(i)
                                         aggregate(x[[i]], by = by[[i]],
                                                   FUN = FUN,
                                                   frequency = frequency,
                                                   delta = delta, ...,
                                                   simplify = simplify)),
                                  metadata = metadata(x),
                                  elementMetadata = elementMetadata(x))
              } else {
                  ans <- callNextMethod()
              }
              ans
          })
