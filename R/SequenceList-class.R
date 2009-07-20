### =========================================================================
### SequenceList object class
### -------------------------------------------------------------------------

setClass("SequenceList", representation("VIRTUAL"),
         prototype = prototype(elementType = "vector"),
         contains = "Sequence")


setMethod("seqextract", "SequenceList",
          function(x, start=NULL, end=NULL, width=NULL)
          {
              if (!missing(start) && is(start, "RangesList")) {
                  if (length(x) != length(start))
                      stop("for Ranges 'start', 'length(x) != length(start)'")
                  ans <- mendoapply(seqextract, x, start)
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
