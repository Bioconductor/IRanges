### =========================================================================
### HitsList objects
### -------------------------------------------------------------------------

### FIXME: Rename this class SimpleHitsList and make HitsList a virtual
### class that SimpleHitsList and CompressedHitsList extend directly.
setClass("HitsList",
    contains="SimpleList",
    representation(
        subjectOffsets="integer"
    ),
    prototype(elementType="Hits")
)

setClass("CompressedHitsList",
    prototype = prototype(elementType = "Hits",
                          unlistData = new("Hits")),
    contains="CompressedList")

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Accessors
###

setMethod("space", "HitsList",
          function(x) {
            space <- names(x)
            if (!is.null(space))
              space <-
                rep.int(space, sapply(as.list(x, use.names = FALSE), length))
            space
          })

setMethod("subjectHits", "HitsList", function(x) {
  as.matrix(x)[,2L,drop=TRUE]
})

setMethod("subjectHits", "CompressedHitsList", function(x) subjectHits(x@unlistData))

setMethod("queryHits", "HitsList", function(x) {
  as.matrix(x)[,1L,drop=TRUE]
})
setMethod("queryHits", "CompressedHitsList", function(x) queryHits(x@unlistData))

setMethod("queryLength", "CompressedHitsList", function(x) queryLength(x@unlistData))
setMethod("subjectLength", "CompressedHitsList", function(x) subjectLength(x@unlistData))

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Constructor
###

HitsList <- function(list_of_hits, subject)
{
  subjectOffsets <- c(0L, head(cumsum(sapply(subject, length)), -1))
  subjectToQuery <- seq_along(list_of_hits)
  if (!is.null(names(list_of_hits)) && !is.null(names(subject)))
    subjectToQuery <- match(names(list_of_hits), names(subject))
  subjectOffsets <- subjectOffsets[subjectToQuery]
  S4Vectors:::new_SimpleList_from_list("HitsList", list_of_hits,
                                       subjectOffsets = subjectOffsets)
}

CompressedHitsList <- function(hits, query)
{
  if (!(is(query, "CompressedIRangesList")))
    stop("'query' must be a 'CompressedIRangesList' object")
  if (!is(hits, "Hits"))
    stop("'hits' must be a 'Hits' object")

  qspace <- space(query)
  hspace <- as.integer(qspace[queryHits(hits)])
  partitioning <- PartitioningByEnd(hspace, names=names(query@partitioning), NG=length(names(query@partitioning)))
  newCompressedList0("CompressedHitsList", unlistData=hits, partitioning=partitioning)
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Going from Hits to HitsList with extractList() and family.
###

setMethod("relistToClass", "Hits", function(x) "HitsList")


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion
###

## return as.matrix as on Hits, with indices adjusted

setMethod("as.matrix", "HitsList", function(x) {
  mats <- lapply(x, as.matrix)
  mat <- do.call(rbind, mats)
  rows <- c(0L, head(cumsum(sapply(x, queryLength)), -1))
  nr <- sapply(mats, nrow)
  mat + cbind(rep.int(rows, nr), rep.int(x@subjectOffsets, nr))
})

setMethod("as.matrix", "CompressedHitsList", function(x) {
  cbind(queryHits=queryHits(x), subjectHits=subjectHits(x))
})

## count up the matches for each query in every matching

setMethod("as.table", "HitsList", function(x, ...) {
  counts <- unlist(lapply(x, as.table))
  as.table(array(counts, length(counts), list(range = seq_along(counts))))
})

setMethod("t", "HitsList", function(x) {
  x@elements <- lapply(as.list(x, use.names = FALSE), t)
  x
})

setMethod("ranges", "HitsList", function(x, query, subject) {
  if (!is(query, "RangesList") || length(query) != length(x))
    stop("'query' must be a RangesList of length equal to that of 'x'")
  if (!is(subject, "RangesList") || length(subject) != length(x))
    stop("'subject' must be a RangesList of length equal to that of 'x'")
  els <- as.list(x, use.names = FALSE)
  queries <- as.list(query, use.names = FALSE)
  subjects <- as.list(subject, use.names = FALSE)
  ans <- do.call(RangesList, lapply(seq_len(length(x)), function(i) {
    ranges(els[[i]], queries[[i]], subjects[[i]])
  }))
  names(ans) <- names(x)
  ans
})

### TODO: many convenience methods
