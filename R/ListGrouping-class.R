### =========================================================================
### Grouping objects implemented with an IntegerList
### -------------------------------------------------------------------------

setClass("SimpleGrouping",
         contains=c("Grouping", "SimpleIntegerList", "VIRTUAL"))

setClass("CompressedGrouping",
         contains=c("Grouping", "CompressedIntegerList", "VIRTUAL"))

setClass("SimpleManyToOneGrouping",
         contains=c("ManyToOneGrouping", "SimpleGrouping"))

setClass("CompressedManyToOneGrouping",
         contains=c("ManyToOneGrouping", "CompressedGrouping"))

### -------------------------------------------------------------------------
### Grouping API implementation
### ----------------------------
###

setMethod("grouplengths", "CompressedGrouping",
          function(x, i=NULL) grouplengths(PartitioningByEnd(x), i))

setMethod("nobj", "CompressedManyToOneGrouping",
          function(x) nobj(PartitioningByEnd(x)))

### -------------------------------------------------------------------------
### Constructors
### ----------------------------
###

ManyToOneGrouping <- function(x, compress=TRUE) {
    CompressedOrSimple <- if (compress) "Compressed" else "Simple"
    Class <- paste0(CompressedOrSimple, "ManyToOneGrouping")
    new(Class, IntegerList(x, compress=compress))
}

### -------------------------------------------------------------------------
### Coercion
### ----------------------------
###

setOldClass("grouping")

## utils::relist dipatches only on 'skeleton' so this is here instead of in R
setMethod("relist", c("grouping", "missing"), function(flesh, skeleton) {
              relist(flesh, PartitioningByEnd(attr(flesh, "ends")))
          })

setMethod("split", c("ANY", "ManyToOneGrouping"), function(x, f, drop=FALSE) {
              stopifnot(isTRUEorFALSE(drop))
              ans <- extractList(x, f)
              if (drop) {
                  ans <- ans[lengths(ans) > 0L]
              }
              ans
          })

setAs("grouping", "Grouping", function(from) {
          as(from, "ManyToOneGrouping")
      })

setAs("grouping", "ManyToOneGrouping", function(from) {
          CompressedManyToOneGrouping(relist(from))
      })

setAs("factor", "Grouping", function(from) {
          as(from, "ManyToOneGrouping")
      })

setAs("factor", "ManyToOneGrouping", function(from) {
          as(grouping(from), "Grouping")
      })

setAs("ManyToOneGrouping", "factor", function(from) {
          structure(togroup(from), levels=seq_along(from),
                    class="factor")
      })
