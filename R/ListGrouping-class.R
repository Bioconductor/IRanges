### =========================================================================
### Grouping objects implemented with an IntegerList
### -------------------------------------------------------------------------

setClass("SimpleGrouping",
         contains=c("Grouping", "SimpleIntegerList"))

setClass("CompressedGrouping",
         contains=c("Grouping", "CompressedIntegerList"))

setClass("SimpleManyToOneGrouping",
         contains=c("ManyToOneGrouping", "SimpleGrouping"))

setClass("CompressedManyToOneGrouping",
         contains=c("ManyToOneGrouping", "CompressedGrouping"))

### -------------------------------------------------------------------------
### Grouping API implementation
### ----------------------------
###

setMethod("grouplength", "CompressedGrouping",
          function(x, i=NULL) grouplength(PartitioningByEnd(x), i))

setMethod("nobj", "CompressedManyToOneGrouping",
          function(x) nobj(PartitioningByEnd(x)))

setMethods("togroup",
           list("SimpleGrouping", "CompressedGrouping"),
           function(x, j=NULL) {
               g <- callNextMethod(x)
               ans <- unlist(x, use.names=FALSE)
               ans[ans] <- g
               if (!is.null(j)) {
                   ans <- ans[j]
               }
               ans
           })

### -------------------------------------------------------------------------
### Constructors
### ----------------------------
###

SimpleGrouping <- function(x) {
    new("SimpleGrouping", IntegerList(x, compress=FALSE))
}

CompressedGrouping <- function(x) {
    new("CompressedGrouping", IntegerList(x, compress=TRUE))
}

SimpleManyToOneGrouping <- function(x) {
    new("SimpleManyToOneGrouping", IntegerList(x, compress=FALSE))
}

CompressedManyToOneGrouping <- function(x) {
    new("CompressedManyToOneGrouping", IntegerList(x, compress=TRUE))
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
              if (drop) {
                  f <- f[grouplength(f) > 0L]
              }
              extractList(x, f)
          })

setAs("grouping", "Grouping", function(from) {
          CompressedGrouping(relist(from))
      })

setAs("grouping", "ManyToOneGrouping", function(from) {
          CompressedManyToOneGrouping(relist(from))
      })

setAs("factor", "Grouping", function(from) {
          as(grouping(from), "Grouping")
      })

setAs("ManyToOneGrouping", "factor", function(from) {
          structure(togroup(from), levels=seq_along(from),
                    class="factor")
      })
