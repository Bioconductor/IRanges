### =========================================================================
### ListGrouping objects
### -------------------------------------------------------------------------
###
### Groupings implemented with a List
###

setClass("ListGrouping", contains="Grouping")

setClass("ListManyToOneGrouping",
         contains=c("ListGrouping", "ManyToOneGrouping"))

### -------------------------------------------------------------------------
### Simple(ManyToOne)Grouping and Compressed(ManyToOne)Grouping
### ----------------------------
###

setClass("SimpleGrouping",
         contains=c("ListGrouping", "SimpleIntegerList"))

setClass("CompressedGrouping",
         contains=c("ListGrouping", "CompressedIntegerList"))

setClass("SimpleManyToOneGrouping",
         contains=c("ListManyToOneGrouping", "SimpleGrouping"))

setClass("CompressedManyToOneGrouping",
         contains=c("ListManyToOneGrouping", "CompressedGrouping"))

### -------------------------------------------------------------------------
### Grouping API implementation
### ----------------------------
###

setMethod("grouplength", "CompressedGrouping",
          function(x, i=NULL) grouplength(PartitioningByEnd(x), i))

setMethod("nobj", "CompressedManyToOneGrouping",
          function(x) nobj(PartitioningByEnd(x)))

setMethods("togroup",
           list("SimpleManyToOneGrouping", "CompressedManyToOneGrouping"),
           function(x, j=NULL) {
               g <- callNextMethod(x)
               if (!is.null(j)) {
                   x <- x[j]
               }
               ans <- unlist(x, use.names=FALSE)
               ans[ans] <- g
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
