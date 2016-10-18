### =========================================================================
### Grouping objects implemented with an IntegerList
### -------------------------------------------------------------------------

setClass("SimpleGrouping",
### TODO: contain VIRTUAL after R 3.4 release
         contains=c("Grouping", "SimpleIntegerList"))

setClass("CompressedGrouping",
### TODO: contain VIRTUAL after R 3.4 release
         contains=c("Grouping", "CompressedIntegerList"))

setClass("SimpleManyToOneGrouping",
         contains=c("ManyToOneGrouping", "SimpleGrouping"))

setClass("CompressedManyToOneGrouping",
         contains=c("ManyToOneGrouping", "CompressedGrouping"))

setClass("BaseManyToManyGrouping",
         representation(nobj="integer"),
### TODO: contain VIRTUAL after R 3.4 release
         contains="ManyToManyGrouping",
         validity=function(object) {
             if (!isSingleNumber(object@nobj))
                 "'nobj' must be a single, non-NA number"
         })

setClass("SimpleManyToManyGrouping",
         contains=c("BaseManyToManyGrouping", "SimpleGrouping"))

setClass("CompressedManyToManyGrouping",
         contains=c("BaseManyToManyGrouping", "CompressedGrouping"))

### -------------------------------------------------------------------------
### Grouping API implementation
### ----------------------------
###

setMethod("grouplengths", "CompressedGrouping",
          function(x, i=NULL) grouplengths(PartitioningByEnd(x), i))

setMethod("nobj", "CompressedManyToOneGrouping",
          function(x) nobj(PartitioningByEnd(x)))

setMethod("nobj", "BaseManyToManyGrouping", function(x) x@nobj)

### -------------------------------------------------------------------------
### Constructors
### ----------------------------
###

ManyToOneGrouping <- function(..., compress=TRUE) {
    CompressedOrSimple <- if (compress) "Compressed" else "Simple"
    Class <- paste0(CompressedOrSimple, "ManyToOneGrouping")
    new(Class, IntegerList(..., compress=compress))
}

ManyToManyGrouping <- function(..., nobj, compress=TRUE) {
    CompressedOrSimple <- if (compress) "Compressed" else "Simple"
    Class <- paste0(CompressedOrSimple, "ManyToManyGrouping")
    new(Class, IntegerList(..., compress=compress), nobj=nobj)
}

### -------------------------------------------------------------------------
### Coercion
### ----------------------------
###

setOldClass("grouping")

## utils::relist dipatches only on 'skeleton' so this is here instead of in R
setMethod("relist", c("grouping", "missing"), function(flesh, skeleton) {
              relist(as.integer(flesh), PartitioningByEnd(attr(flesh, "ends")))
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
          ManyToOneGrouping(relist(from), compress=TRUE)
      })

setAs("vector", "Grouping", function(from) {
          if (anyNA(from))
              as(from, "ManyToManyGrouping")
          else as(from, "ManyToOneGrouping")
      })

setAs("vector", "ManyToOneGrouping", function(from) {
    to <- as(grouping(from), "Grouping")
    names(to) <- from[unlist(to)[end(PartitioningByEnd(to))]]
    to
})

setAs("factor", "ManyToOneGrouping", function(from) {
    ManyToOneGrouping(splitAsList(seq_along(from), from))
})

setAs("vector", "ManyToManyGrouping", function(from) {
         g <- as(from, "ManyToOneGrouping")
         if (anyNA(from))
             g <- g[-length(g)]
         ManyToManyGrouping(g, nobj=length(from))
      })

setAs("ManyToOneGrouping", "factor", function(from) {
    levels <- if (!is.null(names(from))) {
        names(from)
    } else {
        as.character(seq_along(from))
    }
    structure(togroup(from), levels=levels, class="factor")
})

setMethod("as.factor", "ManyToOneGrouping", function(x) {
    as(x, "factor")
})

makeGroupNames <- function(x) {
    if (is.null(x)) {
        x <- character(length(x))
    }
    ind <- which(x == "")
    x[ind] <- paste("Group", ind, sep = ".")
    x
}

levelCols <- function(by) {
    DataFrame(expand.grid(lapply(by, levels)))
}

setAs("FactorList", "Grouping", function(from) {
    l <- as.list(from)
    names(l) <- makeGroupNames(names(from))
    as(DataFrame(l), "Grouping")
})

setAs("DataFrame", "Grouping", function(from) {
    factors <- lapply(from, as.factor)
    l <- splitAsList(seq_len(nrow(from)), factors)
    mcols(l) <- levelCols(factors)
    if (anyNA(from, recursive=TRUE)) {
        ManyToManyGrouping(l, nobj=nrow(from))
    } else {
        ManyToOneGrouping(l)
    }
})
