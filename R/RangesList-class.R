### =========================================================================
### RangesList objects
### -------------------------------------------------------------------------

## Accepts any type of Ranges instance as an element

setClass("RangesList", representation(universe = "characterORNULL"),
         prototype(elementClass = "Ranges"),
         contains = "TypedList")

setClass("IRangesList", prototype = prototype(elementClass = "IRanges"),
         contains = "RangesList")


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Accessor methods.
###

setMethod("start", "RangesList",
          function(x) unlist(lapply(x, start), use.names = FALSE))
setMethod("end", "RangesList",
          function(x) unlist(lapply(x, end), use.names = FALSE))
setMethod("width", "RangesList",
          function(x) unlist(lapply(x, width), use.names = FALSE))

setGeneric("space", function(x, ...) standardGeneric("space"))
setMethod("space", "RangesList",
          function(x) {
            space <- names(x)
            if (!is.null(space))
              space <- rep(space, sapply(elements(x), length))
            space
          })

setGeneric("universe", function(x) standardGeneric("universe"))
setMethod("universe", "RangesList", function(x) x@universe)

setGeneric("universe<-", function(x, value) standardGeneric("universe<-"))
setReplaceMethod("universe", "RangesList",
                 function(x, value) {
                   if (!is.null(value) && !isSingleString(value))
                     stop("'value' must be a single string or NULL")
                   x@universe <- value
                   x
                 })

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Constructor.
###

RangesList <- function(..., universe = NULL)
{
  if (!is.null(universe) && !isSingleString(universe))
    stop("'universe' must be a single string or NULL")
  ranges <- list(...)
  if (!all(sapply(ranges, is, "Ranges")))
    stop("all elements in '...' must be instances of 'Ranges'")
  NAMES <- names(ranges)
  names(ranges) <- NULL
  new("RangesList", elements=ranges, NAMES=NAMES, universe=universe)
}

IRangesList <- function(..., universe = NULL)
{
  ranges <- list(...)
  if (!all(sapply(ranges, is, "IRanges")))
    stop("all elements in '...' must be instances of 'IRanges'")
  new("IRangesList", RangesList(..., universe = universe))
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Subsetting.
###

### Support subsetting by another RangesList
setMethod("[", "RangesList",
          function(x, i, j, ..., drop)
          {
            if (!missing(j) || length(list(...)) > 0)
              stop("invalid subsetting")
            if (missing(i))
              return(x)
            if (is(i, "RangesList")) {
              ol <- overlap(i, x, multiple = FALSE)
              els <- elements(x)
              for (j in seq_len(length(x))) {
                els[[j]] <- els[[j]][!is.na(ol[[j]])]
              }
              x@elements <- els
              x
            } else callNextMethod(x, i)
          }
          )

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Methods that are vectorized over the ranges
###

setMethod("isEmpty", "RangesList",
          function(x)
          {
            if (length(x) == 0)
              return(logical(0))
            sapply(elements(x), isEmpty)
          }
          )

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Some useful endomorphisms: "reduce" and "gaps".
###

### 'with.inframe.attrib' is ignored.
setMethod("reduce", "RangesList",
          function(x, with.inframe.attrib=FALSE)
          {
            elements <- elements(x)
            if (length(elements) == 0) {
              nir1 <- new("NormalIRanges")
            } else {
              start1 <- start(x)
              width1 <- width(x)
              ranges <- new2("IRanges", start=start1, width=width1, check=FALSE)
              nir1 <- asNormalIRanges(ranges, force=TRUE)
            }
            ## This transformation must be atomic.
            x@elements <- list(nir1)
            x@NAMES <- NULL
            x
          }
          )

setMethod("gaps", "RangesList",
          function(x, start=NA, end=NA)
          {
            names(x) <- NULL
            x@elements <- lapply(x, function(r) gaps(r, start=start, end=end))
            x
          }
          )

setMethod("range", "RangesList",
          function(x, ..., na.rm) {
            args <- list(x, ...)
            if (!all(sapply(sapply(args, universe), identical, universe(x))))
              stop("All args in '...' must have the same universe as 'x'")
            spaceList <- lapply(args, names)
            names <- spaces <- unique(do.call("c", spaceList))
            if (any(sapply(spaceList, is.null))) {
              if (!all(sapply(args, length) == length(x)))
                stop("If any args are missing names, all must have same length")
              spaces <- seq_len(length(x))
            }
            ranges <- lapply(spaces, function(space) {
              r <- lapply(args, `[[`, space)
              do.call("range", r[!sapply(r, is.null)])
            })
            initialize(x, elements = ranges, NAMES = names)
          })

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Overlap.
###

setMethod("overlap", c("RangesList", "RangesList"),
          function(object, query, maxgap = 0, multiple = TRUE)
          {
            query <- as.list(query)
            subject <- as.list(object)
            if (!is.null(names(subject)) && !is.null(names(query)))
              subject <- subject[names(query)]
            else subject <- subject[seq_along(query)]
            ## NULL's are introduced where they do not match
            ## We replace those with empty IRanges
            subject[sapply(subject, is.null)] <- IRanges()
            ans <- lapply(seq_len(length(subject)), function(i) {
              overlap(subject[[i]], query[[i]], maxgap, multiple)
            })
            names(ans) <- names(subject)
            if (multiple)
              ans <- do.call("RangesMatchingList", ans)
            ans
          })

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion.
###

setMethod("as.data.frame", "RangesList",
          function(x, row.names=NULL, optional=FALSE, ...)
          {
            if (!(is.null(row.names) || is.character(row.names)))
              stop("'row.names'  must be NULL or a character vector")
            if (!missing(optional) || length(list(...)))
              warning("'optional' and arguments in '...' ignored")
            x <- as(x, "IRangesList")
            df <- as.data.frame(unlist(x), row.names = row.names)
            if (!is.null(names(x)))
              df <- cbind(space = rep(names(x), unlist(lapply(x, length))), df)
            df
          })

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "show" method.
###

setMethod("show", "RangesList",
          function(object)
          {
            lo <- length(object)
            cat("  A ", class(object), " instance of length ", lo, "\n", sep="")
            ### TODO: show (some of the) ranges here
          }
          )


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "summary" method.
###

setMethod("summary", "RangesList",
          function(object)
          {
            object <- as(object, "IRangesList")
            if (all(unlist(lapply(object, is, "IRanges"))))
              .Call("summary_IRangesList", object, PACKAGE="IRanges")
            else
              stop("all elements must be of class 'IRanges' ")
          })


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion.
###

setMethod("as.data.frame", "RangesList",
          function(x, row.names=NULL, optional=FALSE, ...)
          {
            if (!(is.null(row.names) || is.character(row.names)))
              stop("'row.names'  must be NULL or a character vector")
            if (!missing(optional) || length(list(...)))
              warning("'optional' and arguments in '...' ignored")
            x <- as(x, "IRangesList")
            df <- as.data.frame(unlist(x), row.names = row.names)
            if (!is.null(names(x)))
              df <- cbind(space = rep(names(x), unlist(lapply(x, length))), df)
            df
          })

setMethod("unlist", "IRangesList",
          function(x, recursive = TRUE, use.names = TRUE) {
            if (!missing(recursive))
              warning("'recursive' argument currently ignored")
            ans <- callNextMethod()
            if (is.null(ans))
              ans <- IRanges()
            ans
          })

### From an IRangesList object to a NormalIRanges object.
setAs("IRangesList", "NormalIRanges",
      function(from) reduce(from)[[1]]
      )

setAs("RangesList", "IRangesList",
      function(from) {
        ir <- lapply(from, as, "IRanges")
        names(ir) <- NULL
        new("IRangesList", from, elements = ir)
      })


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Splitting.
###

setMethod("split", "Ranges",
    function(x, f, drop = FALSE, ...)
    {
        do.call("RangesList", callNextMethod())
    }
)

