### =========================================================================
### RangesList objects
### -------------------------------------------------------------------------

## Accepts any type of Ranges instance as an element

setClass("RangesList", representation(universe = "characterORNULL"),
         prototype = prototype(elementClass = "Ranges", compressible = FALSE),
         contains = "TypedList")

setClass("IRangesList",
         prototype = prototype(elementClass = "IRanges", compressible = TRUE),
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
              space <- rep(space, elementLengths(x))
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
  ans <- TypedList("RangesList", ranges, compress = FALSE)
  ans@universe <- universe
  ans
}

IRangesList <- function(..., universe = NULL, compress = TRUE)
{
  if (!is.null(universe) && !isSingleString(universe))
    stop("'universe' must be a single string or NULL")
  ranges <- list(...)
  if (!all(sapply(ranges, is, "IRanges")))
    stop("all elements in '...' must be instances of 'IRanges'")
  ans <- TypedList("IRangesList", ranges, compress = compress)
  ans@universe <- universe
  ans
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
              els <- as.list(x)
              for (j in seq_len(length(x))) {
                els[[j]] <- els[[j]][!is.na(ol[[j]])]
              }
              ans <- do.call(class(x), els)
            } else {
              ans <- callNextMethod(x, i)
            }
            ans
          })

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Some useful endomorphisms: "reduce" and "gaps".
###

### 'with.inframe.attrib' is ignored.
setMethod("reduce", "RangesList",
          function(x, with.inframe.attrib=FALSE)
          {
            if (length(x) == 0) {
              nirl <- list(new("NormalIRanges"))
            } else {
              ranges <- new2("IRanges", start=start(x), width=width(x), check=FALSE)
              nirl <- list(asNormalIRanges(ranges, force=TRUE))
            }
            ## This transformation must be atomic.
            do.call(class(x), nirl)
          })

setMethod("gaps", "RangesList",
          function(x, start=NA, end=NA)
          {
            do.call(class(x), lapply(x, gaps, start = start, end = end))
          })

setMethod("range", "RangesList",
          function(x, ..., na.rm) {
            args <- list(x, ...)
            if (!all(sapply(sapply(args, universe), identical, universe(x))))
              stop("All args in '...' must have the same universe as 'x'")
            spaceList <- lapply(args, names)
            names <- spaces <- unique(do.call(c, spaceList))
            if (any(sapply(spaceList, is.null))) {
              if (!all(unlist(lapply(args, length)) == length(x)))
                stop("If any args are missing names, all must have same length")
              spaces <- seq_len(length(x))
            }
            ranges <- lapply(spaces, function(space) {
              r <- lapply(args, `[[`, space)
              do.call(range, r[!sapply(r, is.null)])
            })
            names(ranges) <- names
            do.call(class(x), ranges)
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
              ans <- do.call(RangesMatchingList, ans)
            ans
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
            if (length(names(x)) > 0)
              df <- cbind(space = rep(names(x), elementLengths(x)), df)
            df
          })

### From an IRangesList object to a NormalIRanges object.
setAs("IRangesList", "NormalIRanges",
      function(from) reduce(from)[[1]])

### FIXME: Not clear why this is needed
setAs("IRangesList", "list",
      function(from) as(as(from, "RangesList"), "list"))

setAs("RangesList", "IRangesList",
      function(from) {
        ir <- lapply(from, as, "IRanges")
        names(ir) <- NULL
        ans <- new("IRangesList")
        for (i in slotNames(ans))
          slot(ans, i, check = FALSE) <- slot(from, i)
        slot(ans, "elements", check = FALSE) <- ir
        ans
      })

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Splitting.
###

setMethod("split", "Ranges",
    function(x, f, drop = FALSE, ...)
    {
        do.call(RangesList, callNextMethod())
    })
