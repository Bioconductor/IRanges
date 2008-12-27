### Ranges is a virtual class that serves as the base for all range containers
### Conceptually Ranges are closed, one-dimensional intervals with integer end
### points and on the domain of integers.

setClass("Ranges", contains = "VIRTUAL")


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The Ranges API (work in progress):
###
###   Basic get/set methods:
###     length
###     start, width, end, names
###     start<-, width<-, end<-, names<-
###
###   More basic stuff:
###     isEmpty
###     as.matrix, as.data.frame
###     duplicated
###     show
###
###   Endomorphisms:
###     update
###     [, [<-, rep
###     shift, restrict, narrow, reduce, gaps, reflect, flank
###
###   More operations:
###     c
###     split
###     overlap
###
###   Normality and operations on sets:
###     isNormal, whichFirstNotNormal
###     union, intersect, setdiff
###
### Note that, except for some default methods provided below (and implemented
### as formal algorithms), Ranges subclasses need to implement their own
### methods.
###

### The "start" and "end" generics are defined in the stats package.
setGeneric("width", function(x) standardGeneric("width"))

### The 3 default methods below provide a formalization of the relationship
### between the starts/widths/ends of a Ranges object. Of course Ranges
### subclasses need to implement at least 2 of them!
### Note that when width(x)[i] is 0, then end(x)[i] is start(x)[i] - 1
setMethod("start", "Ranges", function(x, ...) {end(x) - width(x) + 1L})
setMethod("width", "Ranges", function(x) {end(x) - start(x) + 1L})
setMethod("end", "Ranges", function(x, ...) {start(x) + width(x) - 1L})

setGeneric("start<-", signature="x",
    function(x, check=TRUE, value) standardGeneric("start<-")
)

setGeneric("width<-", signature="x",
    function(x, check=TRUE, value) standardGeneric("width<-")
)

setGeneric("end<-", signature="x",
    function(x, check=TRUE, value) standardGeneric("end<-")
)

### A Ranges object is considered empty iff all its ranges are empty.
setGeneric("isEmpty", function(x) standardGeneric("isEmpty"))
setMethod("isEmpty", "Ranges", function(x) all(width(x) == 0))

setMethod("as.matrix", "Ranges",
    function(x, ...)
        matrix(data=c(start(x), width(x)), ncol=2, dimnames=list(names(x), NULL))
)

setMethod("as.data.frame", "Ranges",
    function(x, row.names=NULL, optional=FALSE, ...)
    {
        if (!(is.null(row.names) || is.character(row.names)))
            stop("'row.names'  must be NULL or a character vector")
        ans <- data.frame(start=start(x),
                          end=end(x),
                          width=width(x),
                          row.names=row.names,
                          check.rows=TRUE,
                          check.names=FALSE,
                          stringsAsFactors=FALSE)
        ans$names <- names(x)
        ans
    }
)

### Note that this default method is very inefficient so efficient methods for
### the Ranges subclasses need to be implemented.
setMethod("duplicated", "Ranges",
    function(x, incomparables=FALSE, ...)
    {
        duplicated(data.frame(start=start(x),
                              width=width(x),
                              check.names=FALSE,
                              stringsAsFactors=FALSE))
    }
)

setGeneric("order", signature="...",
           function (..., na.last=TRUE, decreasing=FALSE)
           standardGeneric("order"))

setMethod("order", "Ranges", function (..., na.last = TRUE, decreasing = FALSE)
          {
            if (!is.na(na.last) && !isTRUEorFALSE(na.last))
              stop("'na.last' must be TRUE, FALSE or NA")
            if (!isTRUEorFALSE(decreasing)) 
              stop("'decreasing' must be TRUE or FALSE")
            args <- list(...)
            if (!all(sapply(args, is, "Ranges")))
              stop("all arguments in '...' must be Ranges instances")
            starts <- lapply(args, start)
            do.call(order,
                    c(starts, na.last = na.last, decreasing = decreasing))
          })

setMethod("sort", "Ranges", function (x, decreasing = FALSE, ...) 
          {
            if (!isTRUEorFALSE(decreasing))
              stop("'decreasing' must be TRUE or FALSE")
            x[order(x, decreasing = decreasing)]
          })

setMethod("range", "Ranges", function(x, ..., na.rm) {
  args <- list(x, ...)
  if (!all(sapply(args, is, "Ranges")))
    stop("all arguments in '...' must be Ranges instances")
  x <- do.call(c, args)
  if (!length(x))
    IRanges()
  else IRanges(min(start(x)), max(end(x)))
})

setMethod("show", "Ranges",
    function(object)
    {
        cat(class(object), " object:\n", sep="")
        show(as.data.frame(object))
    }
)

setReplaceMethod("[", "Ranges",
    function(x, i, j,..., value)
        stop("attempt to modify the value of a ", class(x), " instance")
)

setMethod("rep", "Ranges",
    function(x, ...)
        x[rep(seq_len(length(x)), ...)]
)

setGeneric("shift", signature="x",
    function(x, shift, use.names=TRUE) standardGeneric("shift")
)

setGeneric("restrict", signature="x",
    function(x, start=NA, end=NA, keep.all.ranges=FALSE, use.names=TRUE)
        standardGeneric("restrict")
)

setGeneric("narrow", signature="x",
    function(x, start=NA, end=NA, width=NA, use.names=TRUE)
        standardGeneric("narrow")
)

setGeneric("reduce", signature="x",
    function(x, with.inframe.attrib=FALSE) standardGeneric("reduce")
)

setGeneric("gaps", signature="x",
    function(x, start=NA, end=NA) standardGeneric("gaps")
)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "reflect" method.
###
### |   xxx |
### to
### | xxx   |
###

setGeneric("reflect",
    function(x, ...) standardGeneric("reflect")
)

setMethod("reflect", "Ranges", function(x, bounds) {
  if (!is(bounds, "Ranges") || length(bounds) != length(x))
    stop("'bounds' must be a Ranges instance of length equal to that of 'x'")
  IRanges(end(bounds) - (end(x) - start(bounds)), width = width(x))
})

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "flank" method.
###

setGeneric("flank", function(x, ...) standardGeneric("flank"))
setMethod("flank", "Ranges", function(x, width, start = TRUE, both = FALSE) {
  if (!is.numeric(width))
    stop("'width' must be numeric")
  if (!is.logical(start) || any(is.na(start)))
    stop("'start' must be logical without NA's")
  if (!isTRUEorFALSE(both))
    stop("'both' must be TRUE or FALSE")
  start <- recycleVector(start, length(x))
  width <- recycleVector(width, length(x))
  if (both)
    IRanges(ifelse(start, start(x) - abs(width), end(x) - abs(width) + 1),
            width = abs(width)*2)
  else IRanges(ifelse(start, ifelse(width < 0, start(x), start(x) - width),
                      ifelse(width < 0, end(x) + width + 1, end(x) + 1)),
               width = abs(width))
})

## Find objects in the index that overlap those in a query set
setGeneric("overlap", signature = c("object", "query"),
    function(object, query, maxgap = 0, multiple = TRUE, ...)
        standardGeneric("overlap")
)

setMethod("overlap", c("Ranges", "missing"),
    function(object, query, maxgap = 0, multiple = TRUE)
        overlap(object, object, maxgap, multiple)
)

setMethod("overlap", c("Ranges", "integer"),
          function(object, query, maxgap = 0, multiple = TRUE)
          overlap(object, IRanges(query, query), maxgap, multiple)
          )

setMethod("%in%", c("Ranges", "Ranges"),
          function(x, table)
          !is.na(overlap(reduce(table), x, multiple = FALSE)))

setGeneric("isNormal", function(x) standardGeneric("isNormal"))

setMethod("isNormal", "Ranges",
    function(x)
    {
        all_ok <- all(width(x) >= 1)
        if (length(x) >= 2)
            all_ok <- all_ok && all(start(x)[-1] - end(x)[-length(x)] >= 2)
        all_ok
    }
)

setGeneric("isDisjoint", function(x) standardGeneric("isDisjoint"))

setMethod("isDisjoint", "Ranges",
          function(x) {
            x <- x[width(x) > 0]
            if (length(x) < 2)
              return(TRUE)
            starts <- start(x)
            startord <- order(starts)
            all(starts[startord][-1] - end(x)[startord][-length(x)] >= 1)
          })

setGeneric("whichFirstNotNormal", function(x) standardGeneric("whichFirstNotNormal"))

setMethod("whichFirstNotNormal", "Ranges",
    function(x)
    {
        is_ok <- width(x) >= 1
        if (length(x) >= 2)
            is_ok <- is_ok & c(TRUE, start(x)[-1] - end(x)[-length(x)] >= 2)
        which(!is_ok)[1]
    }
)

### union(), intersect() and setdiff() are not endomorphisms.
setMethod("union", c("Ranges", "Ranges"),
    function(x, y)
    {
        z <- reduce(c(x, y))
        z[width(z) != 0]
    }
)
setMethod("intersect", c("Ranges", "Ranges"),
    function(x, y)
    {
        start <- min(c(start(x), start(y)))
        end <- max(c(end(x), end(y)))
        setdiff(x, gaps(y, start=start, end=end))
    }
)
setMethod("setdiff", c("Ranges", "Ranges"),
    function(x, y)
    {
        start <- min(c(start(x), start(y)))
        end <- max(c(end(x), end(y)))
        gaps(union(gaps(x, start=start, end=end), y), start=start, end=end)
    }
)

## zooming (symmetrically scales the width)
setMethod("*", c("Ranges", "numeric"), function(e1, e2) {
  if (any(is.na(e2)))
    stop("NA not allowed as zoom factor")
  if (length(e1) < length(e2) || (length(e1) && !length(e2)) ||
      length(e1) %% length(e2) != 0)
    stop("zoom factor length not a multiple of number of ranges")
  e2 <- ifelse(e2 < 0, abs(1/e2), e2)
  r <- e1
  mid <- (start(r)+end(r))/2
  w <- width(r)/e2
  start(r) <- ceiling(mid - w/2)
  width(r) <- floor(w)
  r
})

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Old stuff (Defunct or Deprecated).
###

setGeneric("first", function(x) standardGeneric("first"))
setMethod("first", "Ranges", function(x) {.Defunct("start"); start(x)})
setGeneric("last", function(x) standardGeneric("last"))
setMethod("last", "Ranges", function(x) {.Defunct("end"); end(x)})

