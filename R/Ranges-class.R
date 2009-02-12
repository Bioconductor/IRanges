### =========================================================================
### Ranges objects
### -------------------------------------------------------------------------
###
### Ranges is a virtual class that serves as the base for all range containers
### Conceptually Ranges are closed, one-dimensional intervals with integer end
### points and on the domain of integers.
###

setClass("Ranges", contains = "VIRTUAL")


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The Ranges API (work still very much in progress):
###
###   Basic get/set methods:
###     length
###     start, width, end, names
###     start<-, width<-, end<-, names<-
###
###   More basic stuff:
###     as.matrix, as.data.frame
###     show
###
###   Uniqueness, ordering and related methods:
###     duplicated
###     unique (not implemented yet)
###     order
###     sort
###     rank (not implemented yet)
###
###   Testing a Ranges object:
###     isEmpty
###     isDisjoint
###     isNormal, whichFirstNotNormal
###
###   Core endomorphisms:
###     update
###     [, [<-, rep
###
###   More endomorphisms:
###     shift, restrict, narrow, reduce, gaps,
###     reflect (currently not an endomorphism),
###     flank (currently not an endomorphism)
###
###   More operations:
###     threebands,
###     split
###     overlap
###     (some are missing, list them all here)
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

setMethod("length", "Ranges", function(x) length(start(x)))

setGeneric("start<-", signature="x",
    function(x, check=TRUE, value) standardGeneric("start<-")
)

setGeneric("width<-", signature="x",
    function(x, check=TRUE, value) standardGeneric("width<-")
)

setGeneric("end<-", signature="x",
    function(x, check=TRUE, value) standardGeneric("end<-")
)

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

setMethod("show", "Ranges",
    function(object)
    {
        cat(class(object), " object:\n", sep="")
        show(as.data.frame(object))
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Uniqueness, ordering and related methods.
###

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

### We want to dispatch on ... (only possible starting with R 2.8.0).
### The implicit generic in package "base" would dispatch on the (na.last,
### decreasing) arguments which is of course not what we want.
setGeneric("order", signature="...",
           function (..., na.last=TRUE, decreasing=FALSE)
           standardGeneric("order"))

setMethod("order", "Ranges", function (..., na.last = TRUE, decreasing = FALSE)
          {
            if (!is.logical(na.last) || length(na.last) != 1)
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


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Testing a Ranges object.
###

### A Ranges object is considered empty iff all its ranges are empty.
setGeneric("isEmpty", function(x) standardGeneric("isEmpty"))

setMethod("isEmpty", "Ranges", function(x) all(width(x) == 0))

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


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Core endomorphisms.
###

setReplaceMethod("[", "Ranges",
    function(x, i, j,..., value)
        stop("attempt to modify the value of a ", class(x), " instance")
)

setMethod("rep", "Ranges",
    function(x, ...)
        x[rep(seq_len(length(x)), ...)]
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### More endomorphisms.
###

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

setGeneric("threebands", signature="x",
    function(x, start=NA, end=NA, width=NA)
        standardGeneric("threebands")
)

setGeneric("reduce", signature="x",
    function(x, with.inframe.attrib=FALSE) standardGeneric("reduce")
)

setGeneric("gaps", signature="x",
    function(x, start=NA, end=NA) standardGeneric("gaps")
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "reflect" generic and method (should be made an endomorphism).
###
### |   xxx |
### to
### | xxx   |
###
### NOTE for Michael (HP-01/28/09): "reflect" and "flank" are listed in the
### endomorphism section of The Ranges API (at the top of this file) but they
### are not implemented as such (you're calling the IRanges() constructor).
### So for example "flank" called on a Views object won't return a Views
### object, which is sad ;-)
### TODO: Make it an endomorphism and move to Ranges-endo.R

setGeneric("reflect", function(x, ...) standardGeneric("reflect"))

setMethod("reflect", "Ranges", function(x, bounds) {
  if (!is(bounds, "Ranges") || length(bounds) != length(x))
    stop("'bounds' must be a Ranges instance of length equal to that of 'x'")
  IRanges(end(bounds) - (end(x) - start(bounds)), width = width(x))
})


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "flank" generic and method (should be made an endomorphism).
###
### TODO: Make it an endomorphism and move to Ranges-endo.R

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


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### More operations.
###

setMethod("range", "Ranges", function(x, ..., na.rm) {
  args <- list(x, ...)
  if (!all(sapply(args, is, "Ranges")))
    stop("all arguments in '...' must be Ranges instances")
  x <- do.call(c, args)
  if (!length(x))
    IRanges()
  else IRanges(min(start(x)), max(end(x)))
})

### Find objects in the index that overlap those in a query set.
setGeneric("overlap", signature = c("object", "query"),
    function(object, query, maxgap = 0, multiple = TRUE, ...)
        standardGeneric("overlap")
)

setMethod("%in%", c("Ranges", "Ranges"),
          function(x, table)
          !is.na(overlap(reduce(table), x, multiple = FALSE)))

setClassUnion("RangesORmissing", c("Ranges", "missing"))

setGeneric("precede", function(x, subject = x, ...) standardGeneric("precede"))

setMethod("precede", c("Ranges", "RangesORmissing"), function(x, subject) {
  s <- start(subject)
  ord <- NULL
  if (is.unsorted(s)) {
    ord <- order(s)
    s <- s[ord]
  }
  i <- findInterval(end(x), s) + 1L
  if (!is.null(ord)) {
    invord <- integer(length(ord))
    invord[ord] <- seq_along(ord)
    i <- invord[i]
  }
  i[i > length(subject)] <- NA
  i
})

setGeneric("follow", function(x, subject = x, ...) standardGeneric("follow"))

setMethod("follow", c("Ranges", "RangesORmissing"), function(x, subject) {
  e <- end(subject)
  ord <- NULL
  if (is.unsorted(e)) {
    ord <- order(e)
    e <- e[ord]
  }
  i <- findInterval(start(x) - 1L, e)
  i[i == 0] <- NA
  if (!is.null(ord)) {
    invord <- integer(length(ord))
    invord[ord] <- seq_along(ord)
    i <- invord[i]
  }
  i
})

setGeneric("nearest",
           function(x, subject, ...) standardGeneric("nearest"))

setMethod("nearest", c("Ranges", "RangesORmissing"), function(x, subject) {
  if (!missing(subject))
    ol <- overlap(subject, x, multiple = FALSE)
  else { ## avoid overlapping with self
    subject <- x
    olm <- as.matrix(overlap(subject, x))
    olm <- olm[olm[,1] != olm[,2],]
    ol <- olm[,2][match(seq_len(length(subject)), olm[,1])]
  }
  x <- x[is.na(ol)]
  before <- precede(x, subject)
  after <- follow(x, subject)
  pre <- (start(subject)[before] - end(x)) < (start(x) - end(subject)[after])
  pre[is.na(pre)] <- is.na(after)[is.na(pre)]
  ol[is.na(ol)] <- ifelse(pre, before, after)
  ol
})

## zooming (symmetrically scales the width)
setMethod("*", c("Ranges", "numeric"), function(e1, e2) {
  if (any(is.na(e2)))
    stop("NA not allowed as zoom factor")
  if ((length(e1) < length(e2) && length(e1)) || (length(e1) && !length(e2)) ||
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

