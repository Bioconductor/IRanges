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

setGeneric("mid", function(x, ...) standardGeneric("mid"))
setMethod("mid", "Ranges", function(x) start(x) + as.integer((width(x)-1) / 2))

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
            stop("'row.names' must be NULL or a character vector")
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

setMethod("as.integer", "Ranges", function(x) {
  x <- x[width(x) > 0]
  mseq(start(x), end(x))
})

setMethod("show", "Ranges",
    function(object)
    {
        cat(class(object), " object:\n", sep="")
        n <- length(object)
        if (n == 0) {
            showme <- as.data.frame(object)
        } else if (n < 20) {
            showme <- as.data.frame(object, row.names=paste("[", seq_len(n), "]", sep=""))
        } else {
            sketch <- function(x)
                c(subseq(x, 1, 9), "...", subseq(x, length(x)-8, length(x)))
            showme <-
                  data.frame(start=sketch(start(object)),
                             end=sketch(end(object)),
                             width=sketch(width(object)),
                             row.names=c(paste("[", 1:9, "]", sep=""), "...",
                                         paste("[", (n-8):n, "]", sep="")),
                             check.rows=TRUE,
                             check.names=FALSE,
                             stringsAsFactors=FALSE)
            NAMES <- names(object)
            if (!is.null(NAMES))
                showme$names <- sketch(NAMES)
        }
        show(showme)
    }
)


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
    stop("'bounds' must be a Ranges object of length equal to that of 'x'")
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

## Question: should these be IRanges methods since they always return
## an IRanges? Or should they use initialize() or some type of
## coercion to become endomorphisms?

setMethod("range", "Ranges", function(x, ..., na.rm) {
  args <- list(x, ...)
  if (!all(sapply(args, is, "Ranges")))
    stop("all arguments in '...' must be Ranges objects")
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

## make intervals disjoint by taking the union of the endpoints
setGeneric("disjoin", function(x, ...) standardGeneric("disjoin"))
setMethod("disjoin", "Ranges", function(x) {
  ## starts: original starts and end+1 when inside another interval
  ## ends: original ends and start-1 when inside another interval
  starts <- unique(start(x))
  ends <- unique(end(x))
  adj_start <- sort(unique(c(starts, ends + 1)))
  adj_end <- sort(unique(c(ends, starts - 1)))
  adj <- IRanges(head(adj_start, -1), tail(adj_end, -1))
  adj[adj %in% x]
})

## make intervals disjoint by segregating them into separate Ranges
setGeneric("disjointBins", function(x, ...) standardGeneric("disjointBins"))
setMethod("disjointBins", "Ranges", function(x) {
  x_ord <- NULL
  if (is.unsorted(start(x))) { # minimize work for sorted ranges (common)
    x_ord <- order(x)
    x <- x[x_ord]
  }
  bins <- .Call("Ranges_disjointBins", start(x), width(x), PACKAGE="IRanges")
  if (!is.null(x_ord)) {
    rev_ord <- integer(length(x_ord))
    rev_ord[x_ord] <- seq_along(rev_ord)
    bins <- bins[rev_ord]
  }
  names(bins) <- names(x)
  bins
})

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Old stuff (Defunct or Deprecated).
###

setGeneric("first", function(x) standardGeneric("first"))
setMethod("first", "Ranges", function(x) {.Defunct("start"); start(x)})
setGeneric("last", function(x) standardGeneric("last"))
setMethod("last", "Ranges", function(x) {.Defunct("end"); end(x)})

