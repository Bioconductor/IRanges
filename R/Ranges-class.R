### =========================================================================
### Ranges objects
### -------------------------------------------------------------------------
###
### Ranges is a virtual class that serves as the base for all range containers
### Conceptually Ranges are closed, one-dimensional intervals with integer end
### points and on the domain of integers.
###

setClass("Ranges", contains="IntegerList", representation("VIRTUAL"))


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
###     as.integer, unlist
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
###     findOverlaps
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

setMethod("update", "Ranges",
    function(object, ...)
        as(update(as(object, "IRanges"), ...), class(object))
)

setMethod("as.matrix", "Ranges",
    function(x, ...)
        matrix(data=c(start(x), width(x)), ncol=2,
               dimnames=list(names(x), NULL))
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

setMethod("as.integer", "Ranges",
    function(x, ...)
    {
        x <- x[width(x) > 0L]
        mseq(start(x), end(x))
    }
)

setMethod("unlist", "Ranges",
    function(x, recursive = TRUE, use.names = TRUE)
    {
        if (!missing(recursive))
            warning("'recursive' argument currently ignored")
        ans <- as.integer(x)
        if (use.names) {
            nms <- rep(names(x), elementLengths(x))
            if (!is.null(nms) && !is.null(names(ans)))
                nms <- paste(nms, names(ans), sep = ".")
            else if (is.null(nms))
                nms <- names(ans)
            names(ans) <- nms
        }
        ans
    }
)

setMethod("[[", "Ranges",
    function(x, i, j, ...)
    {
        i <- checkAndTranslateDbleBracketSubscript(x, i)
        ans_shift <- start(x)[i] - 1L
        ans_length <- width(x)[i]
        seq_len(ans_length) + ans_shift
    }
)

### Without this definition, we inherit the method for Sequence objects
### which is very inefficient on Ranges objects!
setMethod("elementLengths", "Ranges", function(x) width(x))

setMethod("show", "Ranges",
    function(object)
    {
        lo <- length(object)
        cat(class(object), " of length ", lo, "\n", sep="")
        if (lo == 0L) {
            return(NULL)
        } else if (lo < 20L) {
            showme <-
              as.data.frame(object,
                            row.names=paste("[", seq_len(lo), "]", sep=""))
        } else {
            sketch <- function(x)
              c(window(x, 1L, 9L), "...", window(x, length(x)-8L, length(x)))
            showme <-
              data.frame(start=sketch(start(object)),
                         end=sketch(end(object)),
                         width=sketch(width(object)),
                         row.names=c(paste("[", 1:9, "]", sep=""), "...",
                                     paste("[", (lo-8L):lo, "]", sep="")),
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

setMethod("showAsCell", "Ranges", function(object)
          paste("[", format(start(object)), ", ", format(end(object)), "]",
                sep = ""))


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Testing a Ranges object.
###

### A Ranges object is considered empty iff all its ranges are empty.
setMethod("isEmpty", "Ranges", function(x) all(width(x) == 0L))

setGeneric("isDisjoint", function(x) standardGeneric("isDisjoint"))

setMethod("isDisjoint", "Ranges",
    function(x)
    {
        x <- x[width(x) > 0L]
        if (length(x) < 2)
            return(TRUE)
        starts <- start(x)
        startord <- order(starts)
        all(starts[startord][-1L] - end(x)[startord][-length(x)] >= 1L)
    }
)

setGeneric("isNormal", function(x) standardGeneric("isNormal"))

setMethod("isNormal", "Ranges",
    function(x)
    {
        all_ok <- all(width(x) >= 1L)
        if (length(x) >= 2)
            all_ok <- all_ok && all(start(x)[-1L] - end(x)[-length(x)] >= 2L)
        all_ok
    }
)

setGeneric("whichFirstNotNormal",
    function(x) standardGeneric("whichFirstNotNormal")
)

setMethod("whichFirstNotNormal", "Ranges",
    function(x)
    {
        is_ok <- width(x) >= 1L
        if (length(x) >= 2)
            is_ok <- is_ok & c(TRUE, start(x)[-1L] - end(x)[-length(x)] >= 2L)
        which(!is_ok)[1L]
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Core endomorphisms.
###
### TODO: "[" and most of the Ranges endomorphisms below are only defined for
### IRanges objects. Need to fix up the update mechanism, so that they can be
### defined on 'Ranges'. "[" and other endomorphisms below are currently
### implemented as wrappers that coerce to IRanges, which is not a general,
### long-term solution.

setMethod("[", "Ranges",
    function(x, i, j, ..., drop)
    {
        if (!missing(j) || length(list(...)) > 0L)
            stop("invalid subsetting")
        as(callGeneric(as(x, "IRanges"), i=i, ...), class(x))
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### More endomorphisms.
###

setGeneric("shift", signature="x",
    function(x, shift, use.names=TRUE) standardGeneric("shift")
)

setMethod("shift", "Ranges",
    function(x, shift, use.names=TRUE)
    {
        ir <- as(x, "IRanges")
        y <- shift(ir, shift=shift, use.names=use.names)
        as(y, class(x))
    }
)

setGeneric("restrict", signature="x",
    function(x, start=NA, end=NA, keep.all.ranges=FALSE, use.names=TRUE)
        standardGeneric("restrict")
)

setMethod("restrict", "Ranges",
    function(x, start=NA, end=NA, keep.all.ranges=FALSE, use.names=TRUE)
    {
        ir <- as(x, "IRanges")
        y <- restrict(ir, start=start, end=end, keep.all.ranges=keep.all.ranges,
                      use.names=use.names)
        as(y, class(x))
    }
)

setGeneric("narrow", signature="x",
    function(x, start=NA, end=NA, width=NA, use.names=TRUE)
        standardGeneric("narrow")
)

setMethod("narrow", "Ranges",
    function(x, start=NA, end=NA, width=NA, use.names=TRUE)
    {
        ir <- as(x, "IRanges")
        y <- narrow(ir, start=start, end=end, width=width, use.names=use.names)
        as(y, class(x))
    }
)

setGeneric("resize", signature="x",
    function(x, width, start=TRUE, use.names=TRUE)
      standardGeneric("resize")
)

setMethod("resize", "Ranges",
    function(x, width, start=TRUE, use.names=TRUE)
    {
        ir <- as(x, "IRanges")
        y <- resize(ir, width=width, start=start, use.names=use.names)
        as(y, class(x))
    }
)

setGeneric("threebands", signature="x",
    function(x, start=NA, end=NA, width=NA)
        standardGeneric("threebands")
)

setGeneric("reduce", signature="x",
    function(x, ...) standardGeneric("reduce")
)

setMethod("reduce", "Ranges",
    function(x, with.inframe.attrib=FALSE)
    {
        ir <- as(x, "IRanges")
        y <- reduce(ir, with.inframe.attrib)
        as(y, class(x))
    }
)

setGeneric("gaps", signature="x",
    function(x, start=NA, end=NA) standardGeneric("gaps")
)

setMethod("gaps", "Ranges",
    function(x, start=NA, end=NA)
    {
        ir <- as(x, "IRanges")
        y <- gaps(ir, start=start, end=end)
        as(y, class(x))
    }
)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "reflect" generic and method
###
### |   xxx |
### to
### | xxx   |
###

setGeneric("reflect", function(x, ...) standardGeneric("reflect"))

setMethod("reflect", "Ranges",
    function(x, bounds, use.names = TRUE)
    {
        if (!is(bounds, "Ranges") || length(bounds) != length(x))
            stop("'bounds' must be a Ranges object of length equal to that of 'x'")
        x <- update(x, start = end(bounds) - (end(x) - start(bounds)),
                    width = width(x), check = FALSE)
        if (!normargUseNames(use.names))
            names(x) <- NULL
        x
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "flank" generic and method
###

setGeneric("flank", function(x, ...) standardGeneric("flank"))

setMethod("flank", "Ranges",
    function(x, width, start = TRUE, both = FALSE, use.names = TRUE)
    {
        if (!is.numeric(width))
            stop("'width' must be numeric")
        if (!is.logical(start) || any(is.na(start)))
            stop("'start' must be logical without NA's")
        if (!isTRUEorFALSE(both))
            stop("'both' must be TRUE or FALSE")
        start <- recycleVector(start, length(x))
        width <- recycleVector(width, length(x))
        if (both)
            x <-
              update(x, start = ifelse(start, start(x) - abs(width),
                                       end(x) - abs(width) + 1L),
                     width = 2L * abs(width), check = FALSE)
        else
            x <-
              update(x,
                     start = ifelse(start,
                             ifelse(width < 0L, start(x), start(x) - width),
                             ifelse(width < 0L, end(x) + width + 1L, end(x) + 1L)),
                     width = abs(width), check = FALSE)
        if (!normargUseNames(use.names))
            names(x) <- NULL
        x
    }
)

## make intervals disjoint by taking the union of the endpoints
setGeneric("disjoin", function(x, ...) standardGeneric("disjoin"))
setMethod("disjoin", "Ranges",
    function(x)
    {
        ## starts: original starts and end+1 when inside another interval
        ## ends: original ends and start-1 when inside another interval
        starts <- unique(start(x))
        ends <- unique(end(x))
        adj_start <- sort(unique(c(starts, ends + 1L)))
        adj_end <- sort(unique(c(ends, starts - 1L)))
        adj <- update(x, start = head(adj_start, -1L), end = tail(adj_end, -1L),
                      check = FALSE)
        adj[adj %in% x]
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### More operations.
###

## this one is probably fine not being an endomorphism
setMethod("range", "Ranges",
    function(x, ..., na.rm)
    {
        args <- unname(list(x, ...))
        if (!all(sapply(args, is, "Ranges")))
            stop("all arguments in '...' must be Ranges objects")
        x <- do.call(c, args)
        if (!length(x))
            IRanges()
        else
            IRanges(min(start(x)), max(end(x)))
    }
)

### Find objects in the index that overlap those in a query set.
setGeneric("findOverlaps", signature = c("query", "subject"),
    function(query, subject, maxgap = 0, multiple = TRUE, ...)
        standardGeneric("findOverlaps")
)

overlap <- function(object, query, maxgap = 0, multiple = TRUE, ...)
{
    .Deprecated("findOverlaps")
    if (missing(query))
        findOverlaps(object, maxgap = maxgap, multiple = multiple, ...)
    else
        findOverlaps(query, object, maxgap = maxgap, multiple = multiple, ...)
}

setGeneric("countOverlaps", signature = c("query", "subject"),
    function(query, subject, ...) standardGeneric("countOverlaps")
)

setMethod("countOverlaps", c("Ranges", "Ranges"),
    function(query, subject)
    { ## might be faster someday
        sum(query %in% subject)
    }
)

countOverlap <- function(object, query)
{
    .Deprecated("countOverlaps")
    countOverlaps(query, object)
}

setMethod("%in%", c("Ranges", "Ranges"),
    function(x, table) !is.na(match(x, table))
)

setGeneric("match",
    function(x, table, nomatch = NA_integer_, incomparables = NULL)
        standardGeneric("match")
)

setMethod("match", c("Ranges", "Ranges"),
    function(x, table, nomatch = NA_integer_, incomparables = NULL)
    {
        if (length(nomatch) != 1)
            stop("'nomatch' must be of length 1") 
        ans <- findOverlaps(x, table, multiple=FALSE)
        if (!is.na(nomatch))
            ans[is.na(ans)] <- nomatch
        ans
    }
)

setClassUnion("RangesORmissing", c("Ranges", "missing"))

setGeneric("nearest", function(x, subject, ...) standardGeneric("nearest"))

setMethod("nearest", c("Ranges", "RangesORmissing"),
    function(x, subject)
    {
        if (!missing(subject))
            ol <- findOverlaps(x, subject, multiple = FALSE)
        else { ## avoid overlapping with self
            subject <- x
            olm <- as.matrix(findOverlaps(x, subject))
            olm <- olm[olm[,1L] != olm[,2L],]
            ol <- olm[,2L][match(seq_len(length(subject)), olm[,1L])]
        }
        x <- x[is.na(ol)]
        before <- precede(x, subject)
        after <- follow(x, subject)
        pre <- (start(subject)[before] - end(x)) < (start(x) - end(subject)[after])
        pre[is.na(pre)] <- is.na(after)[is.na(pre)]
        ol[is.na(ol)] <- ifelse(pre, before, after)
        ol
    }
)

setGeneric("precede", function(x, subject = x, ...) standardGeneric("precede"))

setMethod("precede", c("Ranges", "RangesORmissing"),
    function(x, subject)
    {
        s <- start(subject)
        ord <- NULL
        if (is.unsorted(s)) {
            ord <- order(s)
            s <- s[ord]
        }
        i <- findInterval(end(x), s) + 1L
        if (!is.null(ord))
            i <- ord[i]
        i[i > length(subject)] <- NA
        i
    }
)

setGeneric("follow", function(x, subject = x, ...) standardGeneric("follow"))

setMethod("follow", c("Ranges", "RangesORmissing"),
    function(x, subject)
    {
        e <- end(subject)
        ord <- NULL
        if (is.unsorted(e)) {
            ord <- order(e)
            e <- e[ord]
        }
        i <- findInterval(start(x) - 1L, e)
        i[i == 0] <- NA
        if (!is.null(ord))
            i <- ord[i]
        i
    }
)

## zooming (symmetrically scales the width)
setMethod("Ops", c("Ranges", "numeric"),
    function(e1, e2)
    {
        if (any(is.na(e2)))
            stop("NA not allowed as zoom factor")
        if ((length(e1) < length(e2) && length(e1)) ||
            (length(e1) && !length(e2)) ||
            (length(e1) %% length(e2) != 0))
            stop("zoom factor length not a multiple of number of ranges")
        if (.Generic == "*") {
            e2 <- ifelse(e2 < 0, abs(1/e2), e2)
            r <- e1
            mid <- (start(r)+end(r))/2
            w <- width(r)/e2
            update(r, start = ceiling(mid - w/2), width = floor(w), check = FALSE)
        } else {
            if (.Generic == "-") {
                e2 <- -e2
                .Generic <- "+"
            }
            if (.Generic == "+") {
                if (any(-e2*2 > width(e1)))
                    stop("adjustment would result in ranges with negative widths")
                update(e1, start = start(e1) - e2, end = end(e1) + e2, check = FALSE)
            }
        }
    }
)

## make intervals disjoint by segregating them into separate Ranges
setGeneric("disjointBins", function(x, ...) standardGeneric("disjointBins"))
setMethod("disjointBins", "Ranges",
    function(x)
    {
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
    }
)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Old stuff (Deprecated or Defunct).
###

setGeneric("first", function(x) standardGeneric("first"))
setMethod("first", "Ranges", function(x) .Defunct("start"))
setGeneric("last", function(x) standardGeneric("last"))
setMethod("last", "Ranges", function(x) .Defunct("end"))
