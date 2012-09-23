### =========================================================================
### Views objects
### -------------------------------------------------------------------------
###
### The Views virtual class is a general container for storing a set of views
### on an arbitrary Vector object, called the "subject".
###

setClass("Views",
    contains="List",
    representation(
        "VIRTUAL",
        subject="Vector",
        ranges="IRanges"
    )
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Accessor methods.
###

setGeneric("subject", function(x) standardGeneric("subject"))

setMethod("subject", "Views", function(x) x@subject)

setGeneric("ranges", function(x, ...) standardGeneric("ranges"))

setMethod("ranges", "Views", function(x) x@ranges)

setGeneric("ranges<-", function(x, ..., value) standardGeneric("ranges<-"))

setReplaceMethod("ranges", "Views",
    function(x, ..., value)
    {
        stop("ranges setter for Views objects not ready yet")
    }
)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Methods derived from the IRanges interface.
###

setMethod("length", "Views", function(x) length(ranges(x)))

setMethod("start", "Views", function(x, ...) start(ranges(x)))
setMethod("end", "Views", function(x, ...) end(ranges(x)))
setMethod("width", "Views", function(x) width(ranges(x)))
setMethod("names", "Views", function(x) names(ranges(x)))

setReplaceMethod("start", "Views",
    function(x, check=TRUE, value)
    {
        x@ranges <- `start<-`(ranges(x), check=check, value)
        x
    }
)

setReplaceMethod("end", "Views",
    function(x, check=TRUE, value)
    {
        x@ranges <- `end<-`(ranges(x), check=check, value)
        x
    }
)

setReplaceMethod("width", "Views",
    function(x, check=TRUE, value)
    {
        x@ranges <- `width<-`(ranges(x), check=check, value)
        x
    }
)

setReplaceMethod("names", "Views",
    function(x, value)
    {
        x@ranges <- `names<-`(ranges(x), value)
        x
    }
)

setMethod("[", "Views",
    function(x, i, j, ..., drop)
    {
        if (!missing(j) || length(list(...)) > 0L)
            stop("invalid subsetting")
        if (missing(i))
            return(x)
        x@ranges <- ranges(x)[i]
        x
    }
)

setMethod("elementLengths", "Views", function(x) width(x))


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Validity.
###

.valid.Views.width <- function(x)
{
    if (length(width(x)) != 0L && min(width(x)) <= 0L)
        return("null widths are not allowed")
    NULL
}

setValidity2("Views", .valid.Views.width)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The low-level "Views" constructor.
###
### TODO: - add a 'check.limits' arg (default to TRUE) for raising an error if
###         some views are "out of limits".
###

newViews <- function(subject, start=NULL, end=NULL, width=NULL, names=NULL,
                     Class=NULL)
{
    if (is(start, "Ranges")) {
        if (!is.null(end) || !is.null(width))
            stop("'end' and 'width' must be NULLs when 'start' is a Ranges object")
        ranges <- start
        if (class(ranges) != "IRanges")
            ranges <- as(ranges, "IRanges")
        ## Keep the names that are already in 'ranges' unless the 'names' arg
        ## was specified.
        if (!is.null(names))
            names(ranges) <- names
    } else {
        ranges <- IRanges(start=start, end=end, width=width, names=names)
    }
    if (is.null(Class))
        Class <- paste(class(subject), "Views", sep="")
    new2(Class, subject=subject, ranges=ranges, check=FALSE)
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The user-friendly "Views" constructor.
###
### TODO: Same as for the newViews() function above.
###

setGeneric("Views", signature="subject",
    function(subject, start=NULL, end=NULL, width=NULL, names=NULL)
        standardGeneric("Views")
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion.
###

### Returns a single view covering the entire sequence.
setAs("Vector", "Views",
    function(from) Views(from, start=1L, width=length(from))
)

setAs("Views", "Ranges", function(from) ranges(from))
setAs("Views", "IRanges", function(from) ranges(from))

### Unfortunately, even if we've already defined the IRanges->NormalIRanges
### "coerce" method to override the silly implicit one, we still need to
### define the <class>->NormalIRanges ones for every <class> that contains
### IRanges. Otherwise, again, 'as(x, "NormalIRanges")' would call another
### silly implicit method when 'x' is a <class> instance.
### Yes, this is another S4 "feature":
###   https://stat.ethz.ch/pipermail/r-devel/2008-April/049027.html
setAs("Views", "NormalIRanges",
    function(from) asNormalIRanges(ranges(from), force=TRUE)
)

setMethod("as.matrix", "Views", function(x, rev = FALSE, max.width = NA) {
  ## TODO: Supress this warning in BioC 2.12.
  msg <- c("as.matrix() on a Views object 'x' has changed ",
           "behavior: now each view is converted into a row of the\n",
           "  returned matrix. To achieve the old behavior, ",
           "do 'as.matrix(ranges(x))'.\n  To supress this warning, do ",
           "'suppressWarnings(as.matrix(x))'.\n  This warning will be ",
           "removed in BioC 2.12.")
  warning(msg)
  x_ranges <- restrict(ranges(x), start = 1L)
  if (is.na(max.width)) {
    max.width <- max(width(x_ranges))
  }
  rev <- recycleVector(rev, length(x))
  part <- PartitioningByWidth(x_ranges)
  ord <- mseq(ifelse(rev, end(part), start(part)),
              ifelse(rev, start(part), end(part)))
  v <- seqselect(subject(x), x_ranges)[ord]
  v_fill <- rep.int(NA, max.width * length(x))
  part <- PartitioningByWidth(rep(max.width, length(x)))
  i <- as.integer(IRanges(start(part), width = width(x_ranges)))
  v_fill[i] <- as.vector(v)
  matrix(v_fill, ncol = max.width, byrow = TRUE,
         dimnames = list(names(x), NULL))
})

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Extracting a view.
###

setMethod("[[", "Views",
    function(x, i, j, ...)
    {
        i <- checkAndTranslateDbleBracketSubscript(x, i)
        start <- start(x)[i]
        end <- end(x)[i]
        if (start < 1L || end > length(subject(x)))
            stop("view is out of limits")
        window(subject(x), start=start, end=end)
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "trim" function.
###

setGeneric("trim", signature="x",
    function(x, use.names=TRUE) standardGeneric("trim")
)

setMethod("trim", "Views",
    function(x, use.names=TRUE)
    {
        x@ranges <- restrict(ranges(x), start=1L, end=length(subject(x)),
                             keep.all.ranges=TRUE,
                             use.names=use.names)
        x
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "subviews" function.
###

### TODO: - add a 'check.limits' arg (default to TRUE) for raising an error if
###         some views are "out of limits"
setGeneric("subviews", signature="x",
    function(x, start=NA, end=NA, width=NA, use.names=TRUE)
        standardGeneric("subviews")
)

setMethod("subviews", "Views",
    function(x, start=NA, end=NA, width=NA, use.names=TRUE)
        trim(narrow(x, start=start, end=end, width=width, use.names=use.names))
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "successiveViews" function.
###

successiveViews <- function(subject, width, gapwidth=0, from=1)
{
    ranges <- successiveIRanges(width, gapwidth=gapwidth, from=from)
    Views(subject, ranges)
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "viewApply" function.
###

setGeneric("viewApply", signature="X",
    function(X, FUN, ..., simplify = TRUE) standardGeneric("viewApply")
)

setMethod("viewApply", "Views",
    function(X, FUN, ..., simplify = TRUE)
    {
        X <- trim(X)
        Xsubject <- subject(X)
        Xstart <- start(X)
        Xwidth <- width(X)
        ans <-
          sapply(structure(seq_len(length(X)), names = names(X)),
                 function(i)
                     FUN(window(Xsubject, start = Xstart[i], width = Xwidth[i]),
                         ...),
                 simplify = simplify)
        if (!simplify) {
            ans <- newList("SimpleList", ans, metadata = metadata(X),
                           mcols = mcols(X))
        }
        ans
    }
)

setGeneric("viewMins", signature="x",
           function(x, na.rm = FALSE) standardGeneric("viewMins"))
setGeneric("viewMaxs", signature="x",
           function(x, na.rm = FALSE) standardGeneric("viewMaxs"))
setGeneric("viewSums", signature="x",
           function(x, na.rm = FALSE) standardGeneric("viewSums"))
setGeneric("viewMeans", signature="x",
           function(x, na.rm = FALSE) standardGeneric("viewMeans"))
setGeneric("viewWhichMins", signature="x",
           function(x, na.rm = FALSE) standardGeneric("viewWhichMins"))
setGeneric("viewWhichMaxs", signature="x",
           function(x, na.rm = FALSE) standardGeneric("viewWhichMaxs"))
setGeneric("viewRangeMaxs",
           function(x, na.rm = FALSE) standardGeneric("viewRangeMaxs"))
setGeneric("viewRangeMins",
           function(x, na.rm = FALSE) standardGeneric("viewRangeMins"))

setMethod("Summary", "Views", function(x, ..., na.rm = FALSE) {
  viewSummaryFunMap <- list(min = viewMins, max = viewMaxs, sum = viewSums)
  viewSummaryFun <- viewSummaryFunMap[[.Generic]]
  if (!is.null(viewSummaryFun)) {
    if (length(list(...)))
      stop("Passing multiple arguments to '", .Generic, "' is not supported.")
    viewSummaryFun(x, na.rm = na.rm)
  } else {
    Summary(ranges(x), ..., na.rm = na.rm)
  }
})

setMethod("mean", "Views", viewMeans)

setMethod("which.max", "Views", function(x) {
  viewWhichMaxs(x, na.rm = TRUE)
})

setMethod("which.min", "Views", function(x) {
  viewWhichMins(x, na.rm = TRUE)
})

