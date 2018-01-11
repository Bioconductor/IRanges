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
### parallelSlotNames()
###

### Combine the new parallel slots with those of the parent class. Make sure
### to put the new parallel slots *first*.
setMethod("parallelSlotNames", "Views",
    function(x) c("ranges", callNextMethod())
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Accessor methods.
###

setGeneric("subject", function(x) standardGeneric("subject"))

setMethod("subject", "Views", function(x) x@subject)

setMethod("ranges", "Views",
    function(x, use.names=TRUE, use.mcols=FALSE) x@ranges
)

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

setMethod("start", "Views", function(x, ...) start(ranges(x)))
setMethod("end", "Views", function(x, ...) end(ranges(x)))
setMethod("width", "Views", function(x) width(ranges(x)))
setMethod("names", "Views", function(x) names(ranges(x)))

setReplaceMethod("start", "Views",
    function(x, ..., value)
    {
        start(x@ranges, ...) <- value
        x
    }
)

setReplaceMethod("end", "Views",
    function(x, ..., value)
    {
        end(x@ranges, ...) <- value
        x
    }
)

setReplaceMethod("width", "Views",
    function(x, ..., value)
    {
        width(x@ranges, ...) <- value
        x
    }
)

setReplaceMethod("names", "Views",
    function(x, value)
    {
        names(x@ranges) <- value
        x
    }
)

setMethod("elementNROWS", "Views", function(x) width(x))


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Constructor
###

### The low-level "Views" constructor.
### NOT exported but used in XVector, Biostrings, and triplex packages.
### TODO: - add a 'check.limits' arg (default to TRUE) for raising an error if
###         some views are "out of limits".
new_Views <- function(subject, start=NULL, end=NULL, width=NULL, names=NULL,
                      Class=NULL)
{
    if (is(start, "Ranges")) {
        if (!is.null(end) || !is.null(width))
            stop(wmsg("'end' and 'width' must be NULLs when ",
                      "'start' is a Ranges object"))
        ans_ranges <- start
        if (class(ans_ranges) != "IRanges")
            ans_ranges <- as(ans_ranges, "IRanges")
        ## Keep the names that are already in 'ranges' unless the 'names' arg
        ## was specified.
        if (!is.null(names))
            names(ans_ranges) <- names
        ans_mcols <- mcols(ans_ranges)
        mcols(ans_ranges) <- NULL
    } else {
        ans_ranges <- IRanges(start=start, end=end, width=width, names=names)
        ans_mcols <- NULL
    }
    if (is.null(Class))
        Class <- paste(class(subject), "Views", sep="")
    new2(Class, subject=subject,
                ranges=ans_ranges,
                elementMetadata=ans_mcols,
                check=FALSE)
}

### The user-friendly "Views" constructor.
### TODO: Same as for the new_Views() function above.
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
  x_ranges <- restrict(ranges(x), start = 1L)
  if (is.na(max.width)) {
    max.width <- max(width(x_ranges))
  }
  rev <- S4Vectors:::recycleVector(rev, length(x))
  part <- PartitioningByWidth(x_ranges)
  ord <- S4Vectors:::mseq(ifelse(rev, end(part), start(part)),
                          ifelse(rev, start(part), end(part)))
  v <- extractROWS(subject(x), x_ranges)[ord]
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

setMethod("getListElement", "Views",
    function(x, i, exact=TRUE)
    {
        i <- normalizeDoubleBracketSubscript(i, x, exact=exact)
        start <- start(x)[i]
        end <- end(x)[i]
        if (start < 1L || end > length(subject(x)))
            stop("view is out of limits")
        extractROWS(subject(x), IRanges(start, end))
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Concatenation
###

### '.Object' is assumed to contain the expected common subject in its
### "subject" slot.
.check_that_Views_objects_are_concatenable <- function(.Object, objects)
{
    ok <- vapply(
        objects,
        function(object) isTRUE(all.equal(subject(object), subject(.Object))),
        logical(1),
        USE.NAMES=FALSE
    )
    if (!all(ok))
        stop(wmsg("the Views objects to concatenate ",
                  "must have the same subject"))
}

.concatenate_Views_objects <-
    function(.Object, objects, use.names=TRUE, ignore.mcols=FALSE, check=TRUE)
{
    objects <- unname(S4Vectors:::delete_NULLs(objects))
    S4Vectors:::check_class_of_objects_to_concatenate(.Object, objects)
    .check_that_Views_objects_are_concatenable(.Object, objects)
    callNextMethod()
}

setMethod("concatenateObjects", "Views", .concatenate_Views_objects)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "trim" function.
###

setGeneric("trim", signature="x",
    function(x, use.names=TRUE, ...) standardGeneric("trim")
)

setMethod("trim", "Views",
    function(x, use.names=TRUE)
    {
        if (length(x) == 0L)
            return(x)
        if (min(start(x)) >= 1L && max(end(x)) <= length(subject(x)))
            return(x)
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
### The "slidingViews" function.
###

slidingViews <- function(subject, width, shift = 1L)
{
    ranges <- slidingIRanges(length(subject), width, shift)
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
          sapply(structure(seq_len(length(X)), names=names(X)),
              function(i)
                  FUN(extractROWS(Xsubject,
                                  IRanges(start=Xstart[i], width=Xwidth[i])),
                      ...),
              simplify = simplify)
        if (!simplify) {
            ans <- S4Vectors:::new_SimpleList_from_list("SimpleList", ans,
                                                        metadata = metadata(X),
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


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Show
###

setMethod("showAsCell", "Views", function(object) {
              showAsCell(as(object, relistToClass(subject(object))))
          })
