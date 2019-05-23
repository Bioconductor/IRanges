### =========================================================================
### IPos objects
### -------------------------------------------------------------------------
###


setClass("IPos",
    contains=c("Pos", "IPosRanges"),
    representation("VIRTUAL")
)

### Too expensive! (and not needed)
#setValidity2("IPos", validate_Pos)

### UnstitchedIPos and StitchedIPos objects cannot hold names at the moment.

setClass("UnstitchedIPos",
    contains="IPos",
    representation(
        pos="integer"
    )
)

### Combine the new parallel slots with those of the parent class. Make sure
### to put the new parallel slots *first*.
setMethod("parallelSlotNames", "UnstitchedIPos",
    function(x) c("pos", callNextMethod())
)

setClass("StitchedIPos",
    contains="IPos",
    representation(
        pos_runs="IRanges"  # An unnamed IRanges instance that has
                            # been "stitched" (see below).
    )
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Getters
###
### No setters (names<-, pos<-) at the moment for IPos objects!
### Should we have them?
###

setMethod("pos", "UnstitchedIPos", function(x) x@pos)
setMethod("pos", "StitchedIPos", function(x) unlist_as_integer(x@pos_runs))

setMethod("length", "UnstitchedIPos", function(x) length(x@pos))
setMethod("length", "StitchedIPos", function(x) sum(width(x@pos_runs)))


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Collapse runs of "stitchable integer ranges"
###
### In an IntegerRanges object 'x', 2 ranges x[i] and x[i+1] are "stitchable"
### if start(x[i+1]) == end(x[i])+1. For example, in the following object:
###   1: .....xxxx.............
###   2: ...xx.................
###   3: .........xxx..........
###   4: ............xxxxxx....
###   5: ..................x...
### x[3] and x[4] are stitchable, and x[4] and x[5] are stitchable. So
### x[3], x[4], and x[5] form a run of "stitchable ranges" that will collapse
### into the following single range after stitching:
###      .........xxxxxxxxxx...
### Note that x[1] and x[3] are not stitchable because they are not
### consecutive vector elements (but they would if we removed x[2]).

### stitch_IntegerRanges() below takes any IntegerRanges derivative and
### returns an IRanges object (so is NOT an endomorphism). Note that this
### transformation preserves 'sum(width(x))'.
### Also note that this is an "inter range transformation". However unlike
### range(), reduce(), gaps(), or disjoin(), its result depends on the order
### of the elements in the input vector. It's also idempotent like range(),
### reduce(), and disjoin() (gaps() is not).

### TODO: Define and export stitch() generic and method for IntegerRanges
### objects (in inter-range-methods.R).
### Maybe it would also make sense to have an isStitched() generic like we
### have isDisjoint() to provide a quick and easy way to check the state of
### the object before applying the transformation to it. In theory each
### idempotent inter range transformation could have a "state checker" so
### maybe add isReduced() too (range() probably doesn't need one).

stitch_IntegerRanges <- function(x)
{
    if (length(x) == 0L)
        return(IRanges())
    x_start <- start(x)
    x_end <- end(x)

    ## Find runs of stitchable elements along 'x'.
    ## Each run is described by the indices of its first ('run_from') and
    ## last ('run_to') elements in 'x'.
    ## The runs form a partitioning of 'x'.
    new_run_idx <- which(x_start[-1L] != x_end[-length(x)] + 1L)
    run_from <- c(1L, new_run_idx + 1L)
    run_to <- c(new_run_idx, length(x))

    IRanges(x_start[run_from], x_end[run_to])
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Validity
###

### TODO


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Constructor
###

### 'pos' must be an integer vector with no NAs.
.make_StitchedIPos_from_pos <- function(pos)
{
    pos_runs <- as(pos, "IRanges")
    new2("StitchedIPos", pos_runs=pos_runs, check=FALSE)
}

.from_UnstitchedIPos_to_StitchedIPos <- function(from)
{
    ans <- .make_StitchedIPos_from_pos(from@pos)
    mcols(ans) <- mcols(from, use.names=FALSE)
    ans
}

### 'pos_runs' must be an IRanges object.
.make_UnstitchedIPos_from_pos_runs <- function(pos_runs)
{
    pos <- unlist_as_integer(pos_runs)
    new2("UnstitchedIPos", pos=pos, check=FALSE)
}

.from_StitchedIPos_to_UnstitchedIPos <- function(from)
{
    ans <- .make_UnstitchedIPos_from_pos_runs(from@pos_runs)
    mcols(ans) <- mcols(from, use.names=FALSE)
    ans
}

### 'pos' must be an integer vector with no NAs or an IntegerRanges derivative.
### This is NOT checked!
new_UnstitchedIPos <- function(pos=integer(0))
{
    if (is(pos, "UnstitchedIPos"))
        return(pos)
    if (is(pos, "StitchedIPos"))
        return(.from_StitchedIPos_to_UnstitchedIPos(pos))
    if (is.integer(pos)) {
        ## Treat 'pos' as a vector of single positions.
        return(new2("UnstitchedIPos", pos=pos, check=FALSE))
    }
    ## 'pos' is an IntegerRanges derivative. Treat its ranges as runs of
    ## consecutive positions.
    ans_len <- sum(width(pos))  # no more integer overflow in R >= 3.5
    if (ans_len > .Machine$integer.max)
        stop("too many positions in 'pos'")
    .make_UnstitchedIPos_from_pos_runs(pos)
}

### 'pos' must be an integer vector with no NAs or an IntegerRanges derivative.
### This is NOT checked!
new_StitchedIPos <- function(pos=integer(0))
{
    if (is(pos, "StitchedIPos"))
        return(pos)
    if (is(pos, "UnstitchedIPos"))
        return(.from_UnstitchedIPos_to_StitchedIPos(pos))
    if (is.integer(pos)) {
        ## Treat 'pos' as a vector of single positions.
        return(.make_StitchedIPos_from_pos(pos))
    }
    ## 'pos' is an IntegerRanges derivative. Treat its ranges as runs of
    ## consecutive positions.
    ans_len <- sum(width(pos))  # no more integer overflow in R >= 3.5
    if (ans_len > .Machine$integer.max)
        stop("too many positions in 'pos'")
    pos_runs <- stitch_IntegerRanges(pos)
    pos_runs <- pos_runs[width(pos_runs) != 0L]
    new2("StitchedIPos", pos_runs=pos_runs, check=FALSE)
}

### Returns an integer vector with no NAs or an IntegerRanges derivative.
.normarg_pos <- function(pos)
{
    if (is(pos, "IntegerRanges"))
        return(pos)
    if (is.numeric(pos)) {
        if (!is.integer(pos))
            pos <- as.integer(pos)
        if (anyNA(pos))
            stop("'pos' cannot contain NAs")
        return(pos)
    }
    ans <- try(as(pos, "IRanges"), silent=TRUE)
    if (inherits(ans, "try-error"))
        stop("'pos' must represent positions")
    ans
}

.normarg_stitch <- function(stitch, pos)
{
    if (!(is.logical(stitch) && length(stitch) == 1L))
        stop("'stitch' must be TRUE, FALSE, or NA")
    if (!is.na(stitch))
        return(stitch)
    is(pos, "IntegerRanges") && !is(pos, "UnstitchedIPos")
}

### If the input object 'pos' is itself an IPos object, its metadata columns
### are propagated.
IPos <- function(pos=integer(0), stitch=NA)
{
    pos <- .normarg_pos(pos)
    stitch <- .normarg_stitch(stitch, pos)
    if (stitch) {
        ans <- new_StitchedIPos(pos)
    } else {
        ans <- new_UnstitchedIPos(pos)
    }
    ans
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion
###

setAs("UnstitchedIPos", "StitchedIPos", .from_UnstitchedIPos_to_StitchedIPos)

setAs("StitchedIPos", "UnstitchedIPos", .from_StitchedIPos_to_UnstitchedIPos)

.check_IntegerRanges_for_coercion_to_IPos <- function(from, to)
{
    if (!all(width(from) == 1L))
        stop(wmsg("all the ranges in the ", class(from), " object to ",
                  "coerce to ", to, " must have a width of 1"))
    if (!is.null(names(from)))
        warning(wmsg("because an IPos derivative cannot hold them, the ",
                     "names on the ", class(from), " object couldn't be ",
                     "propagated during its coercion to ", to))
}
.from_IntegerRanges_to_UnstitchedIPos <- function(from)
{
    .check_IntegerRanges_for_coercion_to_IPos(from, "UnstitchedIPos")
    ans <- new_UnstitchedIPos(from)
    mcols(ans) <- mcols(from, use.names=FALSE)
    ans
}
.from_IntegerRanges_to_StitchedIPos <- function(from)
{
    .check_IntegerRanges_for_coercion_to_IPos(from, "StitchedIPos")
    ans <- new_StitchedIPos(from)
    mcols(ans) <- mcols(from, use.names=FALSE)
    ans
}
setAs("IntegerRanges", "UnstitchedIPos", .from_IntegerRanges_to_UnstitchedIPos)
setAs("IntegerRanges", "StitchedIPos", .from_IntegerRanges_to_StitchedIPos)
setAs("IntegerRanges", "IPos", .from_IntegerRanges_to_UnstitchedIPos)

setAs("ANY", "UnstitchedIPos", function(from) IPos(from, stitch=FALSE))
setAs("ANY", "StitchedIPos", function(from) IPos(from, stitch=TRUE))
setAs("ANY", "IPos", function(from) IPos(from))

### S3/S4 combo for as.data.frame.IPos
### The "as.data.frame" method for IntegerRanges objects works on an IPos
### object but returns a data.frame with identical "start" and "end" columns,
### and a "width" column filled with 1. We overwrite it to return a data.frame
### with a "pos" column instead of the "start" and "end" columns, and no
### "width" column.
.as.data.frame.IPos <- function(x, row.names=NULL, optional=FALSE)
{
    if (!identical(optional, FALSE))
        warning(wmsg("'optional' argument was ignored"))
    ans <- data.frame(pos=pos(x), row.names=row.names, stringsAsFactors=FALSE)
    x_mcols <- mcols(x, use.names=FALSE)  # can be NULL!
    if (!is.null(x_mcols))
        ans <- cbind(ans, as.data.frame(x_mcols))
    ans
}
as.data.frame.IPos <- function(x, row.names=NULL, optional=FALSE, ...)
    .as.data.frame.IPos(x, row.names=NULL, optional=FALSE, ...)
setMethod("as.data.frame", "IPos", .as.data.frame.IPos)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Subsetting
###

### NOT exported but used in the GenomicRanges package.
### 'pos_runs' must be an IRanges or GRanges object or any range-based
### object as long as it supports start(), end(), width(), and is subsettable.
### 'i' must be an IntegerRanges object with no zero-width ranges.
extract_pos_runs_by_ranges <- function(pos_runs, i)
{
    map <- S4Vectors:::map_ranges_to_runs(width(pos_runs),
                                          start(i), width(i))
    ## Because 'i' has no zero-width ranges, 'mapped_range_span' cannot
    ## contain zeroes and so 'mapped_range_Ltrim' and 'mapped_range_Rtrim'
    ## cannot contain garbbage.
    mapped_range_offset <- map[[1L]]
    mapped_range_span <- map[[2L]]
    mapped_range_Ltrim <- map[[3L]]
    mapped_range_Rtrim <- map[[4L]]
    run_idx <- S4Vectors:::fancy_mseq(mapped_range_span,
                                      mapped_range_offset)
    pos_runs <- pos_runs[run_idx]
    if (length(run_idx) != 0L) {
        Rtrim_idx <- cumsum(mapped_range_span)
        Ltrim_idx <- c(1L, Rtrim_idx[-length(Rtrim_idx)] + 1L)
        trimmed_start <- start(pos_runs)[Ltrim_idx] +
                         mapped_range_Ltrim
        trimmed_end <- end(pos_runs)[Rtrim_idx] - mapped_range_Rtrim
        start(pos_runs)[Ltrim_idx] <- trimmed_start
        end(pos_runs)[Rtrim_idx] <- trimmed_end
        new_len <- sum(width(pos_runs))  # no more integer overflow in R >= 3.5
        if (new_len > .Machine$integer.max)
            stop("subscript is too big")
    }
    pos_runs
}

setMethod("extractROWS", "StitchedIPos",
    function(x, i)
    {
        i <- normalizeSingleBracketSubscript(i, x, as.NSBS=TRUE)
        ## TODO: Maybe make this the coercion method from NSBS to
        ## IntegerRanges.
        if (is(i, "RangesNSBS")) {
            ir <- i@subscript
            ir <- ir[width(ir) != 0L]
        } else {
            ir <- as(as.integer(i), "IRanges")
        }
        new_pos_runs <- extract_pos_runs_by_ranges(x@pos_runs, ir)
        x@pos_runs <- stitch_IntegerRanges(new_pos_runs)
        mcols(x) <- extractROWS(mcols(x, use.names=FALSE), i)
        x
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Show
###

.from_IPos_to_naked_character_matrix_for_display <- function(x)
{
    x_len <- length(x)
    x_mcols <- mcols(x, use.names=FALSE)
    x_nmc <- if (is.null(x_mcols)) 0L else ncol(x_mcols)
    ans <- cbind(pos=as.character(pos(x)))
    if (x_nmc > 0L) {
        tmp <- as.data.frame(lapply(x_mcols, showAsCell), optional=TRUE)
        ans <- cbind(ans, `|`=rep.int("|", x_len), as.matrix(tmp))
    }
    ans
}

show_IPos <- function(x, margin="", print.classinfo=FALSE)
{
    x_len <- length(x)
    x_mcols <- mcols(x, use.names=FALSE)
    x_nmc <- if (is.null(x_mcols)) 0L else ncol(x_mcols)
    cat(classNameForDisplay(x), " object with ",
        x_len, " ", ifelse(x_len == 1L, "position", "positions"),
        " and ",
        x_nmc, " metadata ", ifelse(x_nmc == 1L, "column", "columns"),
        ":\n", sep="")
    ## S4Vectors:::makePrettyMatrixForCompactPrinting() assumes that head()
    ## and tail() work on 'xx'.
    xx <- as(x, "IPos")
    out <- S4Vectors:::makePrettyMatrixForCompactPrinting(xx,
                .from_IPos_to_naked_character_matrix_for_display)
    if (print.classinfo) {
        .COL2CLASS <- c(pos="integer")
        classinfo <-
            S4Vectors:::makeClassinfoRowForCompactPrinting(x, .COL2CLASS)
        ## A sanity check, but this should never happen!
        stopifnot(identical(colnames(classinfo), colnames(out)))
        out <- rbind(classinfo, out)
    }
    if (nrow(out) != 0L)
        rownames(out) <- paste0(margin, rownames(out))
    ## We set 'max' to 'length(out)' to avoid the getOption("max.print")
    ## limit that would typically be reached when 'showHeadLines' global
    ## option is set to Inf.
    print(out, quote=FALSE, right=TRUE, max=length(out))
}

setMethod("show", "IPos",
    function(object) show_IPos(object, margin="  ", print.classinfo=TRUE)
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Concatenation
###

.concatenate_StitchedIPos_objects <-
    function(x, objects=list(), use.names=TRUE, ignore.mcols=FALSE, check=TRUE)
{
    objects <- S4Vectors:::prepare_objects_to_bind(x, objects)
    all_objects <- c(list(x), objects)

    ans_len <- sum(lengths(all_objects))  # no more integer overflow
                                          # in R >= 3.5
    if (ans_len > .Machine$integer.max)
        stop("too many integer positions to concatenate")

    ## 1. Take care of the parallel slots

    ## Call method for Vector objects to concatenate all the parallel
    ## slots (only "elementMetadata" in the case of IPos) and stick them
    ## into 'ans'. Note that the resulting 'ans' can be an invalid object
    ## because its "elementMetadata" slot can be longer (i.e. have more rows)
    ## than 'ans' itself so we use 'check=FALSE' to skip validation.
    ans <- callNextMethod(x, objects, use.names=use.names,
                                      ignore.mcols=ignore.mcols,
                                      check=FALSE)

    ## 2. Take care of the non-parallel slots

    ## Concatenate the "pos_runs" slots.
    pos_runs_list <- lapply(all_objects, slot, "pos_runs")
    ans_pos_runs <- stitch_IntegerRanges(
        bindROWS(pos_runs_list[[1L]], pos_runs_list[-1L])
    )

    BiocGenerics:::replaceSlots(ans, pos_runs=ans_pos_runs,
                                     check=check)
}

setMethod("bindROWS", "StitchedIPos", .concatenate_StitchedIPos_objects)

