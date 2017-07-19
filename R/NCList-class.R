### =========================================================================
### NCList and NCLists objects
### -------------------------------------------------------------------------


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### An S4 implementation of Nested Containment List (NCList).
###

### We deliberately do NOT extend IRanges.
setClass("NCList",
    contains="Ranges",
    representation(
        nclist="integer",
        ranges="IRanges"
    )
)

setMethod("length", "NCList", function(x) length(x@ranges))
setMethod("names", "NCList", function(x) names(x@ranges))
setMethod("start", "NCList", function(x, ...) start(x@ranges))
setMethod("end", "NCList", function(x, ...) end(x@ranges))
setMethod("width", "NCList", function(x) width(x@ranges))


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### .shift_ranges_to_first_circle() and
### .shift_ranges_in_groups_to_first_circle()
###
### TODO: Move to intra-range-methods.R, rename (e.g. shiftToFirstCircle()),
### make it a generic with methods for IRanges and IRangesList, export, and
### document.
###

### Returns a single integer.
.normarg_circle.length1 <- function(circle.length)
{
    msg <- "'circle.length' must be a single positive integer or NA"
    if (!isSingleNumberOrNA(circle.length))
        stop(msg)
    if (!is.integer(circle.length))
        circle.length <- as.integer(circle.length)
    if (!is.na(circle.length) && circle.length <= 0L)
        stop(msg)
    circle.length
}

### Returns an integer vector of length 'x_len'.
.normarg_circle.length2 <- function(circle.length, x_len, what)
{
    msg <- c("'circle.length' must be an integer vector ",
             "with positive or NA values")
    if (!is.atomic(circle.length))
        stop(msg)
    if (!(length(circle.length) == 1L || length(circle.length) == x_len))
        stop("'circle.length' must have length 1 or length of ", what)
    all_NAs <- all(is.na(circle.length))
    if (!(all_NAs || is.numeric(circle.length)))
        stop(msg)
    if (!is.integer(circle.length))
        circle.length <- as.integer(circle.length)
    if (!all_NAs && min(circle.length, na.rm=TRUE) <= 0L)
        stop(msg)
    if (length(circle.length) == x_len)
        return(circle.length)
    rep.int(circle.length, x_len)
}

### 'circle.length' assumed to have length 1 or length of 'x'.
.shift_ranges_to_first_circle <- function(x, circle.length)
{
    if (all(is.na(circle.length)))
        return(x)
    x_start0 <- start(x) - 1L  # 0-based start
    x_shift0 <- x_start0 %% circle.length - x_start0
    x_shift0[is.na(x_shift0)] <- 0L
    shift(x, x_shift0)
}

### 'length(circle.length)' assumed to be >= 'length(x_groups)'.
.shift_ranges_in_groups_to_first_circle <- function(x, x_groups, circle.length)
{
    circle.length <- head(circle.length, n=length(x_groups))
    if (all(is.na(circle.length)))
        return(x)
    unlisted_groups <- unlist(x_groups, use.names=FALSE)
    circle_len <- rep.int(NA_integer_, length(x))
    circle_len[unlisted_groups + 1L] <-
        rep.int(circle.length, elementNROWS(x_groups))
    .shift_ranges_to_first_circle(x, circle_len)
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### NCList constructor
###

### Returns an external pointer to the NCList C struct.
.NCList_xp <- function(x_start, x_end, x_subset)
{
    ans <- .Call2("NCList_new", PACKAGE="IRanges")
    reg.finalizer(ans,
        function(e) .Call("NCList_free", e, PACKAGE="IRanges")
    )
    .Call2("NCList_build", ans, x_start, x_end, x_subset, PACKAGE="IRanges")
}

.nclist <- function(x_start, x_end, x_subset=NULL)
{
    nclist_xp <- .NCList_xp(x_start, x_end, x_subset)
    .Call2("new_NCListAsINTSXP_from_NCList", nclist_xp, PACKAGE="IRanges")
}

NCList <- function(x, circle.length=NA_integer_)
{
    if (!is(x, "Ranges"))
        stop("'x' must be a Ranges object")
    if (!is(x, "IRanges"))
        x <- as(x, "IRanges")
    ans_mcols <- mcols(x)
    mcols(x) <- NULL
    circle.length <- .normarg_circle.length1(circle.length)
    x <- .shift_ranges_to_first_circle(x, circle.length)
    x_nclist <- .nclist(start(x), end(x))
    new2("NCList", nclist=x_nclist,
                   ranges=x,
                   elementMetadata=ans_mcols,
                   check=FALSE)
}

setAs("Ranges", "NCList", function(from) NCList(from))

### NOT exported.
print_NCList <- function(x)
{
    if (!is(x, "NCList"))
        stop("'x' must be an NCList object")
    .Call2("NCListAsINTSXP_print", x@nclist, start(x@ranges), end(x@ranges),
                                   PACKAGE="IRanges")
    invisible(NULL)
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### findOverlaps_NCList()
###

### NOT exported.
findOverlaps_NCList <- function(query, subject,
             maxgap=0L, minoverlap=1L,
             type=c("any", "start", "end", "within", "extend", "equal"),
             select=c("all", "first", "last", "arbitrary", "count"),
             circle.length=NA_integer_)
{
    if (!(is(query, "Ranges") && is(subject, "Ranges")))
        stop("'query' and 'subject' must be Ranges objects")

    if (!isSingleNumber(maxgap))
        stop("'maxgap' must be a single integer")
    if (!is.integer(maxgap))
        maxgap <- as.integer(maxgap)

    if (!isSingleNumber(minoverlap))
        stop("'minoverlap' must be a single integer")
    if (!is.integer(minoverlap))
        minoverlap <- as.integer(minoverlap)

    type <- match.arg(type)
    select <- match.arg(select)
    circle.length <- .normarg_circle.length1(circle.length)

    if (is(subject, "NCList")) {
        nclist <- subject@nclist
        nclist_is_q <- FALSE
        query <- .shift_ranges_to_first_circle(query, circle.length)
    } else if (is(query, "NCList")) {
        nclist <- query@nclist
        nclist_is_q <- TRUE
        subject <- .shift_ranges_to_first_circle(subject, circle.length)
    } else {
        ## We'll do "on-the-fly preprocessing".
        nclist <- NULL
        nclist_is_q <- NA
        query <- .shift_ranges_to_first_circle(query, circle.length)
        subject <- .shift_ranges_to_first_circle(subject, circle.length)
    }
    .Call2("NCList_find_overlaps",
           start(query), end(query),
           start(subject), end(subject),
           nclist, nclist_is_q,
           maxgap, minoverlap, type, select, circle.length,
           PACKAGE="IRanges")
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Representation of a list of NCList objects
###

setClass("NCLists",
    contains="RangesList",
    representation(
        nclists="list",
        rglist="CompressedIRangesList"
    ),
    prototype(
        elementType="NCList"
    )
)

setMethod("parallelSlotNames", "NCLists",
    function(x) c("nclists", "rglist", callNextMethod())
)

### TODO: Move rglist() generic from GenomicRanges to IRanges
#setMethod("rglist", "NCLists", function(x, ...) x@rglist)

setMethod("ranges", "NCLists",
    function(x, use.names=TRUE, use.mcols=FALSE)
    {
        if (!isTRUEorFALSE(use.names))
            stop("'use.names' must be TRUE or FALSE")
        if (!isTRUEorFALSE(use.mcols))
            stop("'use.mcols' must be TRUE or FALSE")
        ans <- x@rglist
        if (!use.names)
            names(ans) <- NULL
        if (use.mcols)
            mcols(ans) <- mcols(x)
        ans
    }
)

setMethod("length", "NCLists", function(x) length(x@rglist))
setMethod("names", "NCLists", function(x) names(x@rglist))
setMethod("start", "NCLists", function(x, ...) start(x@rglist))
setMethod("end", "NCLists", function(x, ...) end(x@rglist))
setMethod("width", "NCLists", function(x) width(x@rglist))

setMethod("elementNROWS", "NCLists", function(x) elementNROWS(x@rglist))
setMethod("getListElement", "NCLists",
    function (x, i, exact=TRUE)
    {
        i <- normalizeDoubleBracketSubscript(i, x, exact=exact,
                                             error.if.nomatch=TRUE)
        new2("NCList", nclist=x@nclists[[i]], ranges=x@rglist[[i]],
                       check=FALSE)
    }
)

setAs("NCLists", "CompressedIRangesList",
    function(from) ranges(from, use.mcols=TRUE)
)
setAs("NCLists", "IRangesList",
    function(from) ranges(from, use.mcols=TRUE)
)

.extract_groups_from_RangesList <- function(x)
{
    x_partitioning <- PartitioningByEnd(x)
    relist(as.integer(x_partitioning) - 1L, x_partitioning)
}

.nclists <- function(x, x_groups)
{
    x_start <- start(x)
    x_end <- end(x)
    lapply(x_groups, function(group) .nclist(x_start, x_end, x_subset=group))
}

### NCLists constructor.
NCLists <- function(x, circle.length=NA_integer_)
{
    if (!is(x, "RangesList"))
        stop("'x' must be a RangesList object")
    if (!is(x, "CompressedIRangesList"))
        x <- as(x, "CompressedIRangesList")
    ans_mcols <- mcols(x)
    mcols(x) <- NULL
    unlisted_x <- unlist(x, use.names=FALSE)
    x_groups <- .extract_groups_from_RangesList(x)
    circle.length <- .normarg_circle.length2(circle.length, length(x_groups),
                                             "'x'")
    unlisted_x <- .shift_ranges_in_groups_to_first_circle(
                                   unlisted_x,
                                   x_groups,
                                   circle.length)
    x <- relist(unlisted_x, x)
    x_nclists <- .nclists(unlisted_x, x_groups)
    new2("NCLists", nclists=x_nclists,
                    rglist=x,
                    elementMetadata=ans_mcols,
                    check=FALSE)
}

setAs("RangesList", "NCLists", function(from) NCLists(from))


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### NCList_find_overlaps_in_groups()
###

### NOT exported. Workhorse behind findOverlaps_NCLists() below and behind
### GenomicRanges:::findOverlaps_GNCList().
NCList_find_overlaps_in_groups <- function(
             q, q_space, q_groups,
             s, s_space, s_groups,
             nclists, nclist_is_q,
             maxgap=0L, minoverlap=1L,
             type=c("any", "start", "end", "within", "extend", "equal"),
             select=c("all", "first", "last", "arbitrary", "count"),
             circle.length)
{
    if (!(is(q, "Ranges") && is(s, "Ranges")))
        stop("'q' and 's' must be Ranges object")
    if (!is(q_groups, "CompressedIntegerList"))
        stop("'q_groups' must be a CompressedIntegerList object")
    if (!is(s_groups, "CompressedIntegerList"))
        stop("'s_groups' must be a CompressedIntegerList object")

    if (!isSingleNumber(maxgap))
        stop("'maxgap' must be a single integer")
    if (!is.integer(maxgap))
        maxgap <- as.integer(maxgap)

    if (!isSingleNumber(minoverlap))
        stop("'minoverlap' must be a single integer")
    if (!is.integer(minoverlap))
        minoverlap <- as.integer(minoverlap)

    type <- match.arg(type)
    select <- match.arg(select)

    q_circle_len <- circle.length
    q_circle_len[which(nclist_is_q)] <- NA_integer_
    q <- .shift_ranges_in_groups_to_first_circle(q, q_groups, q_circle_len)
    s_circle_len <- circle.length
    s_circle_len[which(!nclist_is_q)] <- NA_integer_
    s <- .shift_ranges_in_groups_to_first_circle(s, s_groups, s_circle_len)
    .Call2("NCList_find_overlaps_in_groups",
           start(q), end(q), q_space, q_groups,
           start(s), end(s), s_space, s_groups,
           nclists, nclist_is_q,
           maxgap, minoverlap, type, select, circle.length,
           PACKAGE="IRanges")
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### findOverlaps_NCLists()
###

.split_and_remap_hits <- function(all_hits, query, subject, select)
{
    ## Compute list element lengths and offsets for 'query'.
    query_partitioning <- PartitioningByEnd(query)
    query_eltNROWS <- width(query_partitioning)
    query_offsets <- start(query_partitioning) - 1L

    ## Compute list element lengths and offsets for 'subject'.
    subject_partitioning <- PartitioningByEnd(subject)
    subject_eltNROWS <- width(subject_partitioning)
    subject_offsets <- start(subject_partitioning) - 1L

    if (select != "all") {
        ans <- head(relist(all_hits, query), n=length(subject))
        if (select != "count")
            ans <- ans - head(subject_offsets, n=length(ans))
        return(ans)
    }

    q_hits <- queryHits(all_hits)
    query_breakpoints <- end(query_partitioning)
    h_skeleton <- PartitioningByEnd(findInterval(query_breakpoints, q_hits))
    lapply(seq_len(min(length(query), length(subject))),
           function(i) {
               hits <- all_hits[h_skeleton[[i]]]
               hits@from <- hits@from - query_offsets[[i]]
               hits@to <- hits@to - subject_offsets[[i]]
               hits@nLnode <- query_eltNROWS[[i]]
               hits@nRnode <- subject_eltNROWS[[i]]
               hits
           })
}

### NOT exported.
### Return an ordinary list of:
###   (a) SortedByQueryHits objects if 'select' is "all". In that case the
###       list has the length of the shortest of 'query' or 'subject'.
###   (b) integer vectors if 'select' is not "all". In that case the list is
###       parallel to and has the same shape as 'query'.
findOverlaps_NCLists <- function(query, subject,
             maxgap=0L, minoverlap=1L,
             type=c("any", "start", "end", "within", "extend", "equal"),
             select=c("all", "first", "last", "arbitrary", "count"),
             circle.length=NA_integer_)
{
    if (!(is(query, "RangesList") && is(subject, "RangesList")))
        stop("'query' and 'subject' must be RangesList objects")
    type <- match.arg(type)
    select <- match.arg(select)
    circle.length <- .normarg_circle.length2(circle.length,
                              max(length(query), length(subject)),
                              "longest of 'query' or 'subject'")
    if (is(subject, "NCLists")) {
        nclists <- subject@nclists
        nclist_is_q <- rep.int(FALSE, length(nclists))
        subject <- subject@rglist
    } else if (is(query, "NCLists")) {
        nclists <- query@nclists
        nclist_is_q <- rep.int(TRUE, length(nclists))
        query <- query@rglist
    } else {
        ## We'll do "on-the-fly preprocessing".
        NG <- min(length(query), length(subject))
        nclists <- vector(mode="list", length=NG)
        nclist_is_q <- rep.int(NA, length(nclists))
    }

    if (!is(query, "CompressedIRangesList"))
        query <- as(query, "CompressedIRangesList")
    q <- unlist(query, use.names=FALSE)
    q_groups <- .extract_groups_from_RangesList(query)

    if (!is(subject, "CompressedIRangesList"))
        subject <- as(subject, "CompressedIRangesList")
    s <- unlist(subject, use.names=FALSE)
    s_groups <- .extract_groups_from_RangesList(subject)

    all_hits <- NCList_find_overlaps_in_groups(
                        q, NULL, q_groups,
                        s, NULL, s_groups,
                        nclists, nclist_is_q,
                        maxgap, minoverlap, type, select, circle.length)
    .split_and_remap_hits(all_hits, query, subject, select)
}

