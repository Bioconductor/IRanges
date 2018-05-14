### =========================================================================
### extractListFragments()
### -------------------------------------------------------------------------


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### regroupBySupergroup()
###
### A very efficient way to concatenate groups of successive list elements
### in 'x'.
### 'x' must be a list-like object (typically a CompressedList object).
### 'supergroups' must be an object that defines a partitioning of
### 'seq_along(x)' (i.e. it could be used to do
### 'relist(seq_along(x), supergroups)'). It will be immediately replaced with
### 'PartitioningByEnd(supergroups)' so it should be an object that is
### accepted by the PartitioningByEnd() constructor (note that this constructor
### is a no-op if 'supergroups' is already a PartitioningByEnd object).
### Return a list-like object of the same elementType() as 'x' and parallel
### to 'supergroups'. The names on 'supergroups' are propagated but not the
### metadata columns.
###
### Some properties:
### - Behaves as an endomorphism on a CompressedList or PartitioningByEnd
###   object.
### - This
###       regroupBySupergroup(x, length(x))[[1L]]
###   is equivalent to
###       unlist(x, use.names=FALSE)
###
### Other possible names for regroupBySupergroup: regroup,
### mergeGroupsInSupergroups, combineGroupsOfListElements,
### unlistGroupsOfListElements, unlistBySupergroup.
###
### TODO: Maybe export and document this?

regroupBySupergroup <- function(x, supergroups)
{
    supergroups <- PartitioningByEnd(supergroups)
    x_breakpoints <- end(PartitioningByEnd(x))
    ans_breakpoints <- x_breakpoints[end(supergroups)]
    nleading0s <- length(supergroups) - length(ans_breakpoints)
    if (nleading0s != 0L)
        ans_breakpoints <- c(rep.int(0L, nleading0s), ans_breakpoints)
    ans_partitioning <- PartitioningByEnd(ans_breakpoints,
                                          names=names(supergroups))
    if (is(x, "PartitioningByEnd"))
        return(ans_partitioning)
    relist(unlist(x, use.names=FALSE), ans_partitioning)
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### extractListFragments()
###

INCOMPATIBLE_ARANGES_MSG <- c(
    "'aranges' is incompatible with the cumulated ",
    "length of all the list elements in 'x'"
)

### If 'hits.only' is FALSE (the default), return a Partitioning object of
### the same class as 'x' (endomorphism). Otherwise, return a list of 2
### integer vectors of the same length.
.extractPartitioningFragments_by_Partitioning <- function(x, aranges,
                              hits.only=FALSE,
                              msg.if.incompatible=INCOMPATIBLE_ARANGES_MSG)
{
    if (!is(x, "Partitioning"))
        stop(wmsg("'x' must be a Partitioning object"))
    if (!is(aranges, "Partitioning"))
        stop(wmsg("'aranges' must be a Partitioning object"))
    if (!isTRUEorFALSE(hits.only))
        stop(wmsg("'hits.only' must be TRUE or FALSE"))
    if (!is.character(msg.if.incompatible))
        stop(wmsg("'msg.if.incompatible' must be a character vector"))
    x_end <- end(x)
    aranges_end <- end(aranges)
    if (S4Vectors:::last_or(x_end, 0L) !=
        S4Vectors:::last_or(aranges_end, 0L))
        stop(wmsg(msg.if.incompatible))
    C_ans <- .Call2("find_partition_overlaps",
                    x_end, aranges_end, !hits.only, PACKAGE="IRanges")
    if (hits.only)
        return(C_ans)
    revmap <- C_ans[[1L]]
    revmap2 <- C_ans[[2L]]
    ans_names <- names(x)[revmap]
    ans <- new2("PartitioningByEnd", end=C_ans[[3L]], NAMES=ans_names,
                                     check=FALSE)
    ans <- as(ans, class(x))
    mcols(ans) <- DataFrame(revmap=revmap, revmap2=revmap2)
    ans
}

.extractListFragments_by_Partitioning <- function(x, aranges,
                      use.mcols=FALSE,
                      msg.if.incompatible=INCOMPATIBLE_ARANGES_MSG)
{
    if (is(x, "Partitioning")) {
        ans <- .extractPartitioningFragments_by_Partitioning(x, aranges,
                                   msg.if.incompatible=msg.if.incompatible)
        if (use.mcols) {
            revmap <- mcols(ans)[ , "revmap"]
            mcols(ans) <- mcols(x)[revmap, , drop=FALSE]
        }
        return(ans)
    }
    if (!is(x, "List")) {
        if (!is.list(x))
            stop(wmsg("'x' must be a list-like object"))
        if (!use.mcols)
            stop(wmsg("'use.mcols' must be set to TRUE ",
                      "when 'x' is an ordinary list"))
    }
    ## Will work out-of-the box on any List derivative 'x' that supports [
    ## and windows() e.g. all the AtomicList derivatives, IRanges, GRanges,
    ## DNAStringSet, DNAStringSetList, GAlignments, GAlignmentsList objects
    ## and more...
    x_partitioning <- PartitioningByEnd(x)
    hits <- .extractPartitioningFragments_by_Partitioning(
                                   x_partitioning, aranges,
                                   hits.only=TRUE,
                                   msg.if.incompatible=msg.if.incompatible)
    revmap <- hits[[1L]]
    revmap2 <- hits[[2L]]
    ans <- x[revmap]
    if (!use.mcols)
        mcols(ans) <- DataFrame(revmap=revmap, revmap2=revmap2)
    Ltrim <- pmax(start(aranges)[revmap2] -
                  start(x_partitioning)[revmap], 0L)
    Rtrim <- pmax(end(x_partitioning)[revmap] -
                  end(aranges)[revmap2], 0L)
    windows(ans, start=1L+Ltrim, end=-1L-Rtrim)
}

### Return a PartitioningByEnd object of length 2 * length(aranges) + 1.
.make_PartitioningByEnd_from_aranges <- function(aranges, x,
                                                 msg.if.incompatible)
{
    if (!is(aranges, "IntegerRanges"))
        stop(wmsg("'aranges' must be an IntegerRanges derivative ",
                  "(e.g. an IRanges object"))

    ## Check that 'aranges' is disjoint and sorted.
    ## This is the case if and only if 'start_end' is sorted. If 'aranges'
    ## is a NormalIRanges or Partitioning object, then it's disjoint and sorted
    ## so we can skip this check.
    start_end <- as.vector(
        rbind(start(aranges) - 1L, end(aranges), deparse.level=0)
    )
    if (!is(aranges, "NormalIRanges") &&
        !is(aranges, "Partitioning") &&
        S4Vectors:::isNotSorted(start_end))
        stop(wmsg("'aranges' must be disjoint and sorted"))

    ## Check that 'aranges' is compatible with 'x'.
    x_cumlen <- nobj(PartitioningByEnd(x))
    start_end_len <- length(start_end)  # = 2 * length(aranges)
    if (start_end_len >= 2L &&
        (start_end[[1L]] < 0L || start_end[[start_end_len]] > x_cumlen))
        stop(wmsg(msg.if.incompatible))

    ans_end <- c(start_end, x_cumlen)
    new2("PartitioningByEnd", end=ans_end, check=FALSE)
}

### Act as an endomorphism.
### 'x' must be a list-like object.
### 'aranges' must be an IntegerRanges object that is disjoint, sorted,
### and compatible with the cumulated length of all the list elements in 'x'.
extractListFragments <- function(x, aranges,
                            use.mcols=FALSE,
                            msg.if.incompatible=INCOMPATIBLE_ARANGES_MSG)
{
    if (!isTRUEorFALSE(use.mcols))
        stop(wmsg("'use.mcols' must be TRUE or FALSE"))
    if (is(aranges, "Partitioning")) {
        ans <- .extractListFragments_by_Partitioning(x, aranges,
                                     use.mcols=use.mcols,
                                     msg.if.incompatible=msg.if.incompatible)
        return(ans)
    }
    aranges <- .make_PartitioningByEnd_from_aranges(aranges, x,
                                               INCOMPATIBLE_ARANGES_MSG)
    ans <- .extractListFragments_by_Partitioning(x, aranges,
                                 msg.if.incompatible=msg.if.incompatible)
    revmap2 <- mcols(ans)[ , "revmap2"]
    ans <- ans[revmap2 %% 2L == 0L]
    if (use.mcols) {
        revmap <- mcols(ans)[ , "revmap"]
        mcols(ans) <- mcols(x)[revmap, , drop=FALSE]
    } else {
        mcols(ans)[ , "revmap2"] <- mcols(ans)[ , "revmap2"] %/% 2L
    }
    ans
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### equisplit()
###
### A simple wrapper to extractListFragments()
###

### Will work out-of-the box on any object 'x' that supports
### extractListFragments() **and relist()** e.g. IRanges, GRanges,
### DNAStringSet, GAlignments objects and more... Won't work on AtomicList
### derivatives or DNAStringSetList or GAlignmentsList objects because they
### don't support relist().
equisplit <- function(x, nchunk, chunksize, use.mcols=FALSE)
{
    if (!isTRUEorFALSE(use.mcols))
        stop(wmsg("'use.mcols' must be TRUE or FALSE"))
    x_cumlen <- nobj(PartitioningByEnd(x))
    aranges <- breakInChunks(x_cumlen, nchunk=nchunk, chunksize=chunksize)
    unlisted_ans <- extractListFragments(x, aranges)
    unlisted_ans_mcols <- mcols(unlisted_ans)
    revmap <- unlisted_ans_mcols[ , "revmap"]
    revmap2 <- unlisted_ans_mcols[ , "revmap2"]
    if (use.mcols) {
        mcols(unlisted_ans) <- mcols(x)[revmap, , drop=FALSE]
    } else {
        mcols(unlisted_ans) <- DataFrame(revmap=revmap)
    }
    ans_partitioning <- PartitioningByEnd(revmap2, NG=length(aranges))
    relist(unlisted_ans, ans_partitioning)
}

