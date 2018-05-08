### =========================================================================
### splitListElements()
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
### splitListElements()
###

INCOMPATIBLE_SPLITTER_MSG <- c(
    "'splitter' is incompatible with the cumulated ",
    "length of all the list elements in 'x'"
)

### Return a Partitioning object of the same class as 'x' (endomorphism)
### if 'hits.only' is FALSE (the default). Otherwise, return a list of 2
### integer vectors of the same length.
.split_partitions <- function(x, splitter, hits.only=FALSE,
                              msg.if.incompatible=INCOMPATIBLE_SPLITTER_MSG)
{
    if (!is(x, "Partitioning"))
        stop(wmsg("'x' must be a Partitioning object"))
    if (!is(splitter, "Partitioning"))
        stop(wmsg("'splitter' must be a Partitioning object"))
    if (!isTRUEorFALSE(hits.only))
        stop(wmsg("'hits.only' must be TRUE or FALSE"))
    if (!is.character(msg.if.incompatible))
        stop(wmsg("'msg.if.incompatible' must be a character vector"))
    x_end <- end(x)
    splitter_end <- end(splitter)
    if (S4Vectors:::last_or(x_end, 0L) !=
        S4Vectors:::last_or(splitter_end, 0L))
        stop(wmsg(msg.if.incompatible))
    C_ans <- .Call2("find_partition_overlaps",
                    x_end, splitter_end, !hits.only, PACKAGE="IRanges")
    if (hits.only)
        return(C_ans)
    revmap <- C_ans[[1L]]
    revmap2 <- C_ans[[2L]]
    ans <- as(PartitioningByEnd(C_ans[[3L]], names=names(x)[revmap]), class(x))
    mcols(ans) <- DataFrame(revmap=revmap, revmap2=revmap2)
    ans
}

### Act as an endomorphism.
splitListElements <- function(x, splitter, use.mcols=FALSE,
                              msg.if.incompatible=INCOMPATIBLE_SPLITTER_MSG)
{
    if (!isTRUEorFALSE(use.mcols))
        stop(wmsg("'use.mcols' must be TRUE or FALSE"))
    if (is(x, "Partitioning")) {
        ans <- .split_partitions(x, splitter,
                                 msg.if.incompatible=msg.if.incompatible)
        if (use.mcols)
            mcols(ans) <- mcols(x)[mcols(ans)$revmap, , drop=FALSE]
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
    hits <- .split_partitions(x_partitioning, splitter, hits.only=TRUE,
                              msg.if.incompatible=msg.if.incompatible)
    revmap <- hits[[1L]]
    revmap2 <- hits[[2L]]
    ans <- x[revmap]
    if (!use.mcols)
        mcols(ans) <- DataFrame(revmap=revmap, revmap2=revmap2)
    Ltrim <- pmax(start(splitter)[revmap2] -
                  start(x_partitioning)[revmap], 0L)
    Rtrim <- pmax(end(x_partitioning)[revmap] -
                  end(splitter)[revmap2], 0L)
    windows(ans, start=1L+Ltrim, end=-1L-Rtrim)
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### equisplit()
###
### A simple wrapper to splitListElements()
###

### Will work out-of-the box on any object 'x' that supports
### splitListElements() **and relist()** e.g. IRanges, GRanges, DNAStringSet,
### GAlignments objects and more... Won't work on AtomicList derivatives or
### DNAStringSetList or GAlignmentsList objects because they don't support
### relist().
equisplit <- function(x, nchunk, chunksize, use.mcols=FALSE)
{
    if (!isTRUEorFALSE(use.mcols))
        stop(wmsg("'use.mcols' must be TRUE or FALSE"))
    x_eltNROWS <- elementNROWS(x)
    splitter <- breakInChunks(sum(x_eltNROWS),
                              nchunk=nchunk, chunksize=chunksize)
    unlisted_ans <- splitListElements(x, splitter)
    unlisted_ans_mcols <- mcols(unlisted_ans)
    revmap <- unlisted_ans_mcols[ , "revmap"]
    revmap2 <- unlisted_ans_mcols[ , "revmap2"]
    if (use.mcols) {
        mcols(unlisted_ans) <- mcols(x)[revmap, , drop=FALSE]
    } else {
        mcols(unlisted_ans) <- DataFrame(revmap=revmap)
    }
    ans_partitioning <- PartitioningByEnd(revmap2, NG=length(splitter))
    relist(unlisted_ans, ans_partitioning)
}

