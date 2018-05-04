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

### Act as an endomorphism.
### Will work out-of-the box on any List derivative 'x' that supports [
### and windows() e.g. all the AtomicList derivatives, IRanges, GRanges,
### DNAStringSet, DNAStringSetList, GAlignments, GAlignmentsList objects
### and more...
splitListElements <- function(x, partitioning, use.mcols=FALSE)
{
    if (!isTRUEorFALSE(use.mcols))
        stop(wmsg("'use.mcols' must be TRUE or FALSE"))
    if (!is(x, "List")) {
        if (!is.list(x))
            stop(wmsg("'x' must be a list-like object"))
        if (!use.mcols)
            stop(wmsg("'use.mcols' must be set to TRUE ",
                      "when 'x' is an ordinary list"))
    }
    if (!is(partitioning, "Partitioning"))
        stop(wmsg("'partitioning' must be a Partitioning object"))
    x_partitioning <- PartitioningByEnd(x)
    if (nobj(x_partitioning) != nobj(partitioning))
        stop(wmsg("'partitioning' is incompatible with the cumulated ",
                  "length of all the list elements in 'x'"))
    ## Partitioning objects are a particular type of disjoint and strictly
    ## sorted IntegerRanges objects. Finding the overlaps between 2 such
    ## objects is a simple business that could be solved in linear time with
    ## dedicated code (the code would just walk along the 2 objects at the
    ## same time). This would be slightly more efficient than using
    ## findOverlaps() which is a little bit overkill for this (it builds a
    ## Nested Containment List object and uses a binary search on it).
    ## Also, in the case of disjoint and strictly sorted IntegerRanges objects,
    ## an important property of the Hits object representing the hits between
    ## the overlapping ranges is that it can always be sorted in such a way
    ## that **both** queryHits() and subjectHits() end up sorted in ascending
    ## order. However, the SortedByQueryHits object returned by findOverlaps()
    ## is only guaranteed to have its queryHits() sorted. We need to call
    ## sort() on it if we want to make sure that its subjectHits() is sorted
    ## too. If we were using our dedicated code for finding the overlaps
    ## between 2 disjoint and strictly sorted IntegerRanges objects, we
    ## wouldn't need to do that.
    hits <- sort(findOverlaps(x_partitioning, partitioning))
    revmap <- queryHits(hits)       # sorted
    partition <- subjectHits(hits)  # sorted
    ans <- x[revmap]
    if (!use.mcols)
        mcols(ans) <- DataFrame(revmap=revmap, partition=partition)
    Ltrim <- pmax(start(partitioning)[partition] -
                  start(x_partitioning)[revmap], 0L)
    Rtrim <- pmax(end(x_partitioning)[revmap] -
                  end(partitioning)[partition], 0L)
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
    partitioning <- breakInChunks(sum(x_eltNROWS),
                                  nchunk=nchunk, chunksize=chunksize)
    unlisted_ans <- splitListElements(x, partitioning)
    unlisted_ans_mcols <- mcols(unlisted_ans)
    revmap <- unlisted_ans_mcols[ , "revmap"]
    partition <- unlisted_ans_mcols[ , "partition"]
    if (use.mcols) {
        mcols(unlisted_ans) <- mcols(x)[revmap, , drop=FALSE]
    } else {
        mcols(unlisted_ans) <- DataFrame(revmap=revmap)
    }
    ans_partitioning <- PartitioningByEnd(partition, NG=length(partitioning))
    relist(unlisted_ans, ans_partitioning)
}

