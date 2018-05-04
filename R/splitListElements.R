### =========================================================================
### splitListElements()
### -------------------------------------------------------------------------


### Will work out-of-the box on any List derivative 'x' that supports [ and
### windows() e.g. all the AtomicList derivatives, IRanges, GRanges,
### DNAStringSet, DNAStringSetList, GAlignments, GAlignmentsList objects and
### more...
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
    x_eltNROWS <- elementNROWS(x)
    if (sum(x_eltNROWS) != nobj(partitioning))
        stop(wmsg("'partitioning' is incompatible with the cumulated ",
                  "length of all the list elements in 'x'"))
    q <- PartitioningByWidth(x_eltNROWS)
    ## findOverlaps() returns a SortedByQueryHits object which is only
    ## guaranteed to have its queryHits() sorted. By sorting the object,
    ## we make sure that its subjectHits() is sorted too.
    hits <- sort(findOverlaps(q, partitioning))
    revmap <- queryHits(hits)       # sorted
    partition <- subjectHits(hits)  # sorted
    ans <- x[revmap]
    if (!use.mcols)
        mcols(ans) <- DataFrame(revmap=revmap, partition=partition)
    Ltrim <- pmax(start(partitioning)[partition] - start(q)[revmap], 0L)
    Rtrim <- pmax(end(q)[revmap] - end(partitioning)[partition], 0L)
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

