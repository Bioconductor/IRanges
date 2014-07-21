### =========================================================================
### NCList objects
### -------------------------------------------------------------------------
###
### An S4 implementation of Nested Containment List (NCList).
###

setClass("NCList",
    contains="Ranges",
    representation(
        nclist="externalptr",
        ranges="IRanges"
    )
)

setMethod("length", "NCList", function(x) length(x@ranges))

setMethod("start", "NCList", function(x, ...) start(x@ranges))
setMethod("end", "NCList", function(x, ...) end(x@ranges))
setMethod("width", "NCList", function(x) width(x@ranges))
setMethod("names", "NCList", function(x) names(x@ranges))

### Usage:
###   x <- IRanges(c(11, 10, 13, 10, 14,  8, 10, 11),
###                c(15, 12, 18, 13, 14, 12, 15, 15))
###   subject <- NCList(x)
NCList <- function(x)
{
    if (!is(x, "Ranges"))
        stop("'x' must be a Ranges object")
    if (!is(x, "IRanges"))
        x <- as(x, "IRanges")
    ans_nclist <- .Call("NCList_build", start(x), end(x), PACKAGE="IRanges")
    reg.finalizer(ans_nclist,
        function(e) .Call("NCList_free", e, PACKAGE="IRanges")
    )
    new2("NCList", nclist=ans_nclist, ranges=x, check=FALSE)
}

### NOT exported.
print_NCList <- function(x)
{
    if (!is(x, "NCList"))
        stop("'x' must be an NCList object")
    .Call("NCList_print", x@nclist, start(x@ranges), end(x@ranges),
                          PACKAGE="IRanges")
    invisible(NULL)
}

### NOT exported.
findOverlaps_NCList <- function(query, subject)
{
    if (!is(query, "Ranges"))
        stop("'query' must be a Ranges object")
    if (!is(subject, "NCList"))
        stop("'subject' must be an NCList object")
    .Call("NCList_find_overlaps", start(query), end(query),
                                  subject@nclist,
                                  start(subject@ranges), end(subject@ranges),
                                  PACKAGE="IRanges")
}

if (FALSE) {  #     <<<--- begin testing findOverlaps_NCList() --->>>

### TEST 1:
### - NCList:       0.004 s / 7.115 s / 408.2 Mb / 858m
### - IntervalTree: 0.004 s / 0.180 s /  65.0 Mb / 508m
library(RNAseqData.HNRNPC.bam.chr14)
library(GenomicAlignments)
bamfile <- RNAseqData.HNRNPC.bam.chr14_BAMFILES[1]
gal <- readGAlignmentsFromBam(bamfile)
reads <- ranges(gal)
library(TxDb.Hsapiens.UCSC.hg19.knownGene)
ex <- exons(TxDb.Hsapiens.UCSC.hg19.knownGene)
seqlevels(ex, force=TRUE) <- "chr14"
ex14 <- ranges(ex)

system.time(subject <- NCList(ex14))
gc()
system.time(hits1 <- IRanges:::findOverlaps_NCList(reads, subject))
gc()

system.time(subject <- IntervalTree(ex14))
gc()
system.time(hits1b <- findOverlaps(reads, subject))
gc()

### TEST 2: Same as TEST 1 but switch query and subject.
### - NCList:       0.095 s /  0.074 s / 63.8 Mb / 619m
### - IntervalTree: 0.138 s /  0.236 s / 63.8 Mb / 540m
system.time(subject <- NCList(reads))
gc()
system.time(hits2 <- IRanges:::findOverlaps_NCList(ex14, subject))
gc()

system.time(subject <- IntervalTree(reads))
gc()
system.time(hits2b <- findOverlaps(ex14, subject))
gc()

### TEST 3: NO duplicate ranges in subject!
### - NCList:        8.9 s /  4.4 s  / 944.3 Mb / 1745m
### - IntervalTree: 48.2 s / 13.25 s / 874.6 Mb / 2059m
library(IRanges)
N <- 25000000L  # nb of ranges
W <- 180L       # range width
start <- 1L
end <- 250000000L
set.seed(777)
range_starts <- sample(end-W+1L, N, replace=FALSE)
range_widths <- rep.int(W, N)
x <- IRanges(start=range_starts, width=range_widths)
query <- successiveIRanges(rep(10000, 25010), from=-50000)

system.time(subject <- NCList(x))
gc()
system.time(hits3 <- IRanges:::findOverlaps_NCList(query, subject))
gc()

system.time(subject <- IntervalTree(x))
gc()
system.time(hits3b <- findOverlaps(query, subject))
gc()

### TEST 4: Same as TEST 3 but with duplicate ranges in subject.
### - NCList:        9.4 s /  4.6 s  / 944.5 Mb / 2057m
### - IntervalTree: 48.6 s / 13.24 s / 874.6 Mb / 2059m
library(IRanges)
N <- 25000000L  # nb of ranges
W <- 180L       # range width
start <- 1L
end <- 250000000L
set.seed(777)
range_starts <- sample(end-W+1L, N, replace=TRUE)
range_widths <- rep.int(W, N)
x <- IRanges(start=range_starts, width=range_widths)
query <- successiveIRanges(rep(10000, 25010), from=-50000)

system.time(subject <- NCList(x))
gc()
system.time(hits4 <- IRanges:::findOverlaps_NCList(query, subject))
gc()

system.time(subject <- IntervalTree(x))
gc()
system.time(hits4b <- findOverlaps(query, subject))
gc()

### TEST 5:
### - NCList:       1.9 s / 3.7 s / 426.2 Mb / 875m (961m)
### - IntervalTree:   7 s / 5.9 s / 417.6 Mb / 640m (1221m)
library(IRanges)
N <- 5000000L  # nb of ranges
W <- 50000L    # max range width
start <- 1L
end <- 125000000L
set.seed(777)
range_starts <- sample(end-W+1L, N, replace=TRUE)
range_widths <- sample(W, N, replace=TRUE)
x <- IRanges(start=range_starts, width=range_widths)
query <- successiveIRanges(rep(10000, 12510), from=-50000)

system.time(subject <- NCList(x))
gc()
system.time(hits5 <- IRanges:::findOverlaps_NCList(query, subject))
gc()

library(digest)
stopifnot(identical(digest(hits5), "9cffe9ea1288cc60211afe044bd8e525"))

system.time(subject <- IntervalTree(x))
gc()
system.time(hits5b <- findOverlaps(query, subject))
gc()
stopifnot(identical(hits5, hits5b))

### TEST 6:
### - NCList:       14.06 s / 22.6 s / 2930.3 Mb / 4263m (5931m)
### - IntervalTree: 60 s    / 44.6 s / 2468.0 Mb / 3009m (7414m)
library(IRanges)
N <- 30000000L  # nb of ranges
W <- 50000L    # max range width
start <- 1L
end <- 125000000L
set.seed(777)
range_starts <- sample(end-W+1L, N, replace=TRUE)
range_widths <- sample(W, N, replace=TRUE)
x <- IRanges(start=range_starts, width=range_widths)
query <- successiveIRanges(rep(10000, 12510), from=-50000)

system.time(subject <- NCList(x))
gc()
system.time(hits6 <- IRanges:::findOverlaps_NCList(query, subject))
gc()

system.time(subject <- IntervalTree(x))
gc()
system.time(hits6b <- findOverlaps(query, subject))
gc()

}

