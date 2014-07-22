### =========================================================================
### NCList objects
### -------------------------------------------------------------------------
###
### An S4 implementation of Nested Containment List (NCList).
###

setClass("NCList",
    contains="Ranges",
    representation(
        nclist="integer",
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
    x_nclist <- .Call("NCList_new", PACKAGE="IRanges")
    reg.finalizer(x_nclist,
        function(e) .Call("NCList_free", e, PACKAGE="IRanges")
    )
    C_ans <- .Call("NCList_build", x_nclist, start(x), end(x),
                                   PACKAGE="IRanges")
    new2("NCList", nclist=C_ans, ranges=x, check=FALSE)
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
### - NCList:       0.004 s / 6.9 s   / 408.3 Mb / 857m
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
### - NCList:       0.112 s /  0.06 s  / 64.5 Mb / 528m
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
### - NCList:       11.3 s /  4 s    / 1135.0 Mb / 1477m (1989m)
### - IntervalTree: 48.2 s / 13.25 s /  874.6 Mb / 2059m (3125m)
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
### - NCList:       11.6 s /  4.2 s  / 1139.8 Mb / 1574m (2277m)
### - IntervalTree: 48.6 s / 13.24 s /  874.6 Mb / 2059m (3125m)
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
### - NCList:       2.3 s / 3.4 s / 474.0 Mb / 708m (708m)
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

### TEST 6 (requires at least 16 Gb of RAM):
### - NCList:        30 s   / 35.5 s / 4222.0 Mb / 5494m (5891m)
### - IntervalTree: 106.4 s / 83.5 s / 3822.4 Mb / 4806m (11.2g)
library(IRanges)
N <- 50000000L  # nb of ranges
W <- 44000L     # max range width
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

