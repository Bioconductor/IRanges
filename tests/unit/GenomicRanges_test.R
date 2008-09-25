test_GenomicRanges_construction <- function() {
  range1 <- ChromRanges(IRanges(start=c(1,2,3), end=c(5,2,8)))
  range2 <- ChromRanges(IRanges(start=c(15,45,20,1), end=c(15,100,80,5)))

  gr <- GenomicRanges(range1, range2)
  checkTrue(validObject(gr))
  checkIdentical(as(gr, "RangesList"), RangesList(range1, range2))
  gr <- GenomicRanges(range1, range2, genome = "hg18")
  checkTrue(validObject(gr))
  checkIdentical(genome(gr), "hg18")

  checkException(GenomicRanges(IRanges(start=c(1,2,3),end=c(5,2,8)), range2))
  checkException(GenomicRanges(range1, genome = c("hg18", "mm9")))
  checkException(GenomicRanges(range1, genome = 1))
}
