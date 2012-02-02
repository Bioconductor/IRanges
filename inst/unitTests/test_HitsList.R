test_HitsList_as_matrix <- function() {
  x <- RangedData(IRanges(start=c(1,6), end=c(5,10)), space=c("chr1","chr2"))
  y <- RangedData(IRanges(start=8, end=10), space="chr2")
  checkIdentical(as.matrix(findOverlaps(x, y)),
                 cbind(queryHits = 2L, subjectHits = 1L))
}
