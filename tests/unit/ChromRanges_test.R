test_ChromRanges_construction <- function() {
  ranges <- IRanges(c(1,2,3),c(4,5,6))
  score <- c(10L, 2L, NA)
  strand <- c("+", "-", NA)
  strandf <- factor(strand)
  
  checkIdenticalCR <- function(a, b)
    checkIdentical(as.data.frame(a), as.data.frame(b))
  
  cr <- ChromRanges()
  checkTrue(validObject(cr))
  checkIdenticalCR(cr, ValuedIRanges())
  cr <- ChromRanges(ranges)
  checkTrue(validObject(cr))
  checkIdenticalCR(cr, ValuedIRanges(ranges))
  cr <- ChromRanges(ranges, score = score)
  checkTrue(validObject(cr))
  checkIdenticalCR(cr, ValuedIRanges(ranges, score = score))
  cr <- ChromRanges(ranges, strand)
  checkTrue(validObject(cr))
  checkIdentical(strand(cr), strandf)
  cr <- ChromRanges(ranges, c("+", "x", "-")) # non-+/- treated as NA
  checkTrue(validObject(cr))
  checkIdentical(strand(cr), factor(c("+", NA, "-")))
  
  checkException(ChromRanges(strand = "+")) # wrong length
  checkException(ChromRanges(ranges, strand = c(1, 2, NA))) # wrong type
}
