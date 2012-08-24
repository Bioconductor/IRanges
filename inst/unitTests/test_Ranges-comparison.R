test_Ranges_order <- function() {
  ir1 <- IRanges(c(2,5,1,5), c(3,7,3,6))
  ir1.sort <- IRanges(c(1,2,5,5), c(3,3,6,7))
  ir1.rev <- IRanges(c(5,5,2,1), c(7,6,3,3))
  checkIdentical(sort(ir1), ir1.sort)
  checkIdentical(sort(ir1, decreasing=TRUE), ir1.rev)
  checkException(sort(ir1, decreasing=NA), silent = TRUE)
}

