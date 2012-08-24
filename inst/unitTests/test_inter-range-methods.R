test_Ranges_range <- function() {
  ir1 <- IRanges(c(2,5,1), c(3,7,3))
  ir2 <- IRanges(c(5,2,0), c(6,3,1))
  checkIdentical(range(ir1), IRanges(1, 7))
  checkIdentical(range(ir1, ir2), IRanges(0, 7))
  checkIdentical(range(IRanges()), IRanges())
  checkException(range(ir1, c(2,3)), silent = TRUE)
}

test_Ranges_isDisjoint <- function() {
  ir1 <- IRanges(c(2,5,1), c(3,7,3))
  ir2 <- IRanges(c(2,9,5), c(3,9,6))
  ir3 <- IRanges(1, 5)
  checkIdentical(isDisjoint(ir1), FALSE)
  checkIdentical(isDisjoint(ir2), TRUE)
  checkIdentical(isDisjoint(ir3), TRUE)
}

