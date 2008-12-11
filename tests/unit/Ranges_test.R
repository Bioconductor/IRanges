test_Ranges_order <- function() {
  ir1 <- IRanges(c(2,5,1), c(3,7,3))
  ir1.sort <- IRanges(c(1,2,5), c(3,3,7))
  ir1.rev <- IRanges(c(5,2,1), c(7,3,3))
  checkIdentical(sort(ir1), ir1.sort)
  checkIdentical(sort(ir1, decreasing=TRUE), ir1.rev)
  checkException(sort(ir1, decreasing=NA))
}

test_Ranges_range <- function() {
  ir1 <- IRanges(c(2,5,1), c(3,7,3))
  ir2 <- IRanges(c(5,2,0), c(6,3,1))
  checkIdentical(range(ir1), IRanges(1, 7))
  checkIdentical(range(ir1, ir2), IRanges(0, 7))
  checkIdentical(range(IRanges()), IRanges())
  checkException(range(ir1, c(2,3)))
}

test_Ranges_reflect <- function() {
  ir1 <- IRanges(c(2,5,1), c(3,7,3))
  bounds <- IRanges(c(0, 5, 3), c(10, 6, 9))
  checkIdentical(reflect(ir1, bounds),
                 IRanges(c(7, 4, 9), c(8, 6, 11)))
  checkException(reflect(ir1, IRanges(0, 10)))
}

test_Ranges_flank <- function() {
  ir1 <- IRanges(c(2,5,1), c(3,7,3))
  checkIdentical(flank(ir1, 2), IRanges(c(0, 3, -1), c(1, 4, 0)))
  checkIdentical(flank(ir1, 2, FALSE), IRanges(c(4, 8, 4), c(5, 9, 5)))
  checkIdentical(flank(ir1, 2, c(FALSE, TRUE, FALSE)),
                 IRanges(c(4, 3, 4), c(5, 4, 5)))
  checkIdentical(flank(ir1, c(2, -2, 2)), IRanges(c(0, 5, -1), c(1, 6, 0)))
  checkIdentical(flank(ir1, 2, both = TRUE), IRanges(c(0, 3, -1), c(3, 6, 2)))
  checkIdentical(flank(ir1, 2, FALSE, TRUE), IRanges(c(2, 6, 2), c(5, 9, 5)))
  checkIdentical(flank(ir1, -2, FALSE, TRUE), IRanges(c(2, 6, 2), c(5, 9, 5)))
  checkException(flank(ir1, 2, both = c(TRUE, FALSE, TRUE))) # not vectorized
  checkException(flank(ir1, 2, c(FALSE, TRUE, NA)))
  checkException(flank(ir1, NA))
}

test_Ranges_isDisjoint <- function() {
  ir1 <- IRanges(c(2,5,1), c(3,7,3))
  ir2 <- IRanges(c(2,9,5), c(3,9,6))
  ir3 <- IRanges(1, 5)
  checkIdentical(isDisjoint(ir1), FALSE)
  checkIdentical(isDisjoint(ir2), TRUE)
  checkIdentical(isDisjoint(ir3), TRUE)
}
