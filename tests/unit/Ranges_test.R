test_Ranges_order <- function() {
  ir1 <- IRanges(c(2,5,1), c(3,7,3))
  ir1.sort <- IRanges(c(1,2,5), c(3,3,7))
  ir1.rev <- IRanges(c(5,2,1), c(7,3,3))
  checkIdentical(sort(ir1), ir1.sort)
  checkIdentical(sort(ir1, decreasing=TRUE), ir1.rev)
  checkException(sort(ir1, decreasing=NA), silent = TRUE)
}

test_Ranges_range <- function() {
  ir1 <- IRanges(c(2,5,1), c(3,7,3))
  ir2 <- IRanges(c(5,2,0), c(6,3,1))
  checkIdentical(range(ir1), IRanges(1, 7))
  checkIdentical(range(ir1, ir2), IRanges(0, 7))
  checkIdentical(range(IRanges()), IRanges())
  checkException(range(ir1, c(2,3)), silent = TRUE)
}

test_Ranges_reflect <- function() {
  ir1 <- IRanges(c(2,5,1), c(3,7,3))
  bounds <- IRanges(c(0, 5, 3), c(10, 6, 9))
  checkIdentical(reflect(ir1, bounds),
                 IRanges(c(7, 4, 9), c(8, 6, 11)))
  checkException(reflect(ir1, IRanges(0, 10)), silent = TRUE)
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
  checkException(flank(ir1, 2, both = c(TRUE, FALSE, TRUE)),
                 silent = TRUE) # not vectorized
  checkException(flank(ir1, 2, c(FALSE, TRUE, NA)), silent = TRUE)
  checkException(flank(ir1, NA), silent = TRUE)
}

test_Ranges_isDisjoint <- function() {
  ir1 <- IRanges(c(2,5,1), c(3,7,3))
  ir2 <- IRanges(c(2,9,5), c(3,9,6))
  ir3 <- IRanges(1, 5)
  checkIdentical(isDisjoint(ir1), FALSE)
  checkIdentical(isDisjoint(ir2), TRUE)
  checkIdentical(isDisjoint(ir3), TRUE)
}

test_Ranges_zoom <- function() {
  ir <- IRanges(c(1,5), c(3,10))
  checkIdentical(ir*1, ir)
  checkIdentical(ir*c(1,2), IRanges(c(1,6), c(3, 8)))
  checkIdentical(ir*-2, IRanges(c(-1,2), c(4, 13)))
  checkException(ir*NA_integer_, silent = TRUE)
  checkException(ir*numeric(), silent = TRUE)
  checkException(ir*c(1,2,1), silent = TRUE)
  checkException(ir[rep(1,3)]*c(1,2), silent = TRUE)
}

test_Ranges_adjacency <- function() {
  query <- IRanges(c(1, 3, 9), c(3, 7, 10))
  subject <- IRanges(c(3, 2, 10), c(3, 13, 12))
  
  checkIdentical(precede(query, subject), c(3L, 3L, NA))
  checkIdentical(precede(IRanges(), subject), integer())
  checkIdentical(precede(query, IRanges()), rep(NA_integer_, 3))
  checkIdentical(precede(query), c(3L, 3L, NA))
  
  checkIdentical(follow(query, subject), c(NA, NA, 1L))
  checkIdentical(follow(IRanges(), subject), integer())
  checkIdentical(follow(query, IRanges()), rep(NA_integer_, 3))
  checkIdentical(follow(query), c(NA, NA, 2L))
}

test_Ranges_nearest <- function() {
  query <- IRanges(c(1, 3, 9), c(2, 7, 10))
  subject <- IRanges(c(3, 5, 12), c(3, 6, 12))

  checkIdentical(nearest(query, subject), c(1L, 1L, 3L))
  checkIdentical(nearest(query), c(2L, 1L, 2L))
}

test_Ranges_collapse <- function() {
  checkIdentical(collapse(IRanges()), IRanges())
  ir <- IRanges(c(1, 1, 4, 10), c(6, 3, 8, 10))
  checkIdentical(collapse(ir), IRanges(c(1, 4, 7, 10), c(3, 6, 8, 10)))
}

test_Ranges_segregate <- function() {
  checkIdentical(segregate(IRanges()), RangesList())
  checkIdentical(segregate(IRanges(1, 5)), RangesList(IRanges(1, 5)))
  checkIdentical(segregate(IRanges(c(1, 3), c(5, 12))),
                 RangesList(IRanges(1, 5), IRanges(3, 12)))
  checkIdentical(segregate(IRanges(c(1, 3, 10), c(5, 12, 13))),
                 RangesList(IRanges(c(1, 10), c(5, 13)), IRanges(3, 12)))
  checkIdentical(segregate(IRanges(c(3, 1, 10), c(5, 12, 13))),
                 RangesList(IRanges(1, 12), IRanges(c(3, 10), c(5, 13))))
}
