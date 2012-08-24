test_Ranges_shift <- function() {
  ir1 <- IRanges(1:20, width=222000000)
  ## The returned object would have end values > INT_MAX
  checkException(shift(ir1, 1:20 * 99000000L))
}
 
test_Ranges_narrow <- function() {
  ir1 <- IRanges(c(2,5,1), c(3,7,3))
  checkIdentical(narrow(ir1, start=1, end=2),
                 IRanges(c(2, 5, 1), c(3, 6, 2)))
  checkException(narrow(ir1, start=10, end=20), silent = TRUE)
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

test_Ranges_reflect <- function() {
  ir1 <- IRanges(c(2,5,1), c(3,7,3))
  bounds <- IRanges(c(0, 5, 3), c(10, 6, 9))
  checkIdentical(reflect(ir1, bounds),
                 IRanges(c(7, 4, 9), c(8, 6, 11)))
  checkException(reflect(ir1, IRanges()), silent = TRUE)
}

test_Ranges_resize <- function() {
  ir1 <- IRanges(c(2,5,1), c(3,7,3))
  checkIdentical(resize(ir1, width=10),
                 IRanges(c(2, 5, 1), width=10))
  checkIdentical(resize(ir1, width=10, fix="end"),
                 IRanges(c(-6, -2, -6), width=10))
  checkIdentical(resize(ir1, width=10, fix="center"),
                 IRanges(c(-2, 1, -3), width=10))
  checkIdentical(resize(ir1, width=10, fix=c("start", "end", "center")),
                 IRanges(c(2, -2, -3), width=10))
  checkException(resize(ir1, -1), silent = TRUE)
}

test_Ranges_restrict <- function() {
  ir1 <- IRanges(c(2,5,1), c(3,7,3))
  checkIdentical(restrict(ir1, start=2, end=5),
                 IRanges(c(2, 5, 2), c(3, 5, 3)))
  checkIdentical(restrict(ir1, start=1, end=2),
                 IRanges(c(2, 1), c(2, 2)))
  checkIdentical(restrict(ir1, start=1, end=2, keep.all.ranges=TRUE),
                 IRanges(c(2, 3, 1), c(2, 2, 2)))
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

