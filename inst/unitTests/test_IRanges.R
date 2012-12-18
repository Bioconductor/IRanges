
test_IRanges_names <- function() {
  range1 <- IRanges(start=c(1,2,3), end=c(5,2,8))
  
  checkIdentical(names(range1), NULL)
  nms <- c("a", NA, "b")
  names(range1) <- nms
  checkIdentical(names(range1), nms)
  checkTrue(validObject(nms))
  names(range1) <- NULL
  checkTrue(validObject(nms))
  checkIdentical(names(range1), NULL)
  names(range1) <- "a"
  checkTrue(validObject(range1))
  checkIdentical(names(range1), c("a", NA, NA))

  checkException(names(range1) <- c("a", "b", "c", "d"), silent = TRUE)
}

test_IRanges_combine <- function() {
  range <- IRanges(start=c(1,2,3,1), end=c(5,2,8,3))
  srange <- split(range, start(range) == 1)
  checkIdentical(srange,
                 as(RangesList(`FALSE` = range[2:3], `TRUE` = range[c(1,4)]),
                    "CompressedIRangesList"))
  checkIdentical(do.call(c, unname(as.list(srange))),
                 IRanges(c(2,3,1,1), c(2,8,5,3)))

  ir1 <- IRanges(1, 10)
  ir2 <- IRanges(c(1, 15), width=5)
  mcols(ir2) <- DataFrame(score=1:2)
  checkIdentical(mcols(c(ir1, ir2)),
                 DataFrame(score = c(NA, TRUE, TRUE)))
  
  ## Combining multiple IRanges object with varying mcols
  mcols(ir1) <- DataFrame(gc=0.78)
  checkException(c(ir1, ir2), silent=TRUE)
  checkIdentical(mcols(c(ir1, ir2, ignore.mcols=TRUE)), NULL)
}

test_IRanges_subset <- function() { # by range
  query <- IRanges(c(1, 4, 9), c(5, 7, 10))
  subject <- IRanges(c(6, 8, 10), c(7, 12, 14))
  checkIdentical(subsetByOverlaps(query, subject), query[2:3])
}

test_IRanges_annotation <- function() {
  range <- IRanges(c(1, 4), c(5, 7))
  mcols(range) <- DataFrame(a = 1:2)
  checkIdentical(mcols(range)[,1], 1:2)
  checkIdentical(mcols(range[2:1])[,1], 2:1)
  checkIdentical(mcols(c(range,range))[,1], rep(1:2,2))
}

test_IRanges_coverage <- function() {
  ir <- IRanges(c(1, 8, 14, 15, 19, 34, 40), width = c(12, 6, 6, 15, 6, 2, 7))
  checkIdentical(as.vector(coverage(ir)),
                 rep(c(1L, 2L, 1L, 2L, 3L, 2L, 1L, 0L, 1L, 0L, 1L),
                     c(7, 5, 2, 4, 1, 5, 5, 4, 2, 4, 7)))
  ir <- IRanges(start=c(-2L, 6L, 9L, -4L, 1L, 0L, -6L, 10L),
                width=c( 5L, 0L, 6L,  1L, 4L, 3L,  2L,  3L))
  checkIdentical(as.vector(coverage(ir)),
                 rep(c(3L, 1L, 0L, 1L, 2L, 1L), c(2, 2, 4, 1, 3, 2)))
  checkIdentical(as.vector(coverage(ir, shift=7)),
                 rep(c(1L, 0L, 1L, 2L, 3L, 1L, 0L, 1L, 2L, 1L),
                     c(3, 1, 2, 1, 2, 2, 4, 1, 3, 2)))
  checkIdentical(as.vector(coverage(ir, shift=7, width=27)),
                 rep(c(1L, 0L, 1L, 2L, 3L, 1L, 0L, 1L, 2L, 1L, 0L),
                     c(3, 1, 2, 1, 2, 2, 4, 1, 3, 2, 6)))
}
