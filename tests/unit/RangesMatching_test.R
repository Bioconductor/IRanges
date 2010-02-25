test_RangesMatching_as_matrix <- function() {
  ## sparse
  query <- IRanges(c(1, 4, 9), c(5, 7, 10))
  subject <- IRanges(c(2, 2, 10), c(2, 3, 12))
  tree <- IntervalTree(subject)
  result <- findOverlaps(query, tree)

  checkIdentical(as.matrix(result),
                 cbind(query = c(1L, 1L, 3L),
                       subject = 1:3))

  ## dense
  query <- IRanges(c(1, 4, 9), c(5, 7, 10))
  subject <- IRanges(c(2, 2), c(5, 4))
  tree <- IntervalTree(subject)
  result <- findOverlaps(query, tree)

  checkIdentical(as.matrix(result),
                 cbind(query = rep(1:2, each=2),
                       subject = rep(1:2, 2)))
}

test_RangesMatching_matched <- function() {
  ## sparse
  query <- IRanges(c(1, 4, 9), c(5, 7, 10))
  subject <- IRanges(c(2, 2, 10), c(2, 3, 12))
  tree <- IntervalTree(subject)
  result <- findOverlaps(query, tree)

  checkIdentical(as.vector(as.table(result)), c(2L, 0L, 1L))
  checkIdentical(as.vector(as.table(t(result))), c(1L, 1L, 1L))

  ## dense
  query <- IRanges(c(1, 4, 9), c(5, 7, 10))
  subject <- IRanges(c(2, 2), c(5, 4))
  tree <- IntervalTree(subject)
  result <- findOverlaps(query, tree)

  checkIdentical(as.vector(as.table(result)), c(2L, 2L, 0L))
}
