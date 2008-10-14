test_RangesMatching_as_matrix <- function() {
  ## sparse
  query <- IRanges(c(1, 4, 9), c(5, 7, 10))
  subject <- IRanges(c(2, 2, 10), c(2, 3, 12))
  tree <- IntervalTree(subject)
  result <- overlap(tree, query)

  checkIdentical(as.matrix(result),
                 cbind(query = as.integer(c(1, 1, 3)),
                       subject = as.integer(c(1, 2, 3))))

  ## dense
  query <- IRanges(c(1, 4, 9), c(5, 7, 10))
  subject <- IRanges(c(2, 2), c(5, 4))
  tree <- IntervalTree(subject)
  result <- overlap(tree, query)

  checkIdentical(as.matrix(result),
                 cbind(query = as.integer(c(1, 1, 2, 2)),
                       subject = as.integer(c(1, 2, 1, 2))))
}

test_RangesMatching_matched <- function() {
  ## sparse
  query <- IRanges(c(1, 4, 9), c(5, 7, 10))
  subject <- IRanges(c(2, 2, 10), c(2, 3, 12))
  tree <- IntervalTree(subject)
  result <- overlap(tree, query)

  checkIdentical(matched(result), c(TRUE, FALSE, TRUE))
  checkIdentical(matched(t(result)), c(TRUE, TRUE, TRUE))

  ## dense
  query <- IRanges(c(1, 4, 9), c(5, 7, 10))
  subject <- IRanges(c(2, 2), c(5, 4))
  tree <- IntervalTree(subject)
  result <- overlap(tree, query)
  
  checkIdentical(matched(result), c(TRUE, TRUE, FALSE))
}
