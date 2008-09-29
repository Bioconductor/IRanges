test_IntervalTree_construction <- function() {
  query <- IRanges(c(1, 3, 9), c(5, 7, 10))
  subject <- IRanges(c(2, 10), c(2, 12))
  
  tree <- IntervalTree(subject)
  checkTrue(validObject(tree))
  tree <- IntervalTree(IRanges())
  checkTrue(validObject(tree))
  
  checkException(IntervalTree())
  checkException(IntervalTree(subject, query))
  checkException(IntervalTree(NULL))
}

test_IntervalTree_overlap <- function() {
  ## .....
  ##    ....
  ##         ..
  ##  x
  ##  xx
  ##          xxx
  query <- IRanges(c(1, 4, 9), c(5, 7, 10))
  subject <- IRanges(c(2, 2, 10), c(2, 3, 12))
  tree <- IntervalTree(subject)
  
  result <- overlap(tree, query, multiple = FALSE)
  checkIdentical(result, c(1L, NA, 3L))

  result <- overlap(tree, query)
  sparse <- new("ngCMatrix", p = c(0L, 2L, 2L, 3L), i = c(0L, 1L, 2L),
                Dim = as.integer(c(length(subject), length(query))))
  checkIdentical(result, new("RangesMatching", matchmatrix = sparse))

  ## .....
  ##    ....
  ##         ..
  ##  xxxx
  ##  xxx
  query <- IRanges(c(1, 4, 9), c(5, 7, 10))
  subject <- IRanges(c(2, 2), c(5, 4))
  
  tree <- IntervalTree(subject)
  result <- overlap(tree, query)
  dense <- new("lgeMatrix", x = c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE),
               Dim = as.integer(c(length(subject), length(query))))
  checkIdentical(result, new("RangesMatching", matchmatrix = dense))

  result <- overlap(query, subject)
  checkIdentical(result, new("RangesMatching", matchmatrix = Matrix::t(dense)))

  query <- IRanges(c(1, 4, 9, 11), c(5, 7, 10, 11))
  
  result <- overlap(query)
  sparse <- new("ngCMatrix", p = c(0L, 2L, 4L, 5L, 6L),
                i = c(0L, 1L, 0L, 1L, 2L, 3L),
                Dim = as.integer(c(length(query), length(query))))
  checkIdentical(result, new("RangesMatching", matchmatrix=sparse))
  
  checkException(overlap(NULL, query))
  checkException(overlap(query, NULL))
}

test_IntervalTree_asRanges <- function() {
  ranges <- IRanges(c(1, 4, 9), c(5, 7, 10))
  tree <- IntervalTree(ranges)
  checkIdentical(as(tree, "Ranges"), ranges)

  ranges <- IRanges()
  tree <- IntervalTree(ranges)
  checkIdentical(as(tree, "Ranges"), ranges)
}

test_IntervalTree_length <- function() {
  ranges <- IRanges(c(1, 4, 9), c(5, 7, 10))
  tree <- IntervalTree(ranges)
  checkIdentical(length(tree), length(ranges))
}
