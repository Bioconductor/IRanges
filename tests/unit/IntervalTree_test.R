test_IntervalTree_construction <- function() {
  query <- IRanges(c(1, 3, 9), c(5, 7, 10))
  subject <- IRanges(c(2, 10), c(2, 12))
  
  tree <- IntervalTree(subject)
  checkTrue(validObject(tree))
  tree <- IntervalTree(IRanges())
  checkTrue(validObject(tree))
  
  checkException(IntervalTree(), silent = TRUE)
  checkException(IntervalTree(subject, query), silent = TRUE)
  checkException(IntervalTree(NULL), silent = TRUE)
}

library(IRanges)
library(RUnit)

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
  checkIdentical(result, c(2L, NA, 3L))

  checkOverlap <- function(a, q, s, r, c) {
    mat <- cbind(query = as.integer(q), subject = as.integer(s))
    checkIdentical(as.matrix(matchMatrix(a)), mat)
    checkIdentical(dim(a), as.integer(c(r, c)))
  }
  
  result <- overlap(tree, query)
  #sparse <- new("ngCMatrix", p = c(0L, 2L, 2L, 3L), i = c(0L, 1L, 2L),
  #              Dim = as.integer(c(length(subject), length(query))))
  checkOverlap(result, c(1, 1, 3), c(2, 1, 3), 3, 3)

  result <- overlap(tree, query, 1)
  ## sparse <- new("ngCMatrix", p = c(0L, 2L, 3L, 4L), i = c(0L, 1L, 1L, 2L),
  ##               Dim = as.integer(c(length(subject), length(query))))
  checkOverlap(result, c(1, 1, 2, 3), c(2, 1, 2, 3), 3, 3)

  ## empty query range
  query <- IRanges(c(1, 4, 9, 10), c(5, 7, 10, 9))
  result <- overlap(tree, query)
  ## sparse <- new("ngCMatrix", p = c(0L, 2L, 2L, 3L, 3L), i = c(0L, 1L, 2L),
  ##               Dim = as.integer(c(length(subject), length(query))))
  checkOverlap(result, c(1, 1, 3), c(2, 1, 3), 3, 4)

  ## empty subject range
  subject <- IRanges(c(2, 2, 2, 10), c(2, 1, 3, 12))
  tree <- IntervalTree(subject)
  result <- overlap(tree, query)
  ## sparse <- new("ngCMatrix", p = c(0L, 2L, 2L, 3L, 3L), i = c(0L, 2L, 3L),
  ##               Dim = as.integer(c(length(subject), length(query))))
  checkOverlap(result, c(1, 1, 3), c(3, 1, 4), 4, 4)
  
  ## .....
  ##    ....
  ##         ..
  ##  xxxx
  ##  xxx
  query <- IRanges(c(1, 4, 9), c(5, 7, 10))
  subject <- IRanges(c(2, 2), c(5, 4))
  
  tree <- IntervalTree(subject)
  result <- overlap(tree, query)
  ## dense <- new("lgeMatrix", x = c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE),
  ##              Dim = as.integer(c(length(subject), length(query))))
  checkOverlap(result, c(1, 1, 2, 2), c(2, 1, 2, 1), 2, 3)

  result <- overlap(query, subject)
  checkOverlap(result, c(1, 1, 2, 2), c(1, 2, 1, 2), 3, 2)
  ##checkOverlap(result, Matrix::t(dense))

  query <- IRanges(c(1, 4, 9, 11), c(5, 7, 10, 11))
  
  result <- overlap(query)
  ## sparse <- new("ngCMatrix", p = c(0L, 2L, 4L, 5L, 6L),
  ##               i = c(0L, 1L, 0L, 1L, 2L, 3L),
  ##               Dim = as.integer(c(length(query), length(query))))
  checkOverlap(result, c(1, 1, 2, 2, 3, 4), c(1, 2, 1, 2, 3, 4), 4, 4)

  ## check case of identical subjects
  ## .....
  ##    .....
  ##         ..
  ##  xxxx
  ##  xxxx
  ##      xx
  ##      xxx
  ##      xx
  query <- IRanges(c(1, 4, 9), c(5, 7, 10))
  subject <- IRanges(c(2, 2, 6, 6, 6), c(5, 5, 7, 8, 7))  
  tree <- IntervalTree(subject)
  result <- overlap(tree, query)
  ## sparse <- new("ngCMatrix", i = as.integer(c(0, 1, 0, 1, 2, 3, 4)),
  ##               p = as.integer(c(0, 2, 7, 7)), Dim = c(5L, 3L))
  checkOverlap(result, c(1, 1, 2, 2, 2, 2, 2), c(2, 1, 2, 1, 5, 4, 3), 5, 3)

  subject <- IRanges(c(1, 6, 13), c(4, 9, 14)) # single points
  checkIdentical(overlap(subject, c(3L, 7L, 10L), multiple=FALSE),
                 c(1L, 2L, NA))
  checkIdentical(overlap(subject, IRanges(c(2,1),c(3,4))),
                 new("RangesMatching",
                     matchMatrix = cbind(query=2:1, subject=c(1L,1L)), DIM = 3:2))

  checkException(overlap(NULL, query), silent = TRUE)
  checkException(overlap(query, NULL), silent = TRUE)
}

test_IntervalTree_asRanges <- function() {
  ranges <- IRanges(c(1, 4, 9), c(5, 7, 10))
  tree <- IntervalTree(ranges)
  checkIdentical(as(tree, "IRanges"), ranges)

  ranges <- IRanges()
  tree <- IntervalTree(ranges)
  checkIdentical(as(tree, "IRanges"), ranges)
}

test_IntervalTree_length <- function() {
  ranges <- IRanges(c(1, 4, 9), c(5, 7, 10))
  tree <- IntervalTree(ranges)
  checkIdentical(length(tree), length(ranges))
}
