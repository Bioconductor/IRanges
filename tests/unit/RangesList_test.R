test_RangesList_construction <- function() {
  empty <- RangesList()
  checkTrue(validObject(empty))
  checkIdentical(length(empty), 0L)
  range1 <- IRanges(start=c(1,2,3), end=c(5,2,8))
  range2 <- IRanges(start=c(15,45,20,1), end=c(15,100,80,5))
  named <- RangesList(one = range1, two = range2)
  checkTrue(validObject(named))
  checkIdentical(length(named), 2L)
  checkIdentical(start(named), start(c(range1, range2)))
  checkIdentical(end(named), end(c(range1, range2)))
  checkIdentical(width(named), width(c(range1, range2)))
  checkIdentical(names(named), c("one", "two"))
  checkIdentical(range1, named[[1]])
  unnamed <- RangesList(range1, range2)
  checkTrue(validObject(unnamed))
  checkIdentical(length(unnamed), 2L)
  checkIdentical(range2, unnamed[[2]])
  checkIdentical(names(unnamed), NULL)
}

test_RangesList_subset <- function() { ## by RangesList
  range1 <- IRanges(start=c(1,2,3), end=c(5,2,8))
  range2 <- IRanges(start=c(1,15,20,45), end=c(5,15,100,80))
  collection <- RangesList(one = range1, range2)
  checkIdentical(collection[RangesList()], RangesList(one=IRanges(), IRanges()))
  checkIdentical(collection[RangesList(IRanges(4, 6), IRanges(50, 70))],
                 RangesList(one=IRanges(c(1,3),c(5,8)),
                            IRanges(c(20,45),c(100,80))))
  checkIdentical(collection[RangesList(IRanges(50, 70), one=IRanges(4, 6))],
                 RangesList(one=IRanges(c(1,3),c(5,8)), IRanges()))
}

test_RangesList_as_list <- function() {
  range1 <- IRanges(start=c(1,2,3), end=c(5,2,8))
  range2 <- IRanges(start=c(15,45,20,1), end=c(15,100,80,5))
  checkIdentical(list(range1, range2),
                 as.list(RangesList(range1, range2)))
}

test_RangesList_gaps <- function() {
  range1 <- IRanges(start=c(1,2,3), end=c(5,2,8))
  range2 <- IRanges(start=c(15,45,20,1), end=c(15,100,80,5))
  collection <- RangesList(one = range1, range2)

  checkIdentical(gaps(collection), RangesList(gaps(range1), gaps(range2)))
}

test_RangesList_reduce <- function() {
  range1 <- IRanges(start=c(1,2,3), end=c(5,2,8))
  range2 <- IRanges(start=c(45,20,1), end=c(100,80,5))
  collection <- RangesList(one = range1, range2)

  checkIdentical(reduce(collection),
                 RangesList(asNormalIRanges(IRanges(c(1,20), c(8, 100)),
                                            force=FALSE)))

  collection <- RangesList(one = IntervalTree(range1),
                           two = IntervalTree(range2))
  checkIdentical(reduce(collection),
                 RangesList(asNormalIRanges(IRanges(c(1,20), c(8, 100)),
                                            force=FALSE)))
}

test_RangesList_range <- function() {
  rl <- RangesList(a = IRanges(c(1,2),c(4,3)), b = IRanges(c(4,6),c(10,7)))
  rl2 <- RangesList(c = IRanges(c(0,2),c(4,5)), a = IRanges(c(4,5),c(6,7)))
  ans <- RangesList(a = IRanges(1,7), b = IRanges(4,10), c = IRanges(0,5))
  checkIdentical(range(rl, rl2), ans)
  names(rl2) <- NULL
  ans <- RangesList(a = IRanges(0,5), b = IRanges(4,10))
  checkIdentical(range(rl, rl2), ans)
  ## must be same length
  checkException(range(RangesList(IRanges(c(1,2),c(4,3)), IRanges(3,5)),
                       RangesList(IRanges(2,6))), silent=TRUE) 
}

test_IRangesList_construction <- function() {
  range1 <- IRanges(start=c(1,2,3), end=c(5,2,8))
  range2 <- IRanges(start=c(15,45,20,1), end=c(15,100,80,5))
  named <- IRangesList(one = range1, two = range2)
  checkIdentical(length(named), 2L)
  checkIdentical(names(named), c("one", "two"))
  checkIdentical(range1, named[[1]])
  unnamed <- IRangesList(range1, range2)
  checkTrue(validObject(unnamed))
  checkIdentical(length(unnamed), 2L)
  checkIdentical(range2, unnamed[[2]])
  checkIdentical(names(unnamed), NULL)
}
