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
  checkIdentical(names(named), c("one", "two"))
  checkIdentical(range1, named[[1]])
  unnamed <- RangesList(range1, range2)
  checkTrue(validObject(unnamed))
  checkIdentical(length(unnamed), 2L)
  checkIdentical(range2, unnamed[[2]])
  checkIdentical(names(unnamed), NULL)
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
