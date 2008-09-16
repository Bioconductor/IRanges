
test_RangesCollection_construction <- function() {
  empty <- RangesCollection()
  checkIdentical(length(empty), 0)
  range1 <- IRanges(start=c(1,2,3), end=c(5,2,8))
  range2 <- IRanges(start=c(15,45,20,1), end=c(15,100,80,5))
  named <- RangesCollection(one = range1, two = range2)
  checkIdentical(length(named), 2)
  checkIdentical(names(named), c("one", "two"))
  checkIdentical(range1, named[[1]])
  unnamed <- RangesCollection(range1, range2)
  checkIdentical(length(unnamed), 2)
  checkIdentical(range2, unnamed[[2]])
  checkIdentical(names(unnamed), NULL)
}

test_RangesCollection_range_list <- function() {
  range1 <- IRanges(start=c(1,2,3), end=c(5,2,8))
  range2 <- IRanges(start=c(15,45,20,1), end=c(15,100,80,5))
  checkIdentical(list(range1, range2),
                 range_list(RangesCollection(range1, range2)))
}

test_RangesCollection_replace_names <- function() {
  range1 <- IRanges(start=c(1,2,3), end=c(5,2,8))
  range2 <- IRanges(start=c(15,45,20,1), end=c(15,100,80,5))
  collection <- RangesCollection(range1, range2)
  names(collection) <- c("one", "two")
  checkIdentical(names(collection), c("one", "two"))
  names(collection) <- NULL
  checkIdentical(names(collection), NULL)
  names(collection) <- "one"
  checkIdentical(names(collection), c("one", NA))
  checkException(names(collection) <- c("one", "two", "three"))
  checkException(names(collection) <- 4)
}

### TODO:

test_RangesCollection_subset <- function() {
}

test_RangesCollection_gaps <- function() {
}

test_RangesCollection_reduce <- function() {
}

test_RangesCollection_append <- function() {
}
