### NOTE: TypedList is an abstract type, so we just test with RangesList

test_TypedList_replace_names <- function() {
  range1 <- IRanges(start=c(1,2,3), end=c(5,2,8))
  range2 <- IRanges(start=c(15,45,20,1), end=c(15,100,80,5))
  collection <- RangesList(range1, range2)
  names(collection) <- c("one", "two")
  checkIdentical(names(collection), c("one", "two"))
  names(collection) <- NULL
  checkIdentical(names(collection), NULL)
  names(collection) <- "one"
  checkIdentical(names(collection), c("one", NA))
  checkException(names(collection) <- c("one", "two", "three"))
  checkException(names(collection) <- 4)
}

test_TypedList_extraction <- function() {
  range1 <- IRanges(start=c(1,2,3), end=c(5,2,8))
  range2 <- IRanges(start=c(15,45,20,1), end=c(15,100,80,5))
  collection <- RangesList(range1, range2)
  
  checkException(collection[[]])
  checkException(collection[[1, 2]])
  checkException(collection[[numeric()]])
  checkException(collection[[NULL]])
  checkException(collection[[c(1,2)]])
  checkException(collection[[-1]])
  checkException(collection[[5]])
  
  checkIdentical(collection[[NA_integer_]], NULL)
  checkIdentical(collection[[1]], range1)
  checkIdentical(collection[[2]], range2)
  checkIdentical(collection[["1"]], NULL)
  checkIdentical(RangesList(one=range1, range2)[["one"]], range1)
}

test_TypedList_subset <- function() {
  range1 <- IRanges(start=c(1,2,3), end=c(5,2,8))
  range2 <- IRanges(start=c(15,45,20,1), end=c(15,100,80,5))
  collection <- RangesList(one = range1, range2)

  checkException(collection[1,2])
  checkException(collection[list()])
  checkException(collection[-3])
  checkException(collection[5])
  checkException(collection[c(NA, 2)])
  checkException(collection[c(TRUE, TRUE, TRUE)])
  checkException(collection[c(-1,2)])
  unnamed <- RangesList(range1, range2)
  checkException(unnamed["one"])

  empty <- RangesList()
  names(empty) <- character(0)
  checkIdentical(collection[numeric()], empty)
  checkIdentical(collection[logical()], empty)
  checkIdentical(collection[NULL], empty)
  checkIdentical(collection[], collection)
  checkIdentical(collection[FALSE], empty)
  checkIdentical(collection[c(FALSE, FALSE)], empty)
  checkIdentical(collection[TRUE], collection)
  checkIdentical(collection[c(TRUE, FALSE)], RangesList(one = range1))
  rl2 <- RangesList(range2)
  names(rl2) <- ""
  checkIdentical(collection[2], rl2)
  checkIdentical(collection[c(2,1)], RangesList(range2, one = range1))
  checkIdentical(collection[-1], rl2)
  checkIdentical(collection["one"], RangesList(one = range1))
}

test_TypedList_combine <- function() {
  range1 <- IRanges(start=c(1,2,3), end=c(5,2,8))
  range2 <- IRanges(start=c(15,45,20,1), end=c(15,100,80,5))
  col1 <- RangesList(one = range1, range2)
  col2 <- RangesList(two = range2, one = range1)
  col3 <- RangesList(range2)

  checkException(append(col1, col2, c(1,2,3)))
  checkException(append(col1, col2, col3))

  checkIdentical(append(col1, col2),
                 RangesList(one = range1, range2, two = range2,
                                  one = range1))
  checkIdentical(append(col1, col2, 1),
                 RangesList(one = range1, two = range2, one = range1,
                                  range2))
  checkIdentical(append(col1, col2, -5),
                 RangesList(two = range2, one = range1, one = range1,
                                  range2))
  checkIdentical(append(append(col1, col2), col3),
                 RangesList(one = range1, range2, two = range2,
                                  one = range1, range2))

  ## for 'c'
  checkIdentical(c(col1, col2, col3),
                 RangesList(one = range1, range2, two = range2, one = range1,
                            range2))
  checkException(c(col1, range2))
  checkException(c(col1, col2, recursive=TRUE))
}

test_TypedList_apply <- function() {
  range1 <- IRanges(start=c(1,2,3), end=c(5,2,8))
  range2 <- IRanges(start=c(15,45,20,1), end=c(15,100,80,5))
  col1 <- RangesList(one = range1, range2)

  checkIdentical(lapply(col1, start), list(start(range1), start(range2)))
  checkException(lapply(col1, 2))
}
