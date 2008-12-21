### NOTE: TypedList is an abstract type, so we just test with IntegerList

test_TypedList_replace_names <- function() {
  int1 <- c(1L,2L,3L,5L,2L,8L)
  int2 <- c(15L,45L,20L,1L,15L,100L,80L,5L)
  collection <- IntegerList(int1, int2)
  names(collection) <- c("one", "two")
  checkIdentical(names(collection), c("one", "two"))
  names(collection) <- NULL
  checkIdentical(names(collection), NULL)
  names(collection) <- "one"
  checkIdentical(names(collection), c("one", NA))
  checkException(names(collection) <- c("one", "two", "three"), silent = TRUE)
}

test_TypedList_extraction <- function() {
  int1 <- c(1L,2L,3L,5L,2L,8L)
  int2 <- c(15L,45L,20L,1L,15L,100L,80L,5L)
  collection <- IntegerList(int1, int2)
  
  checkException(collection[[]], silent = TRUE)
  checkException(collection[[1, 2]], silent = TRUE)
  checkException(collection[[numeric()]], silent = TRUE)
  checkException(collection[[NULL]], silent = TRUE)
  checkException(collection[[c(1,2)]], silent = TRUE)
  checkException(collection[[-1]], silent = TRUE)
  checkException(collection[[5]], silent = TRUE)
  
  checkIdentical(collection[[NA_integer_]], NULL)
  checkIdentical(collection[[1]], int1)
  checkIdentical(collection[[2]], int2)
  checkIdentical(collection[["1"]], NULL)
  checkIdentical(collection$foo, NULL)
  checkIdentical(IntegerList(one=int1, int2)[["one"]], int1)
  checkIdentical(IntegerList(one=int1, int2)$one, int1)
}

test_TypedList_subset <- function() {
  int1 <- c(1L,2L,3L,5L,2L,8L)
  int2 <- c(15L,45L,20L,1L,15L,100L,80L,5L)
  collection <- IntegerList(one = int1, int2)

  checkException(collection[1,2], silent = TRUE)
  checkException(collection[list()], silent = TRUE)
  checkException(collection[-3], silent = TRUE)
  checkException(collection[5], silent = TRUE)
  checkException(collection[c(NA, 2)], silent = TRUE)
  checkException(collection[c(TRUE, TRUE, TRUE)], silent = TRUE)
  checkException(collection[c(-1,2)], silent = TRUE)
  unnamed <- IntegerList(int1, int2)
  checkException(unnamed["one"], silent = TRUE)

  empty <- IntegerList()
  names(empty) <- character(0)
  checkIdentical(collection[numeric()], empty)
  checkIdentical(collection[logical()], empty)
  checkIdentical(collection[NULL], empty)
  checkIdentical(collection[], collection)
  checkIdentical(collection[FALSE], empty)
  checkIdentical(collection[c(FALSE, FALSE)], empty)
  checkIdentical(collection[TRUE], collection)
  checkIdentical(collection[c(TRUE, FALSE)], IntegerList(one = int1))
  rl2 <- IntegerList(int2)
  names(rl2) <- ""
  checkIdentical(collection[2], rl2)
  checkIdentical(collection[c(2,1)], IntegerList(int2, one = int1))
  checkIdentical(collection[-1], rl2)
  checkIdentical(collection["one"], IntegerList(one = int1))
}

test_TypedList_combine <- function() {
  int1 <- c(1L,2L,3L,5L,2L,8L)
  int2 <- c(15L,45L,20L,1L,15L,100L,80L,5L)
  col1 <- IntegerList(one = int1, int2)
  col2 <- IntegerList(two = int2, one = int1)
  col3 <- IntegerList(int2)

  checkException(append(col1, col2, c(1,2,3)), silent = TRUE)
  checkException(append(col1, col2, col3), silent = TRUE)

  checkIdentical(append(col1, col2),
                 IntegerList(one = int1, int2, two = int2,
                             one = int1))
  checkIdentical(append(col1, col2, 1),
                 IntegerList(one = int1, two = int2, one = int1,
                             int2))
  checkIdentical(append(col1, col2, 0),
                 IntegerList(two = int2, one = int1, one = int1,
                             int2))
  checkIdentical(append(append(col1, col2), col3),
                 IntegerList(one = int1, int2, two = int2,
                             one = int1, int2))

  ## for 'c'
  checkIdentical(c(col1, col2, col3),
                 IntegerList(one = int1, int2, two = int2, one = int1,
                             int2))
  checkException(c(col1, int2), silent = TRUE)
  checkException(c(col1, col2, recursive=TRUE), silent = TRUE)
}

test_TypedList_apply <- function() {
  int1 <- c(1L,2L,3L,5L,2L,8L)
  int2 <- c(15L,45L,20L,1L,15L,100L,80L,5L)
  col1 <- IntegerList(one = int1, int2)

  checkIdentical(lapply(col1, mean), list(one=mean(int1), mean(int2)))
  checkException(lapply(col1, 2), silent = TRUE)
}
