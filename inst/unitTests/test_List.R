### NOTE: List is an abstract type, so we just test with IntegerList

test_List_replace_names <- function() {
  int1 <- c(1L,2L,3L,5L,2L,8L)
  int2 <- c(15L,45L,20L,1L,15L,100L,80L,5L)
  for (compress in c(TRUE, FALSE)) {
    collection <- IntegerList(int1, int2, compress = compress)
    names(collection) <- c("one", "two")
    checkIdentical(names(collection), c("one", "two"))
    names(collection) <- NULL
    checkIdentical(names(collection), NULL)
    names(collection) <- "one"
    checkIdentical(names(collection), c("one", NA))
    checkException(names(collection) <- c("one", "two", "three"),
                   silent = TRUE)
  }
}

test_List_extraction <- function() {
  int1 <- c(1L,2L,3L,5L,2L,8L)
  int2 <- c(15L,45L,20L,1L,15L,100L,80L,5L)
  for (compress in c(TRUE, FALSE)) {
    collection <- IntegerList(int1, int2, compress = compress)

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
    checkIdentical(IntegerList(one=int1, int2, compress = compress)[["one"]],
                   int1)
    checkIdentical(IntegerList(one=int1, int2, compress = compress)$one,
                   int1)
  }
}

test_List_subset <- function() {
  int1 <- c(1L,2L,3L,5L,2L,8L)
  int2 <- c(15L,45L,20L,1L,15L,100L,80L,5L)
  for (compress in c(TRUE, FALSE)) {
    collection <- IntegerList(one = int1, int2, compress = compress)
    unnamed <- IntegerList(int1, int2, compress = compress)

    checkException(collection[1,2], silent = TRUE)
    checkException(collection[list()], silent = TRUE)
    if (compress) {
      checkException(collection[-3], silent = TRUE)
      checkException(collection[5], silent = TRUE)
      checkException(collection[c(NA, 2)], silent = TRUE)
      checkException(collection[c(TRUE, TRUE, TRUE)], silent = TRUE)
      checkException(unnamed["one"], silent = TRUE)
    }
    checkException(collection[c(-1,2)], silent = TRUE)

    empty <- IntegerList(compress = compress)
    names(empty) <- character(0)
    checkIdentical(collection[numeric()], empty)
    checkIdentical(collection[logical()], empty)
    checkIdentical(collection[character()], empty)
    checkIdentical(collection[NULL], empty)
    checkIdentical(collection[], collection)
    checkIdentical(collection[FALSE], empty)
    checkIdentical(collection[c(FALSE, FALSE)], empty)
    checkIdentical(collection[TRUE], collection)
    checkIdentical(collection[c(TRUE, FALSE)],
                   IntegerList(one = int1, compress = compress))
    rl2 <- IntegerList(int2, compress = compress)
    names(rl2) <- ""
    checkIdentical(collection[2], rl2)
    checkIdentical(collection[c(2,1)],
                   IntegerList(int2, one = int1, compress = compress))
    checkIdentical(collection[-1], rl2)
    checkIdentical(collection["one"],
                   IntegerList(one = int1, compress = compress))
  }
}

test_List_replace <- function() {
  int1 <- c(1L,2L,3L,5L,2L,8L)
  int2 <- c(15L,45L,20L,1L,15L,100L,80L,5L)
  for (compress in c(TRUE, FALSE)) {
    collection <- IntegerList(one = int1, int2, compress = compress)

    checkException(collection[1,2] <- 1L, silent = TRUE)
    checkException(collection[c(-1,2)] <- 1L, silent = TRUE)

    newcollection <- collection
    newcollection[list()] <- 1L
    checkIdentical(newcollection, collection)

    newcollection <- collection
    newcollection[] <- collection
    checkIdentical(newcollection, collection)

    newcollection1 <- newcollection2 <- collection
    newcollection1[2:1] <- collection
    checkIdentical(newcollection1,
                   IntegerList(one = int2, int1, compress = compress))
    newcollection2[] <- collection[2:1]
    checkIdentical(newcollection2, newcollection1)

    value <-  IntegerList(1:10, compress = compress)
    newcollection <- collection
    newcollection[TRUE] <- value
    checkIdentical(newcollection,
                   IntegerList(one = 1:10, 1:10, compress = compress))
    newcollection <- collection
    newcollection[c(TRUE, FALSE)] <- value
    checkIdentical(newcollection,
                   IntegerList(one = 1:10, int2, compress = compress))
    newcollection <- collection
    newcollection["one"] <- value
    checkIdentical(newcollection,
                   IntegerList(one = 1:10, int2, compress = compress))

    newcollection <- collection
    newcollection[list(6:5, TRUE)] <- list(-1:-2, -99:-100)
    checkIdentical(newcollection,
                   IntegerList(one = c(1,2,3,5,-2,-1),
                               rep(c(-99,-100), 4), compress = compress))

    collection <- IntegerList(one = int1, two = int2, compress = compress)

    newcollection <- collection
    newcollection[c("two", "one")] <- collection
    checkIdentical(newcollection,
                   IntegerList(one = int2, two = int1, compress = compress))

    newcollection <- collection
    newcollection[list(two=6:5, one=TRUE)] <- list(-1:-2, -99:-100)
    checkIdentical(newcollection,
                   IntegerList(one = rep(c(-99,-100), 3),
                               two = c(15,45,20,1,-2,-1,80,5),
                               compress = compress))

    collection <- IntegerList(one = c(a=1,b=2), two = c(d=1,b=0,a=5),
                              compress = compress)
    newcollection1 <- newcollection2 <- collection
    newcollection1[list(two=2, one=2:1)] <- list(99, 11:12)
    checkIdentical(newcollection1,
                   IntegerList(one = c(a=12,b=11), two = c(d=1,b=99,a=5),
                               compress = compress))

    newcollection2[list(two="b", one=c("b", "a"))] <- list(99, 11:12)
    checkIdentical(newcollection2, newcollection1)
  }
}

test_List_combine <- function() {
  int1 <- c(1L,2L,3L,5L,2L,8L)
  int2 <- c(15L,45L,20L,1L,15L,100L,80L,5L)
  for (compress in c(TRUE, FALSE)) {
    col1 <- IntegerList(one = int1, int2, compress = compress)
    col2 <- IntegerList(two = int2, one = int1, compress = compress)
    col3 <- IntegerList(int2, compress = compress)

    if (compress)
      checkException(append(col1, col2, c(1,2,3)), silent = TRUE)
    checkException(append(col1, col2, col3), silent = TRUE)

    checkIdentical(append(col1, col2),
                   IntegerList(one = int1, int2, two = int2,
                               one = int1, compress = compress))
    checkIdentical(append(col1, col2, 1),
                   IntegerList(one = int1, two = int2, one = int1,
                               int2, compress = compress))
    checkIdentical(append(col1, col2, 0),
                   IntegerList(two = int2, one = int1, one = int1,
                               int2, compress = compress))
    checkIdentical(append(append(col1, col2), col3),
                   IntegerList(one = int1, int2, two = int2,
                               one = int1, int2, compress = compress))

    ## for 'c'
    checkIdentical(c(col1, col2, col3),
                   IntegerList(one = int1, int2, two = int2, one = int1,
                               int2, compress = compress))
    if (compress) {
      checkException(c(col1, int2), silent = TRUE)
      checkException(c(col1, col2, recursive = TRUE), silent = TRUE)
    }
  }
}

test_List_apply <- function() {
  int1 <- c(1L,2L,3L,5L,2L,8L)
  int2 <- c(15L,45L,20L,1L,15L,100L,80L,5L)
  for (compress in c(TRUE, FALSE)) {
    col1 <- IntegerList(one = int1, int2)
    checkIdentical(lapply(col1, mean), list(one=mean(int1), mean(int2)))
    checkException(lapply(col1, 2), silent = TRUE)
  }
}

test_List_funcprog <- function() {
  int1 <- c(1L,2L,3L,5L,2L,8L)
  int2 <- c(15L,45L,20L,1L,15L,100L)
  for (compress in c(TRUE, FALSE)) {
    collection <- IntegerList(int1, int2, int1, compress = compress)
    addcollect <- IntegerList(int2, int1, int1, compress = compress)
    checkIdentical(Reduce("+", collection), Reduce("+", list(int1, int2, int1)))
    checkIdentical(as.list(Filter(function(x) mean(x) > 10, collection)),
                   Filter(function(x) mean(x) > 10, list(int1, int2, int1)))
    checkIdentical(Find(function(x) mean(x) > 10, collection),
                   Find(function(x) mean(x) > 10, list(int1, int2, int1)))
    checkIdentical(Map("+", collection, addcollect),
                   Map("+", list(int1, int2, int1), list(int2, int1, int1)))
    checkIdentical(mapply("+", collection, addcollect),
                   mapply("+", list(int1, int2, int1), list(int2, int1, int1)))
    checkIdentical(Position(function(x) mean(x) > 10, collection),
                   Position(function(x) mean(x) > 10, list(int1, int2, int1)))
  }
}

test_List_annotation <- function() {
  int1 <- c(1L,2L,3L,5L,2L,8L)
  int2 <- c(15L,45L,20L,1L,15L,100L,80L,5L)
  for (compress in c(TRUE, FALSE)) {
    ilist <- IntegerList(int1, int2, compress = compress)
    elementMetadata(ilist) <- DataFrame(a = 1:2)
    checkIdentical(elementMetadata(ilist)[,1], 1:2)
    checkIdentical(elementMetadata(ilist[2:1])[,1], 2:1)
    checkIdentical(elementMetadata(c(ilist,ilist))[,1], rep(1:2,2))
    checkIdentical(elementMetadata(append(ilist,ilist))[,1], rep(1:2,2))
  }
}
