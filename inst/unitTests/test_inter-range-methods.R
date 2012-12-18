test_Ranges_range <- function() {
  ir1 <- IRanges(c(2,5,1), c(3,7,3))
  ir2 <- IRanges(c(5,2,0), c(6,3,1))
  checkIdentical(range(ir1), IRanges(1, 7))
  checkIdentical(range(ir1, ir2), IRanges(0, 7))
  checkIdentical(range(IRanges()), IRanges())
  checkException(range(ir1, c(2,3)), silent = TRUE)
}

test_IRanges_reduce <- function() {
  x <- IRanges()
  current <- reduce(x, with.mapping=FALSE)
  checkIdentical(x, current)

  x <- IRanges(1:3, width=0)

  current <- reduce(x)
  target <- x
  mcols(target) <- DataFrame(mapping=as(seq_along(target), "IntegerList"))
  checkIdentical(target, current)

  current <- reduce(x, drop.empty.ranges=TRUE)
  target <- IRanges()
  mcols(target) <- DataFrame(mapping=as(seq_along(target), "IntegerList"))
  checkIdentical(target, current)

  x <- IRanges(c(1:4, 10:11, 11), width=c(0,1,1,0,0,0,1))

  current <- reduce(x)
  target <- IRanges(c(1:2, 10:11), width=c(0,2,0,1))
  mcols(target) <- DataFrame(mapping=IntegerList(1,2:4,5,6:7))
  checkIdentical(target, current)

  current <- reduce(x, drop.empty.ranges=TRUE)
  target <- IRanges(c(2, 11), width=c(2,1))
  mcols(target) <- DataFrame(mapping=IntegerList(2:3,7))
  checkIdentical(target, current)

  x <- IRanges(start=c(1,2,3), end=c(5,2,8))
  y <- reduce(x)

  target <- IRanges(start=1, end=8)
  mcols(target) <- DataFrame(mapping=IntegerList(1:3))
  checkIdentical(target, y)

  mcols(target)$mapping <- as(seq_along(target), "IntegerList")
  checkIdentical(target, reduce(y))

  x <- IRanges(start=c(15,45,20,1), end=c(15,100,80,5))
  y <- reduce(x)

  target <- IRanges(start=c(1,15,20), end=c(5,15,100))
  mcols(target) <- DataFrame(mapping=IntegerList(4, 1, 3:2))
  checkIdentical(target, y)

  mcols(target)$mapping <- as(seq_along(target), "IntegerList")
  checkIdentical(target, reduce(y))

  x <- IRanges(start=c(7,3,-2,6,7,-10,-2,3), width=c(3,1,0,0,0,0,8,0))
  ## Before reduction:
  ##     start end width  ==-10===-5====0===+5==+10===
  ## [1]     7   9     3  ....:....:....:....:.xxx:...
  ## [2]     3   3     1  ....:....:....:..x.:....:...
  ## [3]    -2  -3     0  ....:....:..[.:....:....:...
  ## [4]     6   5     0  ....:....:....:....:[...:...
  ## [5]     7   6     0  ....:....:....:....:.[..:...
  ## [6]   -10 -11     0  ....[....:....:....:....:...
  ## [7]    -2   5     8  ....:....:..xxxxxxxx....:...
  ## [8]     3   2     0  ....:....:....:..[.:....:...
  ## ---------------------==-10===-5====0===+5==+10===
  ## After reduction:
  ##                  y1: ....[....:..xxxxxxxx.xxx:...
  ##                  y3: ....:....:..xxxxxxxx....:...
  y1 <- reduce(x, with.mapping=FALSE)
  checkIdentical(y1, IRanges(start=c(-10,-2,7), end=c(-11,5,9)))
  checkIdentical(reduce(y1, with.mapping=FALSE), y1)
  y2 <- reduce(x, with.mapping=FALSE, with.inframe.attrib=TRUE)
  checkIdentical(start(attr(y2, "inframe")), c(9L,6L,1L,9L,9L,1L,1L,6L))
  checkIdentical(width(attr(y2, "inframe")), width(x))
  y3 <- reduce(x, drop.empty.ranges=TRUE, with.mapping=FALSE)
  checkIdentical(y3, y1[width(y1) != 0L])
  checkIdentical(reduce(y3, with.mapping=FALSE), y3)
  y4 <- reduce(x, drop.empty.ranges=TRUE,
                  with.mapping=FALSE, with.inframe.attrib=TRUE)
  checkIdentical(attr(y4, "inframe"), attr(y2, "inframe"))
  y5 <- reduce(x, min.gapwidth=0, with.mapping=FALSE)
  checkIdentical(y5, IRanges(start=c(-10,-2,-2,6,7,7), end=c(-11,-3,5,5,6,9)))
  y6 <- reduce(x, drop.empty.ranges=TRUE, min.gapwidth=0, with.mapping=FALSE)
  checkIdentical(y6, y5[width(y5) != 0L])
  y7 <- reduce(x, min.gapwidth=2, with.mapping=FALSE)
  checkIdentical(y7, IRanges(start=c(-10,-2), end=c(-11,9)))
  y8 <- reduce(x, min.gapwidth=8, with.mapping=FALSE)
  checkIdentical(y8, y7)
  y9 <- reduce(x, min.gapwidth=9, with.mapping=FALSE)
  checkIdentical(y9, IRanges(start=-10, end=9))
}

test_RangesList_reduce <- function() {
  for (compress in c(TRUE, FALSE)) {
    range1 <- IRanges(start=c(1,2,3), end=c(5,2,8))
    range2 <- IRanges(start=c(15,45,20,1), end=c(15,100,80,5))
    range3 <- IRanges(start=c(3,-2,6,7,-10,-2,3), width=c(1,0,0,0,0,8,0))
    range4 <- IRanges()
    collection <- IRangesList(one = range1, range2, range3, range4,
                              compress = compress)
    checkIdentical(reduce(collection, with.mapping=FALSE),
                   IRangesList(one = reduce(range1, with.mapping=FALSE),
                               reduce(range2, with.mapping=FALSE),
                               reduce(range3, with.mapping=FALSE),
                               reduce(range4, with.mapping=FALSE),
                               compress = compress))
    checkIdentical(reduce(collection, drop.empty.ranges=TRUE,
                                      with.mapping=FALSE),
                   IRangesList(one = reduce(range1, drop.empty.ranges=TRUE,
                                                    with.mapping=FALSE),
                               reduce(range2, drop.empty.ranges=TRUE,
                                              with.mapping=FALSE),
                               reduce(range3, drop.empty.ranges=TRUE,
                                              with.mapping=FALSE),
                               reduce(range4, drop.empty.ranges=TRUE,
                                              with.mapping=FALSE),
                               compress = compress))
  }
}

test_IRanges_gaps <- function() {
  checkIdentical(gaps(IRanges()), IRanges())
  checkIdentical(gaps(IRanges(), start=1, end=4),
                 IRanges(start=1, end=4))

  x <- IRanges(start=2, end=3)
  checkIdentical(gaps(x), IRanges())
  checkIdentical(gaps(x, start=2), IRanges())
  checkIdentical(gaps(x, start=4), IRanges())
  checkIdentical(gaps(x, start=0), IRanges(start=0, end=1))
  checkIdentical(gaps(x, end=3), IRanges())
  checkIdentical(gaps(x, end=1), IRanges())
  checkIdentical(gaps(x, end=5), IRanges(start=4, end=5))
  checkIdentical(gaps(x, start=0, end=5), IRanges(start=c(0,4), end=c(1,5)))
}

test_Ranges_isDisjoint <- function() {
  ir1 <- IRanges(c(2,5,1), c(3,7,3))
  ir2 <- IRanges(c(2,9,5), c(3,9,6))
  ir3 <- IRanges(1, 5)
  checkIdentical(isDisjoint(ir1), FALSE)
  checkIdentical(isDisjoint(ir2), TRUE)
  checkIdentical(isDisjoint(ir3), TRUE)
}

