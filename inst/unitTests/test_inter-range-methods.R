test_Ranges_range <- function() {
  ir1 <- IRanges(c(2,5,1), c(3,7,3))
  ir2 <- IRanges(c(5,2,0), c(6,3,1))
  checkIdentical(range(ir1), IRanges(1, 7))
  checkIdentical(range(ir1, ir2), IRanges(0, 7))
  checkIdentical(range(IRanges()), IRanges())
  checkException(range(ir1, c(2,3)), silent = TRUE)
}

test_RangesList_range <- function() {
  for (compress in c(TRUE, FALSE)) {
    rl1 <- IRangesList(a = IRanges(c(1,2),c(4,3)), b = IRanges(c(4,6),c(10,7)),
                       compress = compress)
    rl2 <- IRangesList(c = IRanges(c(0,2),c(4,5)), a = IRanges(c(4,5),c(6,7)),
                       compress = compress)
    ans <- IRangesList(a = IRanges(1,7), b = IRanges(4,10), c = IRanges(0,5),
                       compress = compress)
    checkIdentical(range(rl1, rl2), ans)
    names(rl2) <- NULL
    ans <- IRangesList(IRanges(0,5), IRanges(4,10), compress = compress)
    checkIdentical(range(rl1, rl2), ans)
    ## must be same length
    checkException(range(rl2, rep.int(rl2, 2L)), silent=TRUE)
  }
}

test_IRanges_reduce <- function() {
  x <- IRanges()
  current <- reduce(x)
  checkIdentical(x, current)

  x <- IRanges(1:3, width=0)

  current <- reduce(x, with.revmap=TRUE)
  target <- x
  mcols(target) <- DataFrame(revmap=as(seq_along(target), "IntegerList"))
  checkIdentical(target, current)

  current <- reduce(x, drop.empty.ranges=TRUE, with.revmap=TRUE)
  target <- IRanges()
  mcols(target) <- DataFrame(revmap=as(seq_along(target), "IntegerList"))
  checkIdentical(target, current)

  x <- IRanges(c(1:4, 10:11, 11), width=c(0,1,1,0,0,0,1))

  current <- reduce(x, with.revmap=TRUE)
  target <- IRanges(c(1:2, 10:11), width=c(0,2,0,1))
  mcols(target) <- DataFrame(revmap=IntegerList(1,2:4,5,6:7))
  checkIdentical(target, current)

  current <- reduce(x, drop.empty.ranges=TRUE, with.revmap=TRUE)
  target <- IRanges(c(2, 11), width=c(2,1))
  mcols(target) <- DataFrame(revmap=IntegerList(2:3,7))
  checkIdentical(target, current)

  x <- IRanges(start=c(1,2,3), end=c(5,2,8))
  y <- reduce(x, with.revmap=TRUE)

  target <- IRanges(start=1, end=8)
  mcols(target) <- DataFrame(revmap=IntegerList(1:3))
  checkIdentical(target, y)

  mcols(target)$revmap <- as(seq_along(target), "IntegerList")
  checkIdentical(target, reduce(y, with.revmap=TRUE))

  x <- IRanges(start=c(15,45,20,1), end=c(15,100,80,5))
  y <- reduce(x, with.revmap=TRUE)

  target <- IRanges(start=c(1,15,20), end=c(5,15,100))
  mcols(target) <- DataFrame(revmap=IntegerList(4, 1, 3:2))
  checkIdentical(target, y)

  mcols(target)$revmap <- as(seq_along(target), "IntegerList")
  checkIdentical(target, reduce(y, with.revmap=TRUE))

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
  y1 <- reduce(x)
  checkIdentical(y1, IRanges(start=c(-10,-2,7), end=c(-11,5,9)))
  checkIdentical(reduce(y1), y1)
  y2 <- reduce(x, with.inframe.attrib=TRUE)
  checkIdentical(start(attr(y2, "inframe")), c(9L,6L,1L,9L,9L,1L,1L,6L))
  checkIdentical(width(attr(y2, "inframe")), width(x))
  y3 <- reduce(x, drop.empty.ranges=TRUE)
  checkIdentical(y3, y1[width(y1) != 0L])
  checkIdentical(reduce(y3), y3)
  y4 <- reduce(x, drop.empty.ranges=TRUE, with.inframe.attrib=TRUE)
  checkIdentical(attr(y4, "inframe"), attr(y2, "inframe"))
  y5 <- reduce(x, min.gapwidth=0)
  checkIdentical(y5, IRanges(start=c(-10,-2,-2,6,7,7), end=c(-11,-3,5,5,6,9)))
  y6 <- reduce(x, drop.empty.ranges=TRUE, min.gapwidth=0)
  checkIdentical(y6, y5[width(y5) != 0L])
  y7 <- reduce(x, min.gapwidth=2)
  checkIdentical(y7, IRanges(start=c(-10,-2), end=c(-11,9)))
  y8 <- reduce(x, min.gapwidth=8)
  checkIdentical(y8, y7)
  y9 <- reduce(x, min.gapwidth=9)
  checkIdentical(y9, IRanges(start=-10, end=9))
}

test_RangesList_reduce <- function() {
  range1 <- IRanges(start=c(1,2,3), end=c(5,2,8))
  range2 <- IRanges(start=c(15,45,20,1), end=c(15,100,80,5))
  range3 <- IRanges(start=c(3,-2,6,7,-10,-2,3), width=c(1,0,0,0,0,8,0))
  range4 <- IRanges()
  for (compress in c(TRUE, FALSE)) {
    collection <- IRangesList(one=range1,
                              range2,
                              range3,
                              range4,
                              compress=compress)
    for (with.revmap in c(FALSE, TRUE)) {
      for (drop.empty.ranges in c(FALSE, TRUE)) {
        current <- reduce(collection, drop.empty.ranges=drop.empty.ranges,
                                      with.revmap=with.revmap)
        target <- IRangesList(one=reduce(range1,
                                         drop.empty.ranges=drop.empty.ranges,
                                         with.revmap=with.revmap),
                              reduce(range2,
                                     drop.empty.ranges=drop.empty.ranges,
                                     with.revmap=with.revmap),
                              reduce(range3,
                                     drop.empty.ranges=drop.empty.ranges,
                                     with.revmap=with.revmap),
                              reduce(range4,
                                     drop.empty.ranges=drop.empty.ranges,
                                     with.revmap=with.revmap),
                              compress=compress)
        checkIdentical(target, current)
      }
    }
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

test_RangesList_gaps <- function() {
  range1 <- IRanges(start=c(1,2,3), end=c(5,2,8))
  range2 <- IRanges(start=c(15,45,20,1), end=c(15,100,80,5))
  for (compress in c(TRUE, FALSE)) {
    collection <- IRangesList(one = range1, range2, compress = compress)
    checkIdentical(gaps(collection),
                   IRangesList(one = gaps(range1), gaps(range2),
                               compress = compress))
  }
}

test_Ranges_disjoin <- function()
{
  checkIdentical(disjoin(IRanges()), IRanges())
  ir <- IRanges(c(1, 1, 4, 10), c(6, 3, 8, 10))
  checkIdentical(disjoin(ir), IRanges(c(1, 4, 7, 10), c(3, 6, 8, 10)))
}

test_CompressedIRangesList_disjoin <- function()
{
    r0 <- IRanges(10, 20)
    checkTrue(validObject(disjoin(IRangesList())))
    ## unnamed; incl. 0-length
    irl <- IRangesList(IRanges())
    checkIdentical(irl, disjoin(irl))
    irl <- IRangesList(r0, IRanges(), r0)
    checkIdentical(irl, disjoin(irl))
    irl <- IRangesList(r0, IRanges(), IRanges(), r0)
    checkIdentical(irl, disjoin(irl))
    ## named; incl. 0-length
    irl <- IRangesList(a=IRanges())
    checkIdentical(irl, disjoin(irl))
    irl <- IRangesList(a=r0, b=IRanges(), c=r0)
    checkIdentical(irl, disjoin(irl))
    irl <- IRangesList(a=r0, b=IRanges(), c=IRanges(), d=r0)
    checkIdentical(irl, disjoin(irl))
    ## no interference between separate elements
    r0 <- IRanges(10, c(15, 20))
    dr0 <- disjoin(r0)
    irl <- IRangesList(r0, r0)
    checkIdentical(IRangesList(dr0, dr0), disjoin(irl))
    irl <- IRangesList(r0, IRanges(), r0)
    checkIdentical(IRangesList(dr0, IRanges(), dr0), disjoin(irl))
    ## 0-width
    ## 1-width
    r0 <- IRanges(c(1, 10), 10)
    irl <- IRangesList(r0, IRanges())
    checkIdentical(disjoin(r0), disjoin(irl)[[1]])
    irl <- IRangesList(IRanges(), r0)
    checkIdentical(disjoin(r0), disjoin(irl)[[2]])
}

test_Ranges_disjointBins <- function()
{
  checkIdentical(disjointBins(IRanges()), integer())
  checkIdentical(disjointBins(IRanges(1, 5)), 1L)
  checkIdentical(disjointBins(IRanges(c(1, 3), c(5, 12))), c(1L, 2L))
  checkIdentical(disjointBins(IRanges(c(1, 3, 10), c(5, 12, 13))),
                 c(1L, 2L, 1L))
  checkIdentical(disjointBins(IRanges(c(3, 1, 10), c(5, 12, 13))),
                 c(2L, 1L, 2L))
}

