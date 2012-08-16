test_RangesList_construction <- function() {
  empty <- RangesList()
  checkTrue(validObject(empty))
  checkIdentical(length(empty), 0L)
  range1 <- IRanges(start=c(1,2,3), end=c(5,2,8))
  range2 <- IRanges(start=c(15,45,20,1), end=c(15,100,80,5))
  named <- RangesList(one = range1, two = range2)
  checkTrue(validObject(named))
  checkIdentical(length(named), 2L)
  checkIdentical(start(named),
                 IntegerList(one = start(range1), two = start(range2),
                             compress=FALSE))
  checkIdentical(end(named),
                 IntegerList(one = end(range1), two = end(range2),
                             compress=FALSE))
  checkIdentical(width(named),
                 IntegerList(one = width(range1), two = width(range2),
                             compress=FALSE))
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
  checkIdentical(subsetByOverlaps(collection, RangesList()),
                 RangesList(one=IRanges(), IRanges()))
  checkIdentical(subsetByOverlaps(collection,
                                  RangesList(IRanges(4, 6), IRanges(50, 70))),
                 RangesList(one=IRanges(c(1,3),c(5,8)),
                            IRanges(c(20,45),c(100,80))))
  checkIdentical(subsetByOverlaps(collection,
                                  RangesList(IRanges(50, 70), one=IRanges(4, 6))),
                 RangesList(one=IRanges(c(1,3),c(5,8)), IRanges()))
}

test_RangesList_as_list <- function() {
  range1 <- IRanges(start=c(1,2,3), end=c(5,2,8))
  range2 <- IRanges(start=c(15,45,20,1), end=c(15,100,80,5))
  checkIdentical(list(range1, range2),
                 as.list(RangesList(range1, range2)))
  checkIdentical(list(a=range1, b=range2),
                 as.list(RangesList(a=range1, b=range2)))
}

test_RangesList_as_data_frame <- function() {
  range1 <- IRanges(start=c(1,2,3), end=c(5,2,8))
  range2 <- IRanges(start=c(15,45,20,1), end=c(15,100,80,5))
  checkIdentical(data.frame(space=rep(c("1", "2"), c(3,4)),
                            as.data.frame(c(range1,range2))),
                 as.data.frame(RangesList(range1, range2)))
  checkIdentical(data.frame(space=rep(c("a", "b"), c(3,4)),
                            as.data.frame(c(range1,range2))),
                 as.data.frame(RangesList(a=range1, b=range2)))
}

test_RangesList_flank <- function() {
  range1 <- IRanges(start=c(2,5), end=c(3,7))
  range2 <- IRanges(start=1, end=3)
  for (compress in c(TRUE, FALSE)) {
    collection <- IRangesList(range1, range2, compress = compress)
    checkIdentical(flank(collection, 2),
                   IRangesList(IRanges(c(0, 3), c(1, 4)), IRanges(-1, 0),
                               compress = compress))
    checkIdentical(flank(collection, 2, FALSE),
                   IRangesList(IRanges(c(4, 8), c(5, 9)), IRanges(4, 5),
                               compress = compress))
    checkIdentical(flank(collection, 2, LogicalList(c(FALSE, TRUE), FALSE)),
                   IRangesList(IRanges(c(4, 3), c(5, 4)), IRanges(4, 5),
                               compress = compress))
    checkIdentical(flank(collection, IntegerList(c(2, -2), 2)),
                   IRangesList(IRanges(c(0, 5), c(1, 6)), IRanges(-1, 0),
                               compress = compress))
    checkIdentical(flank(collection, 2, both = TRUE),
                   IRangesList(IRanges(c(0, 3), c(3, 6)), IRanges(-1, 2),
                               compress = compress))
    checkIdentical(flank(collection, 2, FALSE, TRUE),
                   IRangesList(IRanges(c(2, 6), c(5, 9)), IRanges(2, 5),
                               compress = compress))
    checkIdentical(flank(collection, -2, FALSE, TRUE),
                   IRangesList(IRanges(c(2, 6), c(5, 9)),
                               IRanges(2, 5), compress = compress))
    checkException(flank(collection, 2, both = c(TRUE, FALSE, TRUE)),
                   silent = TRUE) # not vectorized
    checkException(flank(collection, 2, LogicalList(c(FALSE, TRUE), NA)),
                   silent = TRUE)
    checkException(flank(collection, NA), silent = TRUE)
  }
}

test_RangesList_narrow <- function() {
  range1 <- IRanges(start=c(2,5), end=c(3,7))
  range2 <- IRanges(start=1, end=3)
  for (compress in c(TRUE, FALSE)) {
    collection <- IRangesList(range1, range2, compress = compress)
    checkIdentical(narrow(collection, start=1, end=2),
                   IRangesList(IRanges(c(2, 5), c(3, 6)), IRanges(1, 2),
                               compress = compress))
    checkException(narrow(collection, start=10, end=20), silent = TRUE)
  }
}

test_RangesList_resize <- function() {
  range1 <- IRanges(start=c(2,5), end=c(3,7))
  range2 <- IRanges(start=1, end=3)
  for (compress in c(TRUE, FALSE)) {
    collection <- IRangesList(range1, range2, compress = compress)
    checkIdentical(resize(collection, width=10),
                   IRangesList(IRanges(c(2, 5), width=10), IRanges(1, width=10),
                               compress = compress))
    checkIdentical(resize(collection, width=10, fix="end"),
                   IRangesList(IRanges(c(-6, -2), width=10), IRanges(-6, width=10),
                               compress = compress))
    checkIdentical(resize(collection, width=10, fix="center"),
                   IRangesList(IRanges(c(-2, 1), width=10), IRanges(-3, width=10),
                               compress = compress))
    checkIdentical(resize(collection, width=10,
                          fix=CharacterList(c("start", "end"), "center")),
                   IRangesList(IRanges(c(2, -2), width=10), IRanges(-3, width=10),
                               compress = compress))
    checkException(resize(collection, -1), silent = TRUE)
  }
}

test_RangesList_restrict <- function() {
  range1 <- IRanges(start=c(2,5), end=c(3,7))
  range2 <- IRanges(start=1, end=3)
  for (compress in c(TRUE, FALSE)) {
    collection <- IRangesList(range1, range2, compress = compress)
    checkIdentical(restrict(collection, start=2, end=5),
                   IRangesList(IRanges(c(2, 5), c(3, 5)), IRanges(2, 3),
                               compress = compress))
    checkIdentical(restrict(collection, start=1, end=2),
                   IRangesList(IRanges(2, 2), IRanges(1, 2),
                               compress = compress))
    checkIdentical(restrict(collection, start=1, end=2, keep.all.ranges=TRUE),
                   IRangesList(IRanges(c(2, 3), c(2, 2)), IRanges(1, 2),
                               compress = compress))
  }
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

test_RangesList_reduce <- function() {
  for (compress in c(TRUE, FALSE)) {
    range1 <- IRanges(start=c(1,2,3), end=c(5,2,8))
    range2 <- IRanges(start=c(15,45,20,1), end=c(15,100,80,5))
    range3 <- IRanges(start=c(3,-2,6,7,-10,-2,3), width=c(1,0,0,0,0,8,0))
    range4 <- IRanges()
    collection <- IRangesList(one = range1, range2, range3, range4,
                              compress = compress)
    checkIdentical(reduce(collection),
                   IRangesList(one = reduce(range1), reduce(range2),
                               reduce(range3), reduce(range4),
                               compress = compress))
    checkIdentical(reduce(collection, drop.empty.ranges=TRUE),
                   IRangesList(one = reduce(range1, drop.empty.ranges=TRUE),
                               reduce(range2, drop.empty.ranges=TRUE),
                               reduce(range3, drop.empty.ranges=TRUE),
                               reduce(range4, drop.empty.ranges=TRUE),
                               compress = compress))
  }
}

test_RangesList_setops <- function() {
  rl1 <- RangesList(IRanges(c(1,2),c(4,3)), IRanges(c(4,6),c(10,7)))
  rl2 <- RangesList(IRanges(c(0,2),c(4,5)), IRanges(c(4,5),c(6,7)))
  checkIdentical(union(rl1, rl2),
                 RangesList(union(rl1[[1]], rl2[[1]]), union(rl1[[2]], rl2[[2]])))
  checkIdentical(intersect(rl1, rl2),
                 RangesList(intersect(rl1[[1]], rl2[[1]]), intersect(rl1[[2]], rl2[[2]])))
  checkIdentical(setdiff(rl1, rl2),
                 RangesList(setdiff(rl1[[1]], rl2[[1]]), setdiff(rl1[[2]], rl2[[2]])))
}

test_IRangesList_construction <- function() {
  range1 <- IRanges(start=c(1,2,3), end=c(5,2,8))
  range2 <- IRanges(start=c(15,45,20,1), end=c(15,100,80,5))
  for (compress in c(TRUE, FALSE)) {
    named <- IRangesList(one = range1, two = range2, compress = compress)
    checkIdentical(length(named), 2L)
    checkIdentical(names(named), c("one", "two"))
    checkIdentical(range1, named[[1]])
    unnamed <- IRangesList(range1, range2)
    checkTrue(validObject(unnamed))
    checkIdentical(length(unnamed), 2L)
    checkIdentical(range2, unnamed[[2]])
    checkIdentical(names(unnamed), NULL)
  }
}

test_IRangesList_annotation <- function() {
  range1 <- IRanges(start=c(1,2,3), end=c(5,2,8))
  range2 <- IRanges(start=c(15,45,20,1), end=c(15,100,80,5))
  for (compress in c(TRUE, FALSE)) {
    rl <- IRangesList(range1, range2, compress = compress)
    mcols(rl) <- DataFrame(a = 1:2)
    checkIdentical(mcols(rl)[,1], 1:2)
    checkIdentical(mcols(rl[2:1])[,1], 2:1)
    checkIdentical(mcols(c(rl,rl))[,1], rep(1:2,2))
    checkIdentical(mcols(append(rl,rl))[,1], rep(1:2,2))
  }
}

## test_RangesList_overlap <- function() {
##   rl1 <- RangesList(a = IRanges(c(1,2),c(4,3)), b = IRanges(c(4,6),c(10,7)))
##   rl2 <- RangesList(b = IRanges(c(0,2),c(4,5)), a = IRanges(c(4,5),c(6,7)))
##   overlap(rl1, rl2)
##   overlap(rl1, rl2, select = "first")
##   overlap(rl1, rl2, select = "first", drop = TRUE)
##   names(rl2)[1] <- "c"
##   overlap(rl1, rl2)
##   overlap(rl1, rl2, select = "first")
##   overlap(rl1, rl2, select = "first", drop = TRUE)
##   names(rl2) <- NULL
##   overlap(rl1, rl2)
##   overlap(rl1, rl2, select = "first")
##   overlap(rl1, rl2, select = "first", drop = TRUE)
##   overlap(rl1, rl2[1])
##   overlap(rl1, rl2[1], select = "first")
##   overlap(rl1, rl2[1], select = "first", drop = TRUE)
##   overlap(rl1[1], rl2)
##   overlap(rl1[1], rl2, select = "first")
##   overlap(rl1[1], rl2, select = "first", drop = TRUE)
## }

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
