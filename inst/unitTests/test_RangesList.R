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

test_RangesList_reduce <- function() {
  range1 <- IRanges(start=c(1,2,3), end=c(5,2,8))
  range2 <- IRanges(start=c(15,45,20,1), end=c(15,100,80,5))
  range3 <- IRanges(start=c(3,-2,6,7,-10,-2,3), width=c(1,0,0,0,0,8,0))
  range4 <- IRanges()
  collection <- RangesList(one = range1, range2, range3, range4)
  checkIdentical(reduce(collection),
                 RangesList(one = reduce(range1), reduce(range2),
                            reduce(range3), reduce(range4)))
  checkIdentical(reduce(collection, drop.empty.ranges=TRUE),
                 RangesList(one = reduce(range1, drop.empty.ranges=TRUE),
                            reduce(range2, drop.empty.ranges=TRUE),
                            reduce(range3, drop.empty.ranges=TRUE),
                            reduce(range4, drop.empty.ranges=TRUE)))
}

test_RangesList_gaps <- function() {
  range1 <- IRanges(start=c(1,2,3), end=c(5,2,8))
  range2 <- IRanges(start=c(15,45,20,1), end=c(15,100,80,5))
  collection <- RangesList(one = range1, range2)
  checkIdentical(gaps(collection), RangesList(one = gaps(range1), gaps(range2)))
}

test_RangesList_range <- function() {
  rl1 <- RangesList(a = IRanges(c(1,2),c(4,3)), b = IRanges(c(4,6),c(10,7)))
  rl2 <- RangesList(c = IRanges(c(0,2),c(4,5)), a = IRanges(c(4,5),c(6,7)))
  ans <- RangesList(a = IRanges(1,7), b = IRanges(4,10), c = IRanges(0,5))
  checkIdentical(range(rl1, rl2), ans)
  names(rl2) <- NULL
  ans <- RangesList(a = IRanges(0,5), b = IRanges(4,10))
  checkIdentical(range(rl1, rl2), ans)
  ## must be same length
  checkException(range(RangesList(IRanges(c(1,2),c(4,3)), IRanges(3,5)),
                       RangesList(IRanges(2,6))), silent=TRUE) 
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
    elementMetadata(rl) <- DataFrame(a = 1:2)
    checkIdentical(elementMetadata(rl)[,1], 1:2)
    checkIdentical(elementMetadata(rl[2:1])[,1], 2:1)
    checkIdentical(elementMetadata(c(rl,rl))[,1], rep(1:2,2))
    checkIdentical(elementMetadata(append(rl,rl))[,1], rep(1:2,2))
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
