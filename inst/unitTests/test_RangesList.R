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
  rl <- RangesList(range1, range2)
  df <- data.frame(group=togroup(rl), group_name=NA_character_,
                   as.data.frame(c(range1,range2)), stringsAsFactors=FALSE)
  checkIdentical(df, as.data.frame(rl))
  names(rl) <- c("a", "b")
  df$group_name <- c("a", "b")[togroup(rl)] 
  checkIdentical(df, as.data.frame(rl))
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

