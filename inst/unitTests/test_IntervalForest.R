test_IntervalForest_construction <- function() {  
  query <- IRanges(c(1, 3, 9), c(5, 7, 10))
  qpartition <- factor(c("a","a","b"))
  query <- split(query, qpartition)

  subject <- IRanges(c(2, 10), c(2, 12))
  spartition <- factor(c("a","b"))
  subject <- split(subject, spartition)

  forest <- IntervalForest(subject)
  checkTrue(validObject(forest))
  checkIdentical(length(forest), 2L);
  checkIdentical(names(forest), c("a","b"))
  checkIdentical(sum(elementLengths(forest)), 2L)
  
  forest <- IntervalForest(IRangesList())
  checkTrue(validObject(forest))

  forest <- IntervalForest(IRangesList(a=IRanges(1, 0)))
  checkIdentical(as.list(start(forest)), list(a=1L))
  checkIdentical(names(forest), c("a"))
  checkTrue(validObject(forest))

  forest <- IntervalForest(split(IRanges(c(1, 1), c(1, 0)), factor(c("a","b"))))
  checkIdentical(as.list(width(forest)), list(a=1L, b=0L))
  checkIdentical(names(forest), c("a","b"))
  checkTrue(validObject(forest))

  forest <- IntervalForest(IRangesList(a=IRanges(1:10,width=1)))
  checkIdentical(as.list(start(forest)), list(a=as.integer(1:10)))
  checkIdentical(as.list(width(forest)), list(a=rep(1L,10)))
  checkTrue(validObject(forest))
  
  checkException(IntervalForest(), silent = TRUE)
  checkException(IntervalForest(subject, query), silent = TRUE)
  checkException(IntervalForest(NULL, NULL), silent = TRUE)
}

test_CompressedHitsList <- function() {
  query <- IRanges(c(1, 4, 9), c(5, 7, 10))
  qpartition <- factor(c("a","b","a"))
  ql <- split(query, qpartition)

  subject <- IRanges(c(2, 2, 10), c(2, 3, 12))
  spartition <- factor(c("a","a","b"))
  sl <- split(subject, spartition)

  olaps <- findOverlaps(ql, sl)
  rngs1 <- as(ranges(olaps, ql, sl),"CompressedIRangesList")

  hits <- new2("Hits", queryHits=queryHits(olaps), subjectHits=subjectHits(olaps),
                queryLength=nobj(ql@partitioning), subjectLength=nobj(sl@partitioning))

  hl <- CompressedHitsList(hits, sl)
  rngs2 <- ranges(hl@unlistData, ql@unlistData, sl@unlistData)
  checkIdentical(rngs1@unlistData, rngs2)
  checkIdentical(rngs1@partitioning, hl@partitioning)
}

test_IntervalForest_findOverlaps <- function() {
  ## a .....
  ## b    ....
  ## a        ..
  ## a  x
  ## b  xx
  ## a         xxx
  query <- IRanges(c(1, 4, 9), c(5, 7, 10))
  qpartition <- factor(c("a","b","a"))
  qlist <- split(query, qpartition)

  subject <- IRanges(c(2, 2, 10), c(2, 3, 12))
  spartition <- factor(c("a","a","b"))
  slist <- split(subject, spartition)

  forest <- IntervalForest(slist)

  result <- findOverlaps(qlist, forest, select = "first")
  expected <- new2("CompressedIntegerList", unlistData=c(1L, NA, NA), partitioning=PartitioningByEnd(c(1,1,2), names=c("a","b"), NG=2))
  checkIdentical(result, expected)
  
  query <- IRanges(c(1, 4, 9), c(5, 7, 10))
  qpartition <- factor(c("a","a","b"))
  qlist <- split(query, qpartition)

  result <- findOverlaps(qlist, forest, select = "first")
  expected <- new2("CompressedIntegerList", unlistData= c(1L, NA, 3L), partitioning=PartitioningByEnd(c(1,1,2), names=c("a","b"), NG=2))
  checkIdentical(result, expected)
  
  result <- findOverlaps(qlist, forest, select = "last")
  expected@unlistData <- c(2L, NA, 3L)
  checkIdentical(result, expected)
  
  result <- findOverlaps(qlist, forest, select = "arbitrary")
  checkIdentical(result, expected)

  checkOverlap <- function(a, q, s, r, c) {
    mat <- cbind(queryHits = as.integer(q), subjectHits = as.integer(s))
    checkIdentical(as.matrix(a), mat)
    checkIdentical(queryLength(a), as.integer(r))
    checkIdentical(subjectLength(a), as.integer(c))
  }

  result <- findOverlaps(qlist, forest)
  checkOverlap(result, c(1, 1, 3), c(1, 2, 3), 3, 3)
 
  ## with 'maxgap'
  result <- findOverlaps(qlist, forest, 1)
  checkOverlap(result, c(1, 1, 2, 3), c(1, 2, 2, 3), 3, 3)

  ## with 'minoverlap'
  result <- findOverlaps(qlist, forest, minoverlap = 3L)
  checkOverlap(result, integer(0), integer(0), 3, 3)
  result <- findOverlaps(qlist, forest, minoverlap = 2L)
  checkOverlap(result, 1, 2, 3, 3)
  result <- findOverlaps(qlist, forest, minoverlap = 2L, select = "first")
  expected <- new2("CompressedIntegerList", unlistData=c(2L, NA, NA), partitioning=PartitioningByEnd(c(1,1,2),names=c("a","b"), NG=2))
  checkIdentical(result, expected)
  result <- findOverlaps(qlist, forest, minoverlap = 2L, select = "last")
  expected@unlistData <- c(2L, NA, NA)
  checkIdentical(result, expected)
  result <- findOverlaps(qlist, forest, minoverlap = 2L, select = "arbitrary")
  expected@unlistData <- c(2L, NA, NA)
  checkIdentical(result, expected)

  ## empty query range
  #subject <- IRanges(c(2, 2, 10), c(2, 3, 12))
  #spartition <- factor(c("a","a","b"))
  query <- IRanges(c(1, 4, 9, 10), c(5, 7, 10, 9))
  qpartition <- factor(c("a","a","b","b"))
  qlist <- split(query, qpartition)
  result <- findOverlaps(qlist, forest)
  checkOverlap(result, c(1, 1, 3), c(1, 2, 3), 4, 3)

  ## empty subject range
  subject <- IRanges(c(2, 2, 2, 10), c(2, 1, 3, 12))
  spartition <- factor(c("a","a","a","b"))
  slist <- split(subject, spartition)
  forest <- IntervalForest(slist)
  result <- findOverlaps(qlist, forest)
  checkOverlap(result, c(1, 1, 3), c(1, 3, 4), 4, 4)

  ## .....
  ##    ....
  ##         ..
  ##  xxxx
  ##  xxx
  query <- IRanges(c(1, 4, 9), c(5, 7, 10))
  qpartition <- factor(c("a","b","a"))
  qlist <- split(query, qpartition)

  subject <- IRanges(c(2, 2), c(5, 4))
  spartition <- factor(c("a","b"))
  slist <- split(subject, spartition)

  forest <- IntervalForest(slist)
  result <- findOverlaps(qlist, forest)
  checkOverlap(result, c(1, 3), c(1, 2), 3, 2)

  
  ## check case of identical subjects
  ## .....
  ##    .....
  ##         ..
  ##  xxxx
  ##  xxxx
  ##      xx
  ##      xxx
  ##      xx
  query <- IRanges(c(1, 9, 4), c(5, 10, 7))
  qpartition <- factor(c("a","a","b"))
  qlist <- split(query, qpartition)

  subject <- IRanges(c(2, 2, 6, 6, 6), c(5, 5, 7, 8, 7))  
  spartition <- factor(c("a","a","b","b","b"))
  slist <- split(subject, spartition)

  forest <- IntervalForest(slist)
  result <- findOverlaps(qlist, forest)
  checkOverlap(result, c(1, 1, 3, 3, 3), c(1, 2, 3, 4, 5), 3, 5)

  # on unsorted query
  query <- IRanges(c(10, 7, 9, 5, 3), c(15, 10, 12, 7, 7))
  qpartition <- factor(c("a","a","a","b","b"))
  qlist <- split(query, qpartition)
  result <- findOverlaps(qlist, forest)
  checkOverlap(result, c(4, 4, 4, 5, 5, 5), c(3, 4, 5, 3, 4, 5), 5, 5)

  # query with partition level not in subject
  query <- IRanges(c(10, 7, 9, 5, 3), c(15, 10, 12, 7, 7))
  qpartition <- factor(c("a","a","a","b","c"))
  qlist <- split(query, qpartition)

  subject <- IRanges(c(2, 2, 6, 6, 6), c(5, 5, 7, 8, 7))  
  spartition <- factor(c("b","b","b","b","b"))
  slist <- split(subject, spartition)

  forest <- IntervalForest(slist)
  result <- findOverlaps(qlist, forest)
  checkOverlap(result, c(4, 4, 4, 4, 4), c(1, 2, 3, 4, 5), 5, 5)

  ## check other types of matching

  ## ..
  ##     ..
  ##   ....  
  ##    ......
  ## xxxx
  ##   xxxx
  ##     xxxxx
  ##      xxxx

  query <- IRanges(c(1, 5, 3, 4), width=c(2, 2, 4, 6))
  qpartition <- factor(c("a","a","a","a"))
  qlist <- split(query, qpartition)

  subject <- IRanges(c(1, 3, 5, 6), width=c(4, 4, 5, 4))
  spartition <- factor(c("a","a","a","a"))
  slist <- split(subject, spartition)
  forest <- IntervalForest(slist)

  ## 'start'
  result <- findOverlaps(qlist, forest, type = "start")
  checkOverlap(result, c(1, 2, 3), c(1, 3, 2), 4, 4)

  ## non-zero maxgap
  result <- findOverlaps(qlist, forest, type = "start", maxgap = 1L)
  checkOverlap(result, c(1, 2, 2, 3, 4, 4), c(1, 3, 4, 2, 2, 3), 4, 4)

  ## minoverlap > 1L
  result <- findOverlaps(qlist, forest, type = "start", minoverlap = 3L)
  checkOverlap(result, 3, 2, 4, 4)
  
  ## combine minoverlap and maxgap
  result <- findOverlaps(qlist, forest, type = "start", maxgap = 1L,
                         minoverlap = 3L)
  checkOverlap(result, c(1, 2, 3, 4, 4), c(1, 3, 2, 2, 3), 4, 4)
  
  ## 'end'
  result <- findOverlaps(qlist, forest, type = "end")
  checkOverlap(result, c(2, 3, 4, 4), c(2, 2, 3, 4), 4, 4)

#   ## ensure inverse is same as transpose
#   inverse <- findOverlaps(subject, query, type = "end")
#   tr <- as.matrix(t(result))
#   checkIdentical(as.matrix(inverse), tr[order(tr[,1]),])

  ## select = "first"
  result <- findOverlaps(qlist, forest, type = "end", select = "first")
  expected <- new2("CompressedIntegerList", unlistData=c(NA, 2L, 2L, 3L), partitioning=qlist@partitioning)
  checkIdentical(result, expected)  

  ## 'within'
  result <- findOverlaps(qlist, forest, type = "within")
  checkOverlap(result, c(1, 2, 2, 3), c(1, 2, 3, 2), 4, 4)  

  result <- findOverlaps(qlist, forest, type = "within", maxgap = 1L)
  checkOverlap(result, c(1, 2, 2, 2, 3, 4), c(1, 2, 3, 4, 2, 3), 4, 4)  
  
  ## 'equal'
  result <- findOverlaps(qlist, forest, type = "equal")
  checkOverlap(result, 3, 2, 4, 4)  

  ## self matching
  subject <- IRanges(c(2, 2, 6, 6, 6), c(5, 5, 7, 8, 7))  
  spartition <- factor(c("a","a","b","b","b"))
  slist <- split(subject, spartition)

  forest <- IntervalForest(slist)
  result <- findOverlaps(forest)
  checkOverlap(result, c(1, 1, 2, 2, 3, 3, 3, 4, 4, 4, 5, 5, 5), 
                       c(1, 2, 1, 2, 3, 4, 5, 3, 4, 5, 3, 4, 5), 5, 5)
# 
#   checkException(findOverlaps(query, NULL), silent = TRUE)
#   checkException(findOverlaps(NULL, query), silent = TRUE)

  # check empty subject partition
  query <- IRanges(start=1:2,width=1)
  qpartition <- factor(c("a","b"))
  qlist <- split(query, qpartition)

  subject <- query[1]
  slist <- new2("CompressedIRangesList", unlistData=subject, 
    partitioning=PartitioningByEnd(1L, NG=length(qlist@partitioning@end), names=qlist@partitioning@NAMES))
  forest <- IntervalForest(slist)  

  olaps1 <- findOverlaps(qlist, slist)
  olaps2 <- findOverlaps(qlist, forest)
  checkIdentical(as.matrix(olaps1), as.matrix(olaps2))

  subject <- query[2]
  slist <- new2("CompressedIRangesList", unlistData=subject, 
    partitioning=PartitioningByEnd(2L, NG=length(qlist@partitioning@end), names=qlist@partitioning@NAMES))
  forest <- IntervalForest(slist)

  olaps1 <- findOverlaps(qlist, slist)
  olaps2 <- findOverlaps(qlist, forest)
  checkIdentical(as.matrix(olaps1), as.matrix(olaps2))
}

test_IntervalForest_asRangesList <- function() {
  ranges <- IRanges(c(1, 4, 9), c(5, 7, 10))
  partition <- factor(c("a","b","a"))
  rl <- split(ranges, partition)
  forest <- IntervalForest(rl)
  checkIdentical(as(forest, "CompressedIRangesList"), rl)
  
  ranges <- IRanges()
  partition <- factor()
  rl <- split(ranges,partition)
  tree <- IntervalForest(rl)
  checkIdentical(as(tree, "CompressedIRangesList"), rl)
}

test_IntervalForest_subset <- function() {
  ranges <- IRanges(c(1, 9, 4), c(5, 10, 7))
  partition <- factor(c("a","a","b"))
  rlist <- split(ranges, partition)  
  forest <- IntervalForest(rlist)
  checkIdentical(as(forest, "IRanges"), ranges)
  
  subforest <- forest[c(1,3)]
  subranges <- ranges[c(1,3)]
  subspaces <- space(forest)[c(1,3)]
  
  checkIdentical(as(subforest,"IRanges"), subranges)
  checkIdentical(space(subforest), subspaces)
}

test_IntervalForest_range <- function() {
  compress=TRUE
    rl1 <- IntervalForest(IRangesList(a = IRanges(c(1,2),c(4,3)), b = IRanges(c(4,6),c(10,7)),
                       compress = compress))
    rl2 <- IRangesList(c = IRanges(c(0,2),c(4,5)), a = IRanges(c(4,5),c(6,7)),
                       compress = compress)
    ans <- IRangesList(a = IRanges(1,7), b = IRanges(4,10), c = IRanges(0,5),
                       compress = compress)
    checkIdentical(as(range(rl1, rl2), "CompressedIRangesList"), ans)
    names(rl2) <- NULL
    ans <- IRangesList(IRanges(0,5), IRanges(4,10), compress = compress)
    checkIdentical(as(range(rl1, rl2), "CompressedIRangesList"), ans)
}

test_IntervalForest_reduce <- function() {
  range1 <- IRanges(start=c(1,2,3), end=c(5,2,8))
  range2 <- IRanges(start=c(15,45,20,1), end=c(15,100,80,5))
  range3 <- IRanges(start=c(3,-2,6,7,-10,-2,3), width=c(1,0,0,0,0,8,0))
  range4 <- IRanges()
  collection <- IntervalForest(IRangesList(one=range1,
                              range2,
                              range3,
                              range4,
                              compress=TRUE))

  checkException(current <- reduce(collection, drop.empty.ranges=FALSE),
                 silent=TRUE)

  current <- reduce(collection, drop.empty.ranges=TRUE)
  target <- IRangesList(one=reduce(range1, drop.empty.ranges=TRUE),
                        reduce(range2, drop.empty.ranges=TRUE),
                        reduce(range3, drop.empty.ranges=TRUE),
                        reduce(range4, drop.empty.ranges=TRUE),
                        compress=TRUE)
  checkIdentical(target, as(current,"CompressedIRangesList"))
}

test_IntervalForest_narrow <- function() {
  range1 <- IRanges(start=c(2,5), end=c(3,7))
  range2 <- IRanges(start=1, end=3)
  collection <- IntervalForest(IRangesList(range1, range2, compress = TRUE))
  checkIdentical(as(narrow(collection, start=1, end=2),"CompressedIRangesList"),
                   IRangesList(IRanges(c(2, 5), c(3, 6)), IRanges(1, 2),
                               compress = TRUE))
  checkException(narrow(collection, start=10, end=20), silent = TRUE)
}

test_IntervalForest_flank <- function() {
  range1 <- IRanges(start=c(2,5), end=c(3,7))
  range2 <- IRanges(start=1, end=3)
  collection <- IRangesList(range1, range2, compress = TRUE)
  checkIdentical(as(flank(collection, 2),"CompressedIRangesList"),
                   IRangesList(IRanges(c(0, 3), c(1, 4)), IRanges(-1, 0),
                               compress = TRUE))
  checkIdentical(as(flank(collection, 2, FALSE),"CompressedIRangesList"),
                   IRangesList(IRanges(c(4, 8), c(5, 9)), IRanges(4, 5),
                               compress = TRUE))
  checkIdentical(as(flank(collection, 2, LogicalList(c(FALSE, TRUE), FALSE)),"CompressedIRangesList"),
                   IRangesList(IRanges(c(4, 3), c(5, 4)), IRanges(4, 5),
                               compress = TRUE))
  checkIdentical(as(flank(collection, IntegerList(c(2, -2), 2)),"CompressedIRangesList"),
                   IRangesList(IRanges(c(0, 5), c(1, 6)), IRanges(-1, 0),
                               compress = TRUE))
  checkIdentical(as(flank(collection, 2, both = TRUE),"CompressedIRangesList"),
                   IRangesList(IRanges(c(0, 3), c(3, 6)), IRanges(-1, 2),
                               compress = TRUE))
  checkIdentical(as(flank(collection, 2, FALSE, TRUE),"CompressedIRangesList"),
                   IRangesList(IRanges(c(2, 6), c(5, 9)), IRanges(2, 5),
                               compress = TRUE))
  checkIdentical(as(flank(collection, -2, FALSE, TRUE),"CompressedIRangesList"),
                   IRangesList(IRanges(c(2, 6), c(5, 9)),
                               IRanges(2, 5), compress = TRUE))
  checkException(flank(collection, 2, both = c(TRUE, FALSE, TRUE)),
                   silent = TRUE) # not vectorized
  checkException(flank(collection, 2, LogicalList(c(FALSE, TRUE), NA)),
                   silent = TRUE)
  checkException(flank(collection, NA), silent = TRUE)
}

test_IntervalForest_promoters <- function() {
  rl <- IntervalForest(IRangesList("A"=IRanges(5:7, width=1), "B"=IRanges(10:12, width=5)))
  current <- promoters(rl, 2, 0) 
  checkIdentical(names(current), names(rl))
  checkIdentical(unique(unlist(width(current))), 2L)
}

test_IntervalForest_resize <- function() {
  range1 <- IRanges(start=c(2,5), end=c(3,7))
  range2 <- IRanges(start=1, end=3)
    collection <- IRangesList(range1, range2, compress = TRUE)
    checkIdentical(as(resize(collection, width=10),"CompressedIRangesList"),
                   IRangesList(IRanges(c(2, 5), width=10), IRanges(1, width=10),
                               compress = TRUE))
    checkIdentical(as(resize(collection, width=10, fix="end"),"CompressedIRangesList"),
                   IRangesList(IRanges(c(-6, -2), width=10), IRanges(-6, width=10),
                               compress = TRUE))
    checkIdentical(as(resize(collection, width=10, fix="center"),"CompressedIRangesList"),
                   IRangesList(IRanges(c(-2, 1), width=10), IRanges(-3, width=10),
                               compress = TRUE))
    checkIdentical(as(resize(collection, width=10,
                          fix=CharacterList(c("start", "end"), "center")),"CompressedIRangesList"),
                   IRangesList(IRanges(c(2, -2), width=10), IRanges(-3, width=10),
                               compress = TRUE))
    checkException(resize(collection, -1), silent = TRUE)
}

test_IntervalForest_restrict <- function() {
  range1 <- IRanges(start=c(2,5), end=c(3,7))
  range2 <- IRanges(start=1, end=3)
    collection <- IntervalForest(IRangesList(range1, range2, compress = TRUE))
    checkIdentical(as(restrict(collection, start=2, end=5),"CompressedIRangesList"),
                   IRangesList(IRanges(c(2, 5), c(3, 5)), IRanges(2, 3),
                               compress = TRUE))
    checkIdentical(as(restrict(collection, start=1, end=2),"CompressedIRangesList"),
                   IRangesList(IRanges(2, 2), IRanges(1, 2),
                               compress = TRUE))
    checkException(restrict(collection, start=1, end=2, keep.all.ranges=TRUE), silent=TRUE)
}                  



test_IntervalForest_gaps <- function() {
  range1 <- IRanges(start=c(1,2,3), end=c(5,2,8))
  range2 <- IRanges(start=c(15,45,20,1), end=c(15,100,80,5))
  for (compress in c(TRUE)) {
    collection <- IntervalForest(IRangesList(one = range1, range2, compress = compress))
    checkIdentical(as(gaps(collection),"CompressedIRangesList"),
                   IRangesList(one = gaps(range1), gaps(range2),
                               compress = compress))
  }
}


test_IntervalForest_disjoin <- function()
{
  f <- function(x) as(x, "CompressedIRangesList")

    r0 <- IRanges(10, 20)
    checkTrue(validObject(disjoin(IntervalForest(IRangesList()))))
    ## unnamed; incl. 0-length
    irl <- IntervalForest(IRangesList(IRanges()))
    checkIdentical(f(irl), f(disjoin(irl)))
    irl <- IntervalForest(IRangesList(r0, IRanges(), r0))
    checkIdentical(f(irl), f(disjoin(irl)))
    irl <- IntervalForest(IRangesList(r0, IRanges(), IRanges(), r0))
    checkIdentical(f(irl), f(disjoin(irl)))
    ## named; incl. 0-length
    irl <- IntervalForest(IRangesList(a=IRanges()))
    checkIdentical(f(irl), f(disjoin(irl)))
    irl <- IntervalForest(IRangesList(a=r0, b=IRanges(), c=r0))
    checkIdentical(f(irl), f(disjoin(irl)))
    irl <- IntervalForest(IRangesList(a=r0, b=IRanges(), c=IRanges(), d=r0))
    checkIdentical(f(irl), f(disjoin(irl)))
    ## no interference between separate elements
    r0 <- IRanges(10, c(15, 20))
    dr0 <- disjoin(r0)
    irl <- IntervalForest(IRangesList(r0, r0))
    checkIdentical(IRangesList(dr0, dr0), f(disjoin(irl)))
    irl <- IntervalForest(IRangesList(r0, IRanges(), r0))
    checkIdentical(IRangesList(dr0, IRanges(), dr0), f(disjoin(irl)))
    ## 0-width
    ## 1-width
    r0 <- IRanges(c(1, 10), 10)
    irl <- IntervalForest(IRangesList(r0, IRanges()))
    checkIdentical(disjoin(r0), f(disjoin(irl))[[1]])
    irl <- IntervalForest(IRangesList(IRanges(), r0))
    checkIdentical(disjoin(r0), f(disjoin(irl))[[2]])
}
