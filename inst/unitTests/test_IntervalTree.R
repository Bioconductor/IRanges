test_IntervalTree_construction <- function() {
  query <- IRanges(c(1, 3, 9), c(5, 7, 10))
  subject <- IRanges(c(2, 10), c(2, 12))

  tree <- IntervalTree(subject)
  checkTrue(validObject(tree))
  tree <- IntervalTree(IRanges())
  checkTrue(validObject(tree))

  tree <- IntervalTree(IRanges(1, 0))
  checkIdentical(start(tree), 1L)
  checkTrue(validObject(tree))

  tree <- IntervalTree(IRanges(c(1, 1), c(1, 0)))
  checkIdentical(width(tree), c(1L, 0L))
  checkTrue(validObject(tree))

  checkException(IntervalTree(), silent = TRUE)
  checkException(IntervalTree(subject, query), silent = TRUE)
  checkException(IntervalTree(NULL), silent = TRUE)
}

test_IntervalTree_findOverlaps <- function() {
  ## .....
  ##    ....
  ##         ..
  ##  x
  ##  xx
  ##          xxx
  query <- IRanges(c(1, 4, 9), c(5, 7, 10))
  subject <- IRanges(c(2, 2, 10), c(2, 3, 12))
  tree <- IntervalTree(subject)

  result <- findOverlaps(query, tree, select = "first")
  checkIdentical(result, c(1L, NA, 3L))
  result <- findOverlaps(query, tree, select = "last")
  checkIdentical(result, c(2L, NA, 3L))
  result <- findOverlaps(query, tree, select = "arbitrary")
  checkIdentical(result, c(2L, NA, 3L))

  checkOverlap <- function(a, q, s, r, c) {
    mat <- cbind(queryHits = as.integer(q), subjectHits = as.integer(s))
    checkIdentical(as.matrix(a), mat)
    checkIdentical(queryLength(a), as.integer(r))
    checkIdentical(subjectLength(a), as.integer(c))
  }

  result <- findOverlaps(query, tree)
  checkOverlap(result, c(1, 1, 3), c(1, 2, 3), 3, 3)

  ## with 'maxgap'
  result <- findOverlaps(query, tree, 1)
  checkOverlap(result, c(1, 1, 2, 3), c(1, 2, 2, 3), 3, 3)

  ## with 'minoverlap'
  result <- findOverlaps(query, tree, minoverlap = 3L)
  checkOverlap(result, integer(0), integer(0), 3, 3)
  result <- findOverlaps(query, tree, minoverlap = 2L)
  checkOverlap(result, 1, 2, 3, 3)
  result <- findOverlaps(query, tree, minoverlap = 2L, select = "first")
  checkIdentical(result, c(2L, NA, NA))
  result <- findOverlaps(query, tree, minoverlap = 2L, select = "last")
  checkIdentical(result, c(2L, NA, NA))
  result <- findOverlaps(query, tree, minoverlap = 2L, select = "arbitrary")
  checkIdentical(result, c(2L, NA, NA))

  ## combining 'maxgap' and 'minoverlap'
  result <- findOverlaps(query, tree, maxgap = 1L, minoverlap = 2L)
  checkOverlap(result, c(1, 1, 3), c(1, 2, 3), 3, 3)
  
  ## empty query range
  query <- IRanges(c(1, 4, 9, 10), c(5, 7, 10, 9))
  result <- findOverlaps(query, tree)
  checkOverlap(result, c(1, 1, 3), c(1, 2, 3), 4, 3)

  ## empty subject range
  subject <- IRanges(c(2, 2, 2, 10), c(2, 1, 3, 12))
  tree <- IntervalTree(subject)
  result <- findOverlaps(query, tree)
  checkOverlap(result, c(1, 1, 3), c(1, 3, 4), 4, 4)

  ## .....
  ##    ....
  ##         ..
  ##  xxxx
  ##  xxx
  query <- IRanges(c(1, 4, 9), c(5, 7, 10))
  subject <- IRanges(c(2, 2), c(5, 4))

  tree <- IntervalTree(subject)
  result <- findOverlaps(query, tree)
  checkOverlap(result, c(1, 1, 2, 2), c(1, 2, 1, 2), 3, 2)

  result <- findOverlaps(subject, query)
  checkOverlap(result, c(1, 1, 2, 2), c(1, 2, 1, 2), 2, 3)
  
  query <- IRanges(c(1, 4, 9, 11), c(5, 7, 10, 11))

  result <- findOverlaps(query)
  checkOverlap(result, c(1, 1, 2, 2, 3, 4), c(1, 2, 1, 2, 3, 4), 4, 4)

  ## check case of identical subjects
  ## .....
  ##    .....
  ##         ..
  ##  xxxx
  ##  xxxx
  ##      xx
  ##      xxx
  ##      xx
  query <- IRanges(c(1, 4, 9), c(5, 7, 10))
  subject <- IRanges(c(2, 2, 6, 6, 6), c(5, 5, 7, 8, 7))  
  tree <- IntervalTree(subject)
  result <- findOverlaps(query, tree)
  checkOverlap(result, c(1, 1, 2, 2, 2, 2, 2), c(1, 2, 1, 2, 3, 4, 5), 3, 5)

  subject <- IRanges(c(1, 6, 13), c(4, 9, 14)) # single points
  checkIdentical(findOverlaps(c(3L, 7L, 10L), subject, select = "first"),
                 c(1L, 2L, NA))
  checkIdentical(findOverlaps(c(3L, 7L, 10L), subject, select = "last"),
                 c(1L, 2L, NA))
  checkIdentical(findOverlaps(c(3L, 7L, 10L), subject, select = "arbitrary"),
                 c(1L, 2L, NA))
  checkIdentical(findOverlaps(IRanges(c(2,1),c(3,4)), subject),
                 new("Hits",
                     queryHits = 1:2, subjectHits = c(1L,1L),
                     queryLength = 2L, subjectLength = 3L))

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
  subject <- IRanges(c(1, 3, 5, 6), width=c(4, 4, 5, 4))
  tree <- IntervalTree(subject)

  ## 'start'
  result <- findOverlaps(query, tree, type = "start")
  checkOverlap(result, c(1, 2, 3), c(1, 3, 2), 4, 4)

  ## non-zero maxgap
  result <- findOverlaps(query, tree, type = "start", maxgap = 1L)
  checkOverlap(result, c(1, 2, 2, 3, 4, 4), c(1, 3, 4, 2, 2, 3), 4, 4)

  ## minoverlap > 1L
  result <- findOverlaps(query, tree, type = "start", minoverlap = 3L)
  checkOverlap(result, 3, 2, 4, 4)
  
  ## combine minoverlap and maxgap
  result <- findOverlaps(query, tree, type = "start", maxgap = 1L,
                         minoverlap = 3L)
  checkOverlap(result, c(1, 2, 3, 4, 4), c(1, 3, 2, 2, 3), 4, 4)
  
  ## 'end'
  result <- findOverlaps(query, tree, type = "end")
  checkOverlap(result, c(2, 3, 4, 4), c(2, 2, 3, 4), 4, 4)

  ## ensure inverse is same as transpose
  inverse <- findOverlaps(subject, query, type = "end")
  tr <- as.matrix(t(result))
  checkIdentical(as.matrix(inverse), tr[order(tr[,1]),])

  ## select = "first"
  result <- findOverlaps(query, tree, type = "end", select = "first")
  checkIdentical(result, c(NA, 2L, 2L, 3L))  

  ## 'within'
  result <- findOverlaps(query, tree, type = "within")
  checkOverlap(result, c(1, 2, 2, 3), c(1, 2, 3, 2), 4, 4)  

  result <- findOverlaps(query, tree, type = "within", maxgap = 1L)
  checkOverlap(result, c(1, 2, 2, 2, 3, 4), c(1, 2, 3, 4, 2, 3), 4, 4)  
  
  ## 'equal'
  result <- findOverlaps(query, tree, type = "equal")
  checkOverlap(result, 3, 2, 4, 4)  

  checkException(findOverlaps(query, NULL), silent = TRUE)
  checkException(findOverlaps(NULL, query), silent = TRUE)
}

test_IntervalTree_asRanges <- function() {
  ranges <- IRanges(c(1, 4, 9), c(5, 7, 10))
  tree <- IntervalTree(ranges)
  checkIdentical(as(tree, "IRanges"), ranges)

  ranges <- IRanges()
  tree <- IntervalTree(ranges)
  checkIdentical(as(tree, "IRanges"), ranges)
}

test_IntervalTree_length <- function() {
  ranges <- IRanges(c(1, 4, 9), c(5, 7, 10))
  tree <- IntervalTree(ranges)
  checkIdentical(length(tree), length(ranges))
}
