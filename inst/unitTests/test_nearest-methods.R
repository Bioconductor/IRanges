checkMatching <- function(a, q, s, r, c) {
  mat <- cbind(queryHits = as.integer(q), subjectHits = as.integer(s))
  checkIdentical(as.matrix(a), mat)
  checkIdentical(c(queryLength(a), subjectLength(a)), as.integer(c(r, c)))
}

test_Ranges_adjacency <- function() {
  query <- IRanges(c(1, 3, 9), c(3, 7, 10))
  subject <- IRanges(c(3, 10, 2), c(3, 12, 5))
  
  checkIdentical(precede(query, subject), c(2L, 2L, NA))
  checkIdentical(precede(IRanges(), subject), integer())
  checkIdentical(precede(query, IRanges()), rep(NA_integer_, 3))
  checkIdentical(precede(query), c(3L, 3L, NA))
  
  checkIdentical(follow(query, subject), c(NA, NA, 3L))
  checkIdentical(follow(IRanges(), subject), integer())
  checkIdentical(follow(query, IRanges()), rep(NA_integer_, 3))
  checkIdentical(follow(query), c(NA, NA, 2L))

  checkMatching(precede(query, subject, select="all"),
                c(1, 2), c(2, 2), 3, 3)

  ## xxxx          
  ##  xxx
  ##         xx
  ##               xx
  ##               xxx
  ##      ..
  ##           ..
  ## ..        
  ##             ..
  ##                  ..

  subject <- IRanges(c(1, 2, 9, 15, 15), width=c(4, 3, 2, 2, 3))
  query <- IRanges(c(6, 11, 1, 13, 18), width=c(2, 2, 2, 2, 2))

  checkMatching(precede(query, subject, select="all"),
                c(1, 2, 2, 3, 4, 4), c(3, 4, 5, 3, 4, 5), 5, 5)
  checkMatching(precede(subject, query, select="all"),
                c(1, 2, 3, 4, 5), c(1, 1, 2, 5, 5), 5, 5)

  checkMatching(follow(query, subject, select="all"),
                c(1, 1, 2, 4, 5), c(1, 2, 3, 3, 5), 5, 5)
  checkMatching(follow(subject, query, select="all"),
                c(3, 4, 5), c(1, 4, 4), 5, 5)

  checkMatching(precede(query, select="all"),
                c(1, 2, 3, 4), c(2, 4, 1, 5), 5, 5)
  checkMatching(precede(subject, select="all"),
                c(1, 2, 3, 3), c(3, 3, 4, 5), 5, 5)

  checkMatching(follow(query, select="all"),
                c(1, 2, 4, 5), c(3, 1, 2, 4), 5, 5)
  checkMatching(follow(subject, select="all"),
                c(3, 3, 4, 5), c(1, 2, 3, 3), 5, 5)
}

test_Ranges_nearest <- function() {
  query <- IRanges(c(1, 3, 9), c(2, 7, 10))
  subject <- IRanges(c(3, 5, 12), c(3, 6, 12))

  checkIdentical(nearest(query, subject), c(1L, 1L, 3L))
  checkIdentical(nearest(query), c(2L, 1L, 2L))
  checkIdentical(nearest(query, subject[c(2,3,1)]), c(3L, 3L, 2L))

  ## xxxx          
  ##  xxx
  ##         xx
  ##               xx
  ##               xxx
  ##      ..
  ##           ..
  ## ..        
  ##             ..
  ##                  ..

  subject <- IRanges(c(1, 2, 9, 15, 15), width=c(4, 3, 2, 2, 3))
  query <- IRanges(c(6, 11, 1, 13, 18), width=c(2, 2, 2, 2, 2))
  
  checkMatching(nearest(query, subject, select = "all"),
                c(1, 1, 1, 2, 3, 3, 4, 4, 5),
                c(1, 2, 3, 3, 1, 2, 4, 5, 5), 5, 5)
  checkMatching(nearest(subject, query, select = "all"),
                c(1, 2, 3, 4, 5, 5), c(3, 3, 2, 4, 4, 5), 5, 5)
  
  checkMatching(nearest(subject, select="all"),
                c(1, 2, 3, 3, 3, 3, 4, 5), c(2, 1, 1, 2, 4, 5, 5, 4), 5, 5)
  checkMatching(nearest(query, select="all"),
                c(1, 1, 2, 3, 4, 5), c(2, 3, 4, 1, 2, 4), 5, 5)
}

test_Ranges_distance <- function() {
  query <- IRanges(c(1, 3, 9), c(2, 7, 10))
  subject <- IRanges(c(3, 5, 12), c(3, 6, 12))
  checkIdentical(distance(query, subject), c(1L, 0L, 2L))
  checkIdentical(distance(IRanges(), IRanges()), integer())
  checkException(distance(query[1:2], subject), silent=TRUE)
}

test_Ranges_disjoin <- function() {
  checkIdentical(disjoin(IRanges()), IRanges())
  ir <- IRanges(c(1, 1, 4, 10), c(6, 3, 8, 10))
  checkIdentical(disjoin(ir), IRanges(c(1, 4, 7, 10), c(3, 6, 8, 10)))
}

test_Ranges_disjointBins <- function() {
  checkIdentical(disjointBins(IRanges()), integer())
  checkIdentical(disjointBins(IRanges(1, 5)), 1L)
  checkIdentical(disjointBins(IRanges(c(1, 3), c(5, 12))), c(1L, 2L))
  checkIdentical(disjointBins(IRanges(c(1, 3, 10), c(5, 12, 13))),
                 c(1L, 2L, 1L))
  checkIdentical(disjointBins(IRanges(c(3, 1, 10), c(5, 12, 13))),
                 c(2L, 1L, 2L))
}
