test_Ranges_order <- function() {
  ir1 <- IRanges(c(2,5,1,5), c(3,7,3,6))
  ir1.sort <- IRanges(c(1,2,5,5), c(3,3,6,7))
  ir1.rev <- IRanges(c(5,5,2,1), c(7,6,3,3))
  checkIdentical(sort(ir1), ir1.sort)
  checkIdentical(sort(ir1, decreasing=TRUE), ir1.rev)
  checkException(sort(ir1, decreasing=NA), silent = TRUE)
}

test_Ranges_flank <- function() {
  ir1 <- IRanges(c(2,5,1), c(3,7,3))
  checkIdentical(flank(ir1, 2), IRanges(c(0, 3, -1), c(1, 4, 0)))
  checkIdentical(flank(ir1, 2, FALSE), IRanges(c(4, 8, 4), c(5, 9, 5)))
  checkIdentical(flank(ir1, 2, c(FALSE, TRUE, FALSE)),
                 IRanges(c(4, 3, 4), c(5, 4, 5)))
  checkIdentical(flank(ir1, c(2, -2, 2)), IRanges(c(0, 5, -1), c(1, 6, 0)))
  checkIdentical(flank(ir1, 2, both = TRUE), IRanges(c(0, 3, -1), c(3, 6, 2)))
  checkIdentical(flank(ir1, 2, FALSE, TRUE), IRanges(c(2, 6, 2), c(5, 9, 5)))
  checkIdentical(flank(ir1, -2, FALSE, TRUE), IRanges(c(2, 6, 2), c(5, 9, 5)))
  checkException(flank(ir1, 2, both = c(TRUE, FALSE, TRUE)),
                 silent = TRUE) # not vectorized
  checkException(flank(ir1, 2, c(FALSE, TRUE, NA)), silent = TRUE)
  checkException(flank(ir1, NA), silent = TRUE)
}

test_Ranges_narrow <- function() {
  ir1 <- IRanges(c(2,5,1), c(3,7,3))
  checkIdentical(narrow(ir1, start=1, end=2),
                 IRanges(c(2, 5, 1), c(3, 6, 2)))
  checkException(narrow(ir1, start=10, end=20), silent = TRUE)
}

test_Ranges_reflect <- function() {
  ir1 <- IRanges(c(2,5,1), c(3,7,3))
  bounds <- IRanges(c(0, 5, 3), c(10, 6, 9))
  checkIdentical(reflect(ir1, bounds),
                 IRanges(c(7, 4, 9), c(8, 6, 11)))
  checkException(reflect(ir1, IRanges()), silent = TRUE)
}

test_Ranges_resize <- function() {
  ir1 <- IRanges(c(2,5,1), c(3,7,3))
  checkIdentical(resize(ir1, width=10),
                 IRanges(c(2, 5, 1), width=10))
  checkIdentical(resize(ir1, width=10, fix="end"),
                 IRanges(c(-6, -2, -6), width=10))
  checkIdentical(resize(ir1, width=10, fix="center"),
                 IRanges(c(-2, 1, -3), width=10))
  checkIdentical(resize(ir1, width=10, fix=c("start", "end", "center")),
                 IRanges(c(2, -2, -3), width=10))
  checkException(resize(ir1, -1), silent = TRUE)
}

test_Ranges_restrict <- function() {
  ir1 <- IRanges(c(2,5,1), c(3,7,3))
  checkIdentical(restrict(ir1, start=2, end=5),
                 IRanges(c(2, 5, 2), c(3, 5, 3)))
  checkIdentical(restrict(ir1, start=1, end=2),
                 IRanges(c(2, 1), c(2, 2)))
  checkIdentical(restrict(ir1, start=1, end=2, keep.all.ranges=TRUE),
                 IRanges(c(2, 3, 1), c(2, 2, 2)))
}

test_Ranges_range <- function() {
  ir1 <- IRanges(c(2,5,1), c(3,7,3))
  ir2 <- IRanges(c(5,2,0), c(6,3,1))
  checkIdentical(range(ir1), IRanges(1, 7))
  checkIdentical(range(ir1, ir2), IRanges(0, 7))
  checkIdentical(range(IRanges()), IRanges())
  checkException(range(ir1, c(2,3)), silent = TRUE)
}

test_Ranges_isDisjoint <- function() {
  ir1 <- IRanges(c(2,5,1), c(3,7,3))
  ir2 <- IRanges(c(2,9,5), c(3,9,6))
  ir3 <- IRanges(1, 5)
  checkIdentical(isDisjoint(ir1), FALSE)
  checkIdentical(isDisjoint(ir2), TRUE)
  checkIdentical(isDisjoint(ir3), TRUE)
}

test_Ranges_zoom <- function() {
  ir <- IRanges(c(1,5), c(3,10))
  checkIdentical(ir*1, ir)
  checkIdentical(ir*c(1,2), IRanges(c(1,6), c(3, 8)))
  checkIdentical(ir*-2, IRanges(c(-1,2), c(4, 13)))
  checkException(ir*NA_integer_, silent = TRUE)
  checkException(ir*numeric(), silent = TRUE)
  checkException(ir*c(1,2,1), silent = TRUE)
  checkException(ir[rep(1,3)]*c(1,2), silent = TRUE)
}

checkMatching <- function(a, q, s, r, c) {
  mat <- cbind(query = as.integer(q), subject = as.integer(s))
  checkIdentical(as.matrix(matchMatrix(a)), mat)
  checkIdentical(dim(a), as.integer(c(r, c)))
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
