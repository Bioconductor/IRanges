
test_gaps <- function() {
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

test_union <- function() {
  x <- IRanges(c(1, 4, 9), c(5, 7, 10))
  y <- IRanges(c(2, 2, 10), c(2, 3, 12))
  ans <- union(x, y)
  ans0 <- IRanges(c(1, 9), c(7, 12))
  checkIdentical(ans, ans0)
}

test_intersect <- function() {
  x <- IRanges(c(1, 4, 9), c(5, 7, 10))
  y <- IRanges(c(2, 2, 10), c(2, 3, 12))
  ans <- intersect(x, y)
  ans0 <- IRanges(c(2,10),c(3,10))
  checkIdentical(ans, ans0)  
}

test_setdiff <- function() {
  x <- IRanges(c(1, 4, 9), c(5, 7, 10))
  y <- IRanges(c(2, 2, 10), c(2, 3, 12))
  ans <- setdiff(x, y)
  ans0 <- IRanges(c(1,4,9), c(1,7,9))
  checkIdentical(ans, ans0)
  ans <- setdiff(y, x)
  ans0 <- IRanges(c(11), c(12))
  checkIdentical(ans, ans0)
}

test_punion <- function() {
  x <- IRanges(start=c(1,11,21,31,41,51,61,71), end=c(5,10,25,35,40,55,65,75))
  y <- IRanges(start=c(1, 8,18,35,43,48,63,78), end=c(4,15,22,36,45,50,62,79))
  ans0 <- IRanges(start=c(1,8,18,31,41,48,61,71), end=c(5,15,25,36,45,55,65,79))
  checkIdentical(punion(x, y, fill.gap=TRUE), ans0)
  checkIdentical(punion(y, x, fill.gap=TRUE), ans0)
}

test_pintersect <- function() {
  x <- IRanges(start=c(22,22,22,22,22,22), end=c(28,28,28,28,21,21))
  y <- IRanges(start=c(25,30,29,25,22,22), end=c(30,40,40,24,21,29))
  ansMaxStart <- IRanges(start=c(25,30,29,25,22,22), end=c(28,29,28,24,21,21))
  ansStartX   <- IRanges(start=c(25,22,29,25,22,22), end=c(28,21,28,24,21,21))
  ansStartY   <- IRanges(start=c(25,30,29,25,22,22), end=c(28,29,28,24,21,21))

  checkException(pintersect(x, y), silent = TRUE)
  checkException(pintersect(y, x), silent = TRUE)

  for (resolve.empty in c("none", "max.start", "start.x")) {
      checkIdentical(x, pintersect(x, x, resolve.empty = resolve.empty))
      checkIdentical(y, pintersect(y, y, resolve.empty = resolve.empty))
  }

  checkIdentical(pintersect(x[-c(2,3)], y[-c(2,3)]), ansMaxStart[-c(2,3)])
  checkIdentical(pintersect(y[-c(2,3)], x[-c(2,3)]), ansMaxStart[-c(2,3)])

  checkIdentical(pintersect(x, y, resolve.empty = "max.start"), ansMaxStart)
  checkIdentical(pintersect(y, x, resolve.empty = "max.start"), ansMaxStart)

  checkIdentical(pintersect(x, y, resolve.empty = "start.x"), ansStartX)
  checkIdentical(pintersect(y, x, resolve.empty = "start.x"), ansStartY)
}

test_psetdiff <- function() {
  x <- IRanges(start=c(1,11,21,31,41,51,61,71), end=c(5,10,25,35,40,55,65,75))
  y <- IRanges(start=c(1, 8,18,35,43,48,63,78), end=c(4,15,22,36,45,50,62,79))

  ans <- psetdiff(x[-7], y[-7])
  ans0 <- IRanges(start=c(5,11,23,31,41,51,71), end=c(5,10,25,34,40,55,75))
  checkIdentical(ans, ans0)

  ans <- psetdiff(y[-2], x[-2])
  ans0 <- IRanges(start=c(1,18,36,43,48,63,78), end=c(0,20,36,45,50,62,79))
  checkIdentical(ans, ans0)
}

test_pgap <- function() {
  x <- IRanges(start=c(1,11,21,31,41,51,61,71), end=c(5,10,25,35,40,55,65,75))
  y <- IRanges(start=c(1, 8,18,35,43,48,63,78), end=c(4,15,22,36,45,50,62,79))

  ans <- pgap(x, y)
  checkIdentical(width(ans), c(0L, 0L, 0L, 0L, 2L, 0L, 0L, 2L))
  checkIdentical(start(ans)[width(ans) != 0L], c(41L, 76L))
}

