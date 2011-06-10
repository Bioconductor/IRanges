test_XDouble_slice <- function() {
  ## Use slice against an Rle as an easy test
  x <- c(0.2, 0.5, 1, 1, 1, 1.5, 1.5, -.5, -.5, -.5, 10.2, 10.3)
  r <- Rle(x)
  
  for (lower in c(-0.5, 0, 1.2, 5)) {
    double.slice <- slice(x, lower)
    rle.slice <- slice(r, lower)
    checkEquals(length(double.slice), length(rle.slice))
    is.same <- sapply(1:length(double.slice), function(i) {
      d <- as.numeric(double.slice[[i]])
      r <- as.numeric(rle.slice[[i]])
      checkEqualsNumeric(d, r)
    })
    checkTrue(all(is.same))
  }
}

test_XDoubleViews_equality <- function() {
  x <- rnorm(100)
  bounds <- IRanges(c(1, 20, 50, 80), width=c(5, 10, 15, 18))
  bounds2 <- IRanges(c(10, 30, 50, 80), width=c(5, 8, 15, 18))
  v <- Views(x, bounds)
  v2 <- Views(x, bounds2)
  
  checkTrue(all(v == v))
  checkTrue(all((v != v2) == c(TRUE, TRUE, FALSE, FALSE)))
}


test_XDoubleViews_viewMins <- function() {
  x <- rnorm(100)
  bounds <- IRanges(c(1, 20, 50, 80), width=c(5, 10, 15, 18))
  v <- Views(x, bounds)
  
  val <- viewMins(v)
  expect <- sapply(1:length(bounds), function(i) {
    min(x[start(bounds)[i]:end(bounds[i])])
  })
  
  checkEqualsNumeric(val, expect)
}

test_XDoubleViews_viewMaxs <- function() {
  x <- rnorm(100)
  bounds <- IRanges(c(1, 20, 50, 80), width=c(5, 10, 15, 18))
  v <- Views(x, bounds)
  
  val <- viewMaxs(v)
  expect <- sapply(1:length(bounds), function(i) {
    max(x[start(bounds)[i]:end(bounds[i])])
  })
  checkEqualsNumeric(val, expect)
}

test_XDoubleViews_viewSums <- function() {
  x <- rnorm(100)
  bounds <- IRanges(c(1, 20, 50, 80), width=c(5, 10, 15, 18))
  v <- Views(x, bounds)
  
  val <- viewSums(v)
  expect <- sapply(1:length(bounds), function(i) {
    sum(x[start(bounds)[i]:end(bounds[i])])
  })
  
  checkEqualsNumeric(val, expect)
}

test_XDoubleViews_viewMeans <- function() {
  x <- rnorm(100)
  bounds <- IRanges(c(1, 20, 50, 80), width=c(5, 10, 15, 18))
  v <- Views(x, bounds)
  
  val <- viewMeans(v)
  expect <- sapply(1:length(bounds), function(i) {
    mean(x[start(bounds)[i]:end(bounds[i])])
  })
  
  checkEqualsNumeric(val, expect)
}

test_XDoubleViews_viewWhichMins <- function() {
  x <- rnorm(100)
  bounds <- IRanges(c(1, 20, 50, 80), width=c(5, 10, 15, 18))
  v <- Views(x, bounds)
  
  val <- viewWhichMins(v)
  expect <- sapply(1:length(bounds), function(i) {
    which.min(x[start(bounds)[i]:end(bounds[i])]) + start(bounds)[i] - 1L
  })
  
  checkIdentical(val, expect)
}

test_XDoubleViews_viewWhichMaxs <- function() {
  x <- rnorm(100)
  bounds <- IRanges(c(1, 20, 50, 80), width=c(5, 10, 15, 18))
  v <- Views(x, bounds)
  
  val <- viewWhichMaxs(v)
  expect <- sapply(1:length(bounds), function(i) {
    which.max(x[start(bounds)[i]:end(bounds[i])]) + start(bounds)[i] - 1L
  })
  
  checkIdentical(val, expect)
}
