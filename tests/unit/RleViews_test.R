test_RleViews <- function() {
    x <- rep(c(NA, 3L, NA, 7L, 9L), 1:5)
    xRle <- Rle(x)
    xRleViews <- Views(xRle, start = c(1, 3, 5, 7, 9), end = c(1, 13, 11, 10, 9))
    xList <-
      lapply(seq_len(length(xRleViews)),
             function(i) subseq(x, start = start(xRleViews)[i], end = end(xRleViews)[i]))
    checkEquals(sapply(xList[-1], min), viewMins(xRleViews[-1]))
    checkEquals(sapply(xList[-1], min, na.rm = TRUE), viewMins(xRleViews[-1], na.rm = TRUE))
    checkEquals(sapply(xList[-1], max), viewMaxs(xRleViews[-1]))
    checkEquals(sapply(xList[-1], max, na.rm = TRUE), viewMaxs(xRleViews[-1], na.rm = TRUE))
    checkEquals(sapply(xList, sum), viewSums(xRleViews))
    checkEquals(sapply(xList, sum, na.rm = TRUE), viewSums(xRleViews, na.rm = TRUE))

    y <- rep(c(NA, 3.4, NA, 7.8, 9.0), 1:5)
    yRle <- Rle(y)
    yRleViews <- Views(yRle, start = c(1, 3, 5, 7, 9), end = c(1, 13, 11, 10, 9))
    yList <-
      lapply(seq_len(length(yRleViews)),
             function(i) subseq(y, start = start(yRleViews)[i], end = end(yRleViews)[i]))
    checkEquals(sapply(yList, min), viewMins(yRleViews))
    suppressWarnings(checkEquals(sapply(yList, min, na.rm = TRUE), viewMins(yRleViews, na.rm = TRUE)))
    checkEquals(sapply(yList, max), viewMaxs(yRleViews))
    suppressWarnings(checkEquals(sapply(yList, max, na.rm = TRUE), viewMaxs(yRleViews, na.rm = TRUE)))
    checkEquals(sapply(yList, sum), viewSums(yRleViews))
    checkEquals(sapply(yList, sum, na.rm = TRUE), viewSums(yRleViews, na.rm = TRUE))

    z <- rep(c(NA, 3.4-1i, NA, 7.8+3i, 9.0-2i), 1:5)
    zRle <- Rle(z)
    zRleViews <- Views(zRle, start = c(1, 3, 5, 7, 9), end = c(1, 13, 11, 10, 9))
    zList <-
      lapply(seq_len(length(zRleViews)),
             function(i) subseq(z, start = start(zRleViews)[i], end = end(zRleViews)[i]))
    checkEquals(sapply(zList, sum), viewSums(zRleViews))
    checkEquals(sapply(zList, sum, na.rm = TRUE), viewSums(zRleViews, na.rm = TRUE))
}
