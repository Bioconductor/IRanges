test_RleViews <- function() {
    x <- rep(c(1L, 3L, NA, 7L, 9L), 1:5)
    xRle <- Rle(x)
    xRleViews <- Views(xRle, start = c(1, 3, 5, 7, 9), end = c(1, 13, 11, 10, 9))
    xList <-
      lapply(seq_len(length(xRleViews)),
             function(i) subseq(x, start = start(xRleViews)[i], end = end(xRleViews)[i]))
    checkEqualsNumeric(sapply(xList, min), viewMins(xRleViews))
    checkEqualsNumeric(sapply(xList, min, na.rm = TRUE), viewMins(xRleViews, na.rm = TRUE))
    checkEqualsNumeric(sapply(xList, max), viewMaxs(xRleViews))
    checkEqualsNumeric(sapply(xList, max, na.rm = TRUE), viewMaxs(xRleViews, na.rm = TRUE))
    checkEqualsNumeric(sapply(xList, sum), viewSums(xRleViews))
    checkEqualsNumeric(sapply(xList, sum, na.rm = TRUE), viewSums(xRleViews, na.rm = TRUE))

    y <- rep(c(1.2, 3.4, NA, 7.8, 9.0), 1:5)
    yRle <- Rle(y)
    yRleViews <- Views(yRle, start = c(1, 3, 5, 7, 9), end = c(1, 13, 11, 10, 9))
    yList <-
      lapply(seq_len(length(yRleViews)),
             function(i) subseq(y, start = start(yRleViews)[i], end = end(yRleViews)[i]))
    checkEqualsNumeric(sapply(yList, min), viewMins(yRleViews))
    suppressWarnings(checkEqualsNumeric(sapply(yList, min, na.rm = TRUE), viewMins(yRleViews, na.rm = TRUE)))
    checkEqualsNumeric(sapply(yList, max), viewMaxs(yRleViews))
    suppressWarnings(checkEqualsNumeric(sapply(yList, max, na.rm = TRUE), viewMaxs(yRleViews, na.rm = TRUE)))
    checkEqualsNumeric(sapply(yList, sum), viewSums(yRleViews))
    checkEqualsNumeric(sapply(yList, sum, na.rm = TRUE), viewSums(yRleViews, na.rm = TRUE))

    z <- rep(c(1+1i, 3.4-1i, NA, 7.8+3i, 9.0-2i), 1:5)
    zRle <- Rle(z)
    zRleViews <- Views(zRle, start = c(1, 3, 5, 7, 9), end = c(1, 13, 11, 10, 9))
    zList <-
      lapply(seq_len(length(zRleViews)),
             function(i) subseq(z, start = start(zRleViews)[i], end = end(zRleViews)[i]))
    checkEqualsNumeric(sapply(zList, sum), viewSums(zRleViews))
    checkEqualsNumeric(sapply(zList, sum, na.rm = TRUE), viewSums(zRleViews, na.rm = TRUE))
}
