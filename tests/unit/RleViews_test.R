test_RleViews <- function() {
    x <- rep(c(1L, 3L, NA, 7L, 9L), 1:5)
    xRle <- Rle(x)
    xRleViews <-
      Views(xRle, start = c(1, 3, 5, 7, 9), end = c(1, 13, 11, 10, 9), names = letters[1:5])
    xList <-
      lapply(structure(seq_len(length(xRleViews)), names = letters[1:5]),
             function(i) window(x, start = start(xRleViews)[i], end = end(xRleViews)[i]))
    checkIdentical(letters[1:5], names(viewApply(xRleViews, min)))
    checkIdentical(letters[1:5], names(viewMins(xRleViews)))
    checkIdentical(letters[1:5], names(viewMaxs(xRleViews)))
    checkIdentical(letters[1:5], names(viewSums(xRleViews)))
    checkIdentical(letters[1:5], names(viewWhichMins(xRleViews)))
    checkIdentical(letters[1:5], names(viewWhichMaxs(xRleViews)))
    checkIdentical(letters[1:5], names(viewRangeMins(xRleViews, na.rm = TRUE)))
    checkIdentical(letters[1:5], names(viewRangeMaxs(xRleViews, na.rm = TRUE)))

    checkEqualsNumeric(sapply(xList, min), viewMins(xRleViews))
    checkEqualsNumeric(sapply(xList, min), viewApply(xRleViews, min))
    checkEqualsNumeric(sapply(xList, min, na.rm = TRUE), viewMins(xRleViews, na.rm = TRUE))
    checkEqualsNumeric(sapply(xList, min, na.rm = TRUE), viewApply(xRleViews, min, na.rm = TRUE))
    checkEqualsNumeric(sapply(xList, max), viewMaxs(xRleViews))
    checkEqualsNumeric(sapply(xList, max), viewApply(xRleViews, max))
    checkEqualsNumeric(sapply(xList, max, na.rm = TRUE), viewMaxs(xRleViews, na.rm = TRUE))
    checkEqualsNumeric(sapply(xList, max, na.rm = TRUE), viewApply(xRleViews, max, na.rm = TRUE))
    checkEqualsNumeric(sapply(xList, sum), viewSums(xRleViews))
    checkEqualsNumeric(sapply(xList, sum), viewApply(xRleViews, sum))
    checkEqualsNumeric(sapply(xList, sum, na.rm = TRUE), viewSums(xRleViews, na.rm = TRUE))
    checkEqualsNumeric(sapply(xList, sum, na.rm = TRUE), viewApply(xRleViews, sum, na.rm = TRUE))

    y <- rep(c(1.2, 3.4, NA, 7.8, 9.0), 1:5)
    yRle <- Rle(y)
    yRleViews <- Views(yRle, start = c(1, 3, 5, 7, 9), end = c(1, 13, 11, 10, 9))
    yList <-
      lapply(seq_len(length(yRleViews)),
             function(i) window(y, start = start(yRleViews)[i], end = end(yRleViews)[i]))
    checkEqualsNumeric(sapply(yList, min), viewMins(yRleViews))
    checkEqualsNumeric(sapply(yList, min), viewApply(yRleViews, min))
    checkEqualsNumeric(sapply(yList, min, na.rm = TRUE), viewMins(yRleViews, na.rm = TRUE))
    checkEqualsNumeric(sapply(yList, min, na.rm = TRUE), viewApply(yRleViews, min, na.rm = TRUE))
    checkEqualsNumeric(sapply(yList, max), viewMaxs(yRleViews))
    checkEqualsNumeric(sapply(yList, max), viewApply(yRleViews, max))
    checkEqualsNumeric(sapply(yList, max, na.rm = TRUE), viewMaxs(yRleViews, na.rm = TRUE))
    checkEqualsNumeric(sapply(yList, max, na.rm = TRUE), viewApply(yRleViews, max, na.rm = TRUE))
    checkEqualsNumeric(sapply(yList, sum), viewSums(yRleViews))
    checkEqualsNumeric(sapply(yList, sum), viewApply(yRleViews, sum))
    checkEqualsNumeric(sapply(yList, sum, na.rm = TRUE), viewSums(yRleViews, na.rm = TRUE))
    checkEqualsNumeric(sapply(yList, sum, na.rm = TRUE), viewApply(yRleViews, sum, na.rm = TRUE))

    z <- rep(c(1+1i, 3.4-1i, NA, 7.8+3i, 9.0-2i), 1:5)
    zRle <- Rle(z)
    zRleViews <- Views(zRle, start = c(1, 3, 5, 7, 9), end = c(1, 13, 11, 10, 9))
    zList <-
      lapply(seq_len(length(zRleViews)),
             function(i) window(z, start = start(zRleViews)[i], end = end(zRleViews)[i]))
    checkEqualsNumeric(sapply(zList, sum), viewSums(zRleViews))
    checkEqualsNumeric(sapply(zList, sum), viewApply(zRleViews, sum))
    checkEqualsNumeric(sapply(zList, sum, na.rm = TRUE), viewSums(zRleViews, na.rm = TRUE))
    checkEqualsNumeric(sapply(zList, sum, na.rm = TRUE), viewApply(zRleViews, sum, na.rm = TRUE))
}
