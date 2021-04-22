test_IRanges_constructor <- function() {
  ir0 <- IRanges()
  checkTrue(is(ir0, "IRanges"))
  checkTrue(validObject(ir0))
  checkIdentical(0L, length(ir0))

  ir1 <- IRanges(start=4:-2, end=13:7)
  checkTrue(is(ir1, "IRanges"))
  checkTrue(validObject(ir1))
  checkIdentical(7L, length(ir1))

  ir2 <- IRanges(start=4:-2, width=10)
  checkIdentical(ir1, ir2)

  ir3 <- IRanges(end=13:7, width=10)
  checkIdentical(ir1, ir3)

  ir <- IRanges(start=c(a=1, b=2, c=3), end=c(d=10))
  checkTrue(is(ir, "IRanges"))
  checkTrue(validObject(ir))
  checkIdentical(3L, length(ir))

  ir <- IRanges(matrix(1:24), 30)
  checkTrue(is(ir, "IRanges"))
  checkTrue(validObject(ir))
  checkIdentical(24L, length(ir))

  ir <- IRanges(array(1:24, 4:2), 30)
  checkTrue(is(ir, "IRanges"))
  checkTrue(validObject(ir))
  checkIdentical(24L, length(ir))

  ## Solve NAs in 'start', 'end', or 'width'.
  ir <- IRanges(start=c(NA, -2, 15, -119),
                end  =c(26, NA, 34, -100),
                width=c(20, 20, NA,   20))
  checkTrue(is(ir, "IRanges"))
  checkTrue(validObject(ir))
  checkIdentical(4L, length(ir))
  checkIdentical(c( 7L, -2L, 15L, -119L), start(ir))
  checkIdentical(c(26L, 17L, 34L, -100L), end(ir))
  checkIdentical(c(20L, 20L, 20L,   20L), width(ir))
}

