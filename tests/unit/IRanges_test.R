
test_IRanges_names <- function() {
  range1 <- IRanges(start=c(1,2,3), end=c(5,2,8))
  
  checkIdentical(names(range1), NULL)
  nms <- c("a", NA, "b")
  names(range1) <- nms
  checkIdentical(names(range1), nms)
  checkTrue(validObject(nms))
  names(range1) <- NULL
  checkTrue(validObject(nms))
  checkIdentical(names(range1), NULL)
  names(range1) <- "a"
  checkTrue(validObject(range1))
  checkIdentical(names(range1), c("a", NA, NA))

  checkException(names(range1) <- c("a", "b", "c", "d"))
  checkException(names(range1) <- 1:3)
}

test_IRanges_combine <- function() {
  range <- IRanges(start=c(1,2,3,1), end=c(5,2,8,3))
  srange <- split(range, start(range) == 1)
  checkIdentical(srange,
                 RangesList(`FALSE` = range[2:3], `TRUE` = range[c(1,4)]))
  checkIdentical(do.call("c", as.list(srange)), IRanges(c(2,3,1,1), c(2,8,5,3)))
}
