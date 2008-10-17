test_XRanges_accessors <- function() {
  subject <- IRanges(c(2, 10), c(2, 12))  
  tree <- IntervalTree(subject)

  checkIdentical(start(tree), start(subject))
  checkIdentical(end(tree), end(subject))
  checkIdentical(width(tree), width(subject))
}

test_XRanges_utils <- function() {
  subject <- IRanges(c(2, 10, 1), c(2, 12, 5))  
  tree <- IntervalTree(subject)

  checkIdentical(as(gaps(tree), "IRanges"), gaps(subject))
  checkIdentical(as(reduce(tree), "IRanges"), reduce(subject))
}
