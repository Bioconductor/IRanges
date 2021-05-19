test_unsplit <- function() {
  ir <- IRanges(1:5, 11:15)
  f <- factor(c("a", "b", "a", "b", "b"), c("b", "a", "c"))

  rl <- split(ir, f)
  checkIdentical(unsplit(rl, f), ir)  

  rl <- split(ir, f, drop=TRUE)
  checkIdentical(unsplit(rl, Rle(f), drop=TRUE), ir)
  checkException(unsplit(rl, f, drop=FALSE), silent=TRUE)

  v <- 1:5
  l <- splitAsList(v, f)
  checkIdentical(unsplit(l, Rle(f)), v)

  names(ir) <- letters[1:5]
  rl <- split(ir, f)
  checkIdentical(unsplit(rl, f), ir)
  
  df <- DataFrame(unname(ir), row.names=names(ir))
  dfl <- split(df, f)
  checkIdentical(unsplit(dfl, f), df)

  ir <- IRanges(1:5, 11:15)
  names(ir)[c(1,3,5)] <- letters[1:3]
  rl <- split(ir, f)
  checkIdentical(unsplit(rl, f), ir)
}

