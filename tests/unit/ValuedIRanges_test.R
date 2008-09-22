
test_ValuedIRanges_construction <- function() {
  ranges <- IRanges(c(1,2,3),c(4,5,6))
  filter <- c(TRUE, FALSE, TRUE)
  score <- c(10, 2, NA)

  checkException(ValuedIRanges(c(1,2,3)))
  checkException(ValuedIRanges(ranges, NULL))
  checkException(ValuedIRanges(ranges, c(1,2,3,4,5)))

  vir <- ValuedIRanges()
  checkTRUE(validObject(vir))
  vir <- ValuedIRanges(ranges)
  checkTRUE(validObject(vir))
  checkIdentical(ranges(vir), ranges)
  vir <- ValuedIRanges(ranges, score)
  checkTRUE(validObject(vir))
  vir <- ValuedIRanges(score = score)
  checkTRUE(validObject(vir))
  checkIdentical(vir[["score"]], score)
  vir <- ValuedIRanges(ranges, filter, score = score)
  checkTRUE(validObject(vir))
  checkIdentical(vir[["score"]], score)
  checkIdentical(vir[["filter"]], filter)
  vir <- ValuedIRanges(ranges, filter = filter, vals = score)
  checkTRUE(validObject(vir))
  checkIdentical(vir[["score"]], score)
  checkIdentical(vir[["filter"]], vals)
  vir <- ValuedIRanges(ranges, score + score)
  checkTRUE(validObject(vir))
  checkIdentical(vir[["score...score"]], score + score)
}

test_RangesCollection_extraction <- function() {
  ranges <- IRanges(c(1,2,3),c(4,5,6))
  filter <- c(TRUE, FALSE, TRUE)
  score <- c(10, 2, NA)
  vir <- ValuedIRanges(ranges, filter, score = score)
  
  checkException(vir[[]])
  checkException(vir[["vals"]])
  checkException(vir[[1, 2]])
  checkException(vir[[numeric()]])
  checkException(vir[[NULL]])
  checkException(vir[[c(1,2)]])
  checkException(vir[[-1]])
  checkException(vir[[5]])

  checkIdentical(vir[[NA]], NULL)
  checkIdentical(vir[[1]], filter)
  checkIdentical(vir[[2]], score)
  checkIdentical(vir[["filter"]], filter)
  checkIdentical(vir[["score"]], score)
}

test_RangesCollection_data_replace <- function() {
  ranges <- IRanges(c(1,2,3),c(4,5,6))
  filter <- c(TRUE, FALSE, TRUE)
  score <- c(10, 2, NA)
  vir <- ValuedIRanges(ranges, filter)

  checkException(vir[[]] <- score)
  checkException(vir[[1, 2]] <- score)
  checkException(vir[[numeric()]] <- score)
  checkException(vir[[NULL]] <- score)
  checkException(vir[[c(1,2)]] <- score)
  checkException(vir[[-1]] <- score)
  checkException(vir[[5]] <- score)
  checkException(vir[["score"]] <- numeric())
  checkException(vir[["score"]] <- score[1:2])
  
  vir[["score"]] <- score
  checkTRUE(validObject(vir))
  checkIdentical(vir[["score"]], score)
  filter2 <- c(TRUE, TRUE, FALSE)
  vir[["filter"]] <- filter2
  checkTRUE(validObject(vir))
  checkIdentical(vir[["filter"]], filter2)
  vir[["score"]] <- score[1]
  checkTRUE(validObject(vir))
  checkIdentical(vir[["score"]], rep(score[1], 3))
  vir[[2]] <- score
  checkTRUE(validObject(vir))
  checkIdentical(vir[[2]], score)
}


test_RangeData_subset <- function() {
  ranges <- IRanges(c(1,2,3),c(4,5,6))
  filter <- c(TRUE, FALSE, TRUE)
  score <- c(10, 2, NA)
  vir <- ValuedIRanges(ranges, filter, score = score)

  checkException(vir[1,2])
  checkException(vir[list()])
  checkException(vir[-18])
  checkException(vir[10])
  checkException(vir[c(NA, 2)])
  checkException(vir["one"])
  checkException(vir[c(TRUE, TRUE, TRUE)])
  checkException(vir[c(-1,2)])

  evir <- ValuedIRanges(filter = numeric(), score = numeric())
  fvir <- ValuedIRanges(ranges[1], filter = filter[1], score = score[1])
  
  checkIdentical(vir[numeric()], evir)
  checkIdentical(vir[logical()], evir)
  checkIdentical(vir[NULL], evir)
  checkIdentical(vir[], vir)
  checkIdentical(vir[FALSE], evir)
  checkIdentical(vir[c(FALSE, FALSE)], evir)
  checkIdentical(vir[TRUE], vir)
  checkIdentical(vir[c(TRUE, FALSE)], fvir)
  checkIdentical(vir[1], fvir)
  checkIdentical(vir[c(2,1)],
                 ValuedIRanges(ranges[2:1], filter = filter[2:1],
                               score = score[2:1]))
  checkIdentical(vir[-c(2,3)], fvir)
}

