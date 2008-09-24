test_ValuedIRanges_construction <- function() {
  ranges <- IRanges(c(1,2,3),c(4,5,6))
  filter <- c(1L, 0L, 1L)
  score <- c(10L, 2L, NA)

  checkException(ValuedIRanges(c(1,2,3)))
  checkException(ValuedIRanges(ranges, NULL))
  checkException(ValuedIRanges(ranges, c(1,2,3,4,5)))

  vir <- ValuedIRanges()
  checkTrue(validObject(vir))
  vir <- ValuedIRanges(ranges)
  checkTrue(validObject(vir))
  checkIdentical(as(vir, "IRanges"), ranges)
  vir <- ValuedIRanges(ranges, score)
  checkTrue(validObject(vir))
  checkIdentical(vir[["score"]], score)
  vir <- ValuedIRanges(ranges, score = score)
  checkTrue(validObject(vir))
  checkIdentical(vir[["score"]], score)
  vir <- ValuedIRanges(ranges, filter, score = score)
  checkTrue(validObject(vir))
  checkIdentical(vir[["score"]], score)
  checkIdentical(vir[["filter"]], filter)
  vir <- ValuedIRanges(ranges, filter = filter, vals = score)
  checkTrue(validObject(vir))
  checkIdentical(vir[["vals"]], score)
  checkIdentical(vir[["filter"]], filter)
  vir <- ValuedIRanges(ranges, score + score)
  checkTrue(validObject(vir))
  checkIdentical(vir[["score...score"]], score + score)
}

test_ValuedIRanges_extraction <- function() {
  ranges <- IRanges(c(1,2,3),c(4,5,6))
  filter <- c(1L, 0L, 1L)
  score <- c(10L, 2L, NA)
  vir <- ValuedIRanges(ranges, filter, score = score)
  
  checkException(vir[[]])
  checkException(vir[[1, 2]])
  checkException(vir[[numeric()]])
  checkException(vir[[NULL]])
  checkException(vir[[c(1,2)]])
  checkException(vir[[-1]])
  checkException(vir[[5]])

  checkIdentical(vir[["vals"]], NULL)
  checkIdentical(vir[[NA_integer_]], NULL)
  checkIdentical(vir[[1]], filter)
  checkIdentical(vir[[2]], score)
  checkIdentical(vir[["filter"]], filter)
  checkIdentical(vir[["score"]], score)
}

test_ValuedIRanges_data_replace <- function() {
  ranges <- IRanges(c(1,2,3),c(4,5,6))
  filter <- c(1L, 0L, 1L)
  score <- c(10L, 2L, NA)
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
  checkTrue(validObject(vir))
  checkIdentical(vir[["score"]], score)
  filter2 <- c(1L, 1L, 0L)
  vir[["filter"]] <- filter2
  checkTrue(validObject(vir))
  checkIdentical(vir[["filter"]], filter2)
  ##vir[["score"]] <- score[1] # no recycling yet
  ##checkTrue(validObject(vir))
  ##checkIdentical(vir[["score"]], rep(score[1], 3))
  vir[[2]] <- score
  checkTrue(validObject(vir))
  checkIdentical(vir[[2]], score)
}

test_ValuedIRanges_subset <- function() {
  ranges <- IRanges(c(1,2,3),c(4,5,6))
  filter <- c(1L, 0L, 1L)
  score <- c(10L, 2L, NA)
  vir <- ValuedIRanges(ranges, filter, score = score)

  checkException(vir[list()])
  checkException(vir[-18])
  checkException(vir[10])
  checkException(vir[c(NA, 2)])
  checkException(vir["one"])
  checkException(vir[c(TRUE, TRUE, TRUE, TRUE)])
  checkException(vir[c(-1,2)])

  evir <- ValuedIRanges(filter = integer(), score = integer())
  fvir <- ValuedIRanges(ranges[1], filter = filter[1], score = score[1])

  checkIdenticalVIR <- function(a, b)
    checkIdentical(as.data.frame(a), as.data.frame(b))
  
  checkIdenticalVIR(vir[numeric()], evir)
  checkIdenticalVIR(vir[logical()], evir)
  checkIdenticalVIR(vir[NULL], evir)
  checkIdenticalVIR(vir[], vir)
  checkIdenticalVIR(vir[FALSE], evir)
  checkIdenticalVIR(vir[c(FALSE, FALSE, FALSE)], evir)
  checkIdenticalVIR(vir[TRUE], vir)
  checkIdenticalVIR(vir[c(TRUE, FALSE, FALSE)], fvir)
  checkIdenticalVIR(vir[1], fvir)
  checkIdenticalVIR(vir[c(1,2)],
                 ValuedIRanges(ranges[1:2], filter = filter[1:2],
                               score = score[1:2]))
  checkIdenticalVIR(vir[-c(2,3)], fvir)

  ## now test matrix-style
  
  checkException(vir[,100]) # out of bounds col
  checkException(vir[1000,]) # out of bounds row
  options(warn=2)
  checkException(vir[1:3, drop=TRUE]) # drop ignored
  checkException(vir[foo = "bar"]) # invalid argument
  options(warn=0)
  checkException(vir["Sion",]) # no subsetting by row name yet
  checkException(vir[,"Fert"]) # bad column name

  checkIdenticalVIR(vir[,], vir) # identity

  checkIdenticalVIR(vir[,NULL], ValuedIRanges(ranges)) # empty
  checkIdenticalVIR(vir[NULL,], vir[NULL])

  checkIdenticalVIR(vir[,1], ValuedIRanges(ranges, filter)) # column subsetting
  checkIdenticalVIR(vir[,1:2], vir)
  checkIdenticalVIR(vir[,"filter"], vir[,1]) # by name

  checkIdenticalVIR(vir[1,], vir[1]) # row subsetting
  checkIdenticalVIR(vir[1:3,], vir) # row subsetting
  
  checkIdenticalVIR(vir[1:2, 1], ValuedIRanges(ranges, filter)[1:2]) # combined
  ## repeats
  checkIdenticalVIR(vir[c(1:2,1),], vir[c(1:2,1)])
}

