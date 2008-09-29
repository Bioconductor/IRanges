test_RangedData_construction <- function() {
  ranges <- IRanges(c(1,2,3),c(4,5,6))
  filter <- c(1L, 0L, 1L)
  score <- c(10L, 2L, NA)

  checkException(RangedData(c(1,2,3)))
  checkException(RangedData(ranges))
  checkException(RangedData(ranges, c(1,2,3,4,5)))

  rd <- RangedData()
  checkTrue(validObject(rd))
  rd <- RangedData(ranges, score)
  checkTrue(validObject(rd))
  checkIdentical(values(rd), score)
  checkIdentical(ranges(rd), ranges)
  rd <- RangedDataFrame(ranges, score)
  checkTrue(validObject(rd))
  checkIdentical(rd[["score"]], score)
  rd <- RangedDataFrame(ranges, score = score)
  checkTrue(validObject(rd))
  checkIdentical(rd[["score"]], score)
  rd <- RangedDataFrame(ranges, filter, score = score)
  checkTrue(validObject(rd))
  checkIdentical(rd[["score"]], score)
  checkIdentical(rd[["filter"]], filter)
  rd <- RangedDataFrame(ranges, filter = filter, vals = score)
  checkTrue(validObject(rd))
  checkIdentical(rd[["vals"]], score)
  checkIdentical(rd[["filter"]], filter)
  rd <- RangedDataFrame(ranges, score + score)
  checkTrue(validObject(rd))
  checkIdentical(rd[["score...score"]], score + score)
}

test_RangedData_extraction <- function() {
  ranges <- IRanges(c(1,2,3),c(4,5,6))
  filter <- c(1L, 0L, 1L)
  score <- c(10L, 2L, NA)
  rd <- RangedDataFrame(ranges, filter, score = score)
  
  checkException(rd[[]])
  checkException(rd[[1, 2]])
  checkException(rd[[numeric()]])
  checkException(rd[[NULL]])
  checkException(rd[[c(1,2)]])
  checkException(rd[[-1]])
  checkException(rd[[5]])

  checkIdentical(rd[["vals"]], NULL)
  checkIdentical(rd[[NA_integer_]], NULL)
  checkIdentical(rd[[1]], filter)
  checkIdentical(rd[[2]], score)
  checkIdentical(rd[["filter"]], filter)
  checkIdentical(rd[["score"]], score)

  rd <- RangedData(ranges, filter)
  checkIdentical(rd[[1]], filter[1])
}

test_RangedData_data_replace <- function() {
  ranges <- IRanges(c(1,2,3),c(4,5,6))
  filter <- c(1L, 0L, 1L)
  score <- c(10L, 2L, NA)
  rd <- RangedDataFrame(ranges, filter)

  checkException(rd[[]] <- score)
  checkException(rd[[1, 2]] <- score)
  checkException(rd[[numeric()]] <- score)
  checkException(rd[[NULL]] <- score)
  checkException(rd[[c(1,2)]] <- score)
  checkException(rd[[-1]] <- score)
  checkException(rd[[5]] <- score)
  checkException(rd[["score"]] <- numeric())
  checkException(rd[["score"]] <- score[1:2])
  
  rd[["score"]] <- score
  checkTrue(validObject(rd))
  checkIdentical(rd[["score"]], score)
  filter2 <- c(1L, 1L, 0L)
  rd[["filter"]] <- filter2
  checkTrue(validObject(rd))
  checkIdentical(rd[["filter"]], filter2)
  ##rd[["score"]] <- score[1] # no recycling yet
  ##checkTrue(validObject(rd))
  ##checkIdentical(rd[["score"]], rep(score[1], 3))
  rd[[2]] <- score
  checkTrue(validObject(rd))
  checkIdentical(rd[[2]], score)
  rd[[2]] <- NULL
  checkTrue(validObject(rd))
  checkIdentical(ncol(values(rd)), 1L)

  rd <- RangedData(ranges, filter)
  rd[[1]] <- 0L
  filter[1] <- 0L
  checkIdentical(values(rd), filter)
}

test_RangedData_subset <- function() {
  ranges <- IRanges(c(1,2,3),c(4,5,6))
  filter <- c(1L, 0L, 1L)
  score <- c(10L, 2L, NA)
  rd <- RangedDataFrame(ranges, filter, score = score)

  checkException(rd[list()])
  checkException(rd[-18])
  checkException(rd[10])
  checkException(rd[c(NA, 2)])
  checkException(rd["one"])
  checkException(rd[c(TRUE, TRUE, TRUE, TRUE)])
  checkException(rd[c(-1,2)])

  erd <- RangedDataFrame(IRanges(), filter = integer(), score = integer())
  frd <- RangedDataFrame(ranges[1], filter = filter[1], score = score[1])

  checkIdenticalRD <- function(a, b)
    checkIdentical(as.data.frame(a), as.data.frame(b))
  
  checkIdenticalRD(rd[numeric()], erd)
  checkIdenticalRD(rd[logical()], erd)
  checkIdenticalRD(rd[NULL], erd)
  checkIdenticalRD(rd[], rd)
  checkIdenticalRD(rd[FALSE], erd)
  checkIdenticalRD(rd[c(FALSE, FALSE, FALSE)], erd)
  checkIdenticalRD(rd[TRUE], rd)
  checkIdenticalRD(rd[c(TRUE, FALSE, FALSE)], frd)
  checkIdenticalRD(rd[1], frd)
  checkIdenticalRD(rd[c(1,2,3)], rd)
  checkIdenticalRD(rd[-c(2,3)], frd)

  ## now test matrix-style
  
  checkException(rd[,100]) # out of bounds col
  checkException(rd[1000,]) # out of bounds row
  checkException(rd[1:3, drop=TRUE]) # drop ignored
  checkException(rd[foo = "bar"]) # invalid argument
  checkException(rd["Sion",]) # no subsetting by row name yet
  checkException(rd[,"Fert"]) # bad column name

  checkIdenticalRD(rd[,], rd) # identity

  ## empty
  checkIdenticalRD(rd[,NULL], RangedData(ranges, XDataFrame(row.names=1:3)))
  checkIdenticalRD(rd[NULL,], rd[NULL])

  checkIdenticalRD(rd[,1], RangedDataFrame(ranges, filter)) # column subsetting
  checkIdenticalRD(rd[,1:2], rd)
  checkIdenticalRD(rd[,"filter"], rd[,1]) # by name

  checkIdenticalRD(rd[1,], rd[1]) # row subsetting
  checkIdenticalRD(rd[1:3,], rd) # row subsetting
  
  checkIdenticalRD(rd[1:2, 1], RangedDataFrame(ranges, filter)[1:2]) # combined
  ## repeats
  checkIdenticalRD(rd[c(1:2,1),], rd[c(1:2,1)])

  ## not allowed for non-array values
  rd <- RangedData(ranges, filter)
  checkException(rd[1,1])
  checkException(rd[1,])
  checkException(rd[,1])
}

test_RangedData_rlencode <- function() {
  ranges <- IRanges(c(1,8,14),c(4,12,16))
  values <- c(1L, 2L, 3L)
  rd <- RangedData(ranges, values)
  inverse <- c(1L,1L,1L,1L,NA,NA,NA,2L,2L,2L,2L,2L,NA,3L,3L,3L)
  checkIdentical(inverse.rle(rlencode(rd)), inverse)
  checkIdentical(inverse.rle(rlencode(rd, 1, 6)), inverse[1:6])
  inverse[5:6] <- 0L
  checkIdentical(inverse.rle(rlencode(rd, 1, 6, 0)), inverse[1:6])
  ## gap value must be length 1
  checkException(inverse.rle(rlencode(rd, gapvalue=1:2))) 
  checkException(inverse.rle(rlencode(rd, gapvalue=ranges[1]))) # bad gap value
}
