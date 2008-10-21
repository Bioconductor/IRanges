test_RangedData_construction <- function() {
  ranges <- IRanges(c(1,2,3),c(4,5,6))
  filter <- c(1L, 0L, 1L)
  score <- c(10L, 2L, NA)

  checkException(RangedData(c(1,2,3)), silent = TRUE)
  checkException(RangedData(ranges, c(1,2,3,4,5)), silent = TRUE)

  rd <- RangedData()
  checkTrue(validObject(rd))
  rd <- RangedData(ranges)
  checkTrue(validObject(rd))
  checkIdentical(ranges(rd), RangesList(ranges))
  rd <- RangedData(ranges, score)
  checkTrue(validObject(rd))
  checkIdentical(rd[["score"]], score)
  rd <- RangedData(ranges, score = score)
  checkTrue(validObject(rd))
  checkIdentical(rd[["score"]], score)
  rd <- RangedData(ranges, filter, score = score)
  checkTrue(validObject(rd))
  checkIdentical(rd[["score"]], score)
  checkIdentical(rd[["filter"]], filter)
  rd <- RangedData(ranges, filter = filter, vals = score)
  checkTrue(validObject(rd))
  checkIdentical(rd[["vals"]], score)
  checkIdentical(rd[["filter"]], filter)
  rd <- RangedData(ranges, score + score)
  checkTrue(validObject(rd))
  checkIdentical(rd[["score...score"]], score + score)

  rd <- RangedData(ranges, annotation = "hg18")
  checkTrue(validObject(rd))
  checkIdentical(annotation(rd), "hg18")

  range2 <- IRanges(start=c(15,45,20,1), end=c(15,100,80,5))
  both <- c(ranges, range2)
  score <- c(score, c(0L, 3L, NA, 22L)) 
  chrom <- paste("chr", rep(c(1,2), c(length(ranges), length(range2))), sep="")

  rd <- RangedData(both, score, splitter = chrom, annotation = "hg18")
  checkTrue(validObject(rd))
  checkIdentical(rd[["score"]], score)
  checkIdentical(rd[1][["score"]], score[1:3])

  checkException(RangedData(ranges, annotation = c("hg18", "mm9")), silent = TRUE)
  checkException(RangedData(ranges, annotation = 1), silent = TRUE)
  checkException(RangedData(both, splitter = chrom[1:3]), silent = TRUE)
}

test_RangedData_extraction <- function() {
  ranges <- IRanges(c(1,2,3),c(4,5,6))
  filter <- c(1L, 0L, 1L)
  score <- c(10L, 2L, NA)
  rd <- RangedData(ranges, filter, score = score, splitter = c(1, 1, 2))
  
  checkException(rd[[]], silent = TRUE)
  checkException(rd[[1, 2]], silent = TRUE)
  checkException(rd[[numeric()]], silent = TRUE)
  checkException(rd[[NULL]], silent = TRUE)
  checkException(rd[[c(1,2)]], silent = TRUE)
  checkException(rd[[-1]], silent = TRUE)
  checkException(rd[[5]], silent = TRUE)

  checkIdentical(rd[["vals"]], NULL)
  checkIdentical(rd[[NA_integer_]], NULL)
  checkIdentical(rd[[1]], filter)
  checkIdentical(rd[[2]], score)
  checkIdentical(rd[["filter"]], filter)
  checkIdentical(rd[["score"]], score)

  checkIdentical(rd[1][[1]], filter[1:2])
}

test_RangedData_data_replace <- function() {
  ranges <- IRanges(c(1,2,3),c(4,5,6))
  filter <- c(1L, 0L, 1L)
  score <- c(10L, 2L, NA)
  rd <- RangedData(ranges, filter, splitter = c(1, 2, 1))
  filter <- filter[c(1, 3, 2)]
  score <- score[c(1, 3, 2)]
  
  checkException(rd[[]] <- score, silent = TRUE)
  checkException(rd[[1, 2]] <- score, silent = TRUE)
  checkException(rd[[numeric()]] <- score, silent = TRUE)
  checkException(rd[[NULL]] <- score, silent = TRUE)
  checkException(rd[[c(1,2)]] <- score, silent = TRUE)
  checkException(rd[[-1]] <- score, silent = TRUE)
  checkException(rd[[5]] <- score, silent = TRUE)
  checkException(rd[["score"]] <- numeric(), silent = TRUE)
  checkException(rd[["score"]] <- score[1:2], silent = TRUE)
  
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
  checkIdentical(ncol(rd), 1L)
}

test_RangedData_subset <- function() {
  ranges <- IRanges(c(1,2,3),c(4,5,6))
  filter <- c(1L, 0L, 1L)
  score <- c(10L, 2L, NA)
  rd <- RangedData(ranges, filter, score = score, splitter = c(1, 2, 1))
  filter <- filter[c(1, 3, 2)]
  score <- score[c(1, 3, 2)]

  checkException(rd[list()], silent = TRUE)
  checkException(rd[-18], silent = TRUE)
  checkException(rd[10], silent = TRUE)
  checkException(rd[c(NA, 2)], silent = TRUE)
  checkException(rd["one"], silent = TRUE)
  checkException(rd[c(TRUE, TRUE, TRUE, TRUE)], silent = TRUE)
  checkException(rd[c(-1,2)], silent = TRUE)

  erd <- new("RangedData")
  frd <- RangedData(ranges[c(1,3)], filter = filter[1:2], score = score[1:2])

  checkIdenticalRD <- function(a, b)
    checkIdentical(as.data.frame(a), as.data.frame(b))
  
  checkIdenticalRD(rd[numeric()], erd)
  checkIdenticalRD(rd[logical()], erd)
  checkIdenticalRD(rd[NULL], erd)
  checkIdenticalRD(rd[], rd)
  checkIdenticalRD(rd[FALSE], erd)
  checkIdenticalRD(rd[c(FALSE, FALSE)], erd)
  checkIdenticalRD(rd[TRUE], rd)
  checkIdenticalRD(rd[c(TRUE, FALSE)], frd)
  checkIdenticalRD(rd[1], frd)
  checkIdenticalRD(rd[c(1,2)], rd)
  checkIdenticalRD(rd[-2], frd)

  ## now test matrix-style
  
  checkException(rd[,100], silent = TRUE) # out of bounds col
  checkException(rd[1000,], silent = TRUE) # out of bounds row
  checkException(rd[1:3, drop=TRUE], silent = TRUE) # drop ignored
  checkException(rd[foo = "bar"], silent = TRUE) # invalid argument
  checkException(rd["Sion",], silent = TRUE) # no subsetting by row name yet
  checkException(rd[,"Fert"], silent = TRUE) # bad column name

  checkIdenticalRD(rd[,], rd) # identity

  ## empty
  nocols <- RangedData(ranges, new("XDataFrame", nrows=3L), splitter=c(1,2,1))
  checkIdenticalRD(rd[,NULL], nocols)
  checkIdenticalRD(rd[NULL,], rd[FALSE,])

  ## column subsetting
  onecol <- RangedData(ranges, filter=filter[c(1,3,2)], splitter=c(1,2,1))
  checkIdenticalRD(rd[,1], onecol)
  checkIdenticalRD(rd[,1:2], rd)
  checkIdenticalRD(rd[,"filter"], rd[,1]) # by name

  firstrow <- RangedData(ranges[1], filter = filter[1], score = score[1])
  checkIdenticalRD(rd[1,], firstrow) # row subsetting
  splitrow <- RangedData(ranges[1:2], filter = filter[c(1,3)],
                         score = score[c(1,3)])
  checkIdenticalRD(rd[c(1,3),], splitrow) # row subsetting
  
  checkIdenticalRD(rd[1:2, 1], onecol[1:2,]) # combined
  ## repeats
  repeated <- RangedData(ranges[c(1,3,1,2)], filter=filter[c(1:2,1,3)],
                         splitter = c(1,1,1,2))
  checkIdenticalRD(rd[c(1:2,1,3),1], repeated)
}

test_RangedData_combine <- function() {
  ranges <- IRanges(c(1,2,3),c(4,5,6))
  filter <- c(1L, 0L, 1L)
  score <- c(10L, 2L, NA)
  rd <- RangedData(ranges, score, splitter = filter)

  ## c()
  checkIdentical(c(rd[1], rd[2]), rd)
  checkException(c(rd[1], ranges), silent = TRUE)

  ## split()
  rd2 <- RangedData(ranges, score)
  checkIdentical(as.data.frame(unlist(split(rd2, filter))),
                 as.data.frame(rd))
  checkException(split(rd2, filter[1:2]), silent = TRUE)
}
