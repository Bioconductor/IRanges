test_RangedData_construction <- function() {
  ranges <- IRanges(c(1,2,3),c(4,5,6))
  filter <- c(1L, 0L, 1L)
  score <- c(10L, 2L, NA)

  checkException(RangedData(c(1,2,3)), silent = TRUE)
  checkException(RangedData(ranges, c(1,2,3,4,5)), silent = TRUE)

  rd <- RangedData()
  checkTrue(validObject(rd))
  rd <- RangedData(IRanges())
  checkTrue(validObject(rd))
  rd <- RangedData(IRangesList())
  checkTrue(validObject(rd))
  rd <- RangedData(IRangesList(IRanges()))
  checkTrue(validObject(rd))
  rd <- RangedData(IRangesList(IRanges(), IRanges()))
  checkTrue(validObject(rd))
  rd <- RangedData(IRangesList(IRanges(), IRanges(1,1)))
  checkTrue(validObject(rd))
  rd <- RangedData(ranges)
  checkTrue(validObject(rd))
  checkIdentical(unname(ranges(rd)), IRangesList(ranges))
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

  rd <- RangedData(ranges, universe = "hg18")
  checkTrue(validObject(rd))
  checkIdentical(universe(rd), "hg18")

  range2 <- IRanges(start=c(15,45,20,1), end=c(15,100,80,5))
  both <- c(ranges, range2)
  score <- c(score, c(0L, 3L, NA, 22L)) 
  chrom <- paste("chr", rep(c(1,2), c(length(ranges), length(range2))), sep="")

  rd <- RangedData(both, score, space = chrom, universe = "hg18")
  checkTrue(validObject(rd))
  checkIdentical(rd[["score"]], score)
  checkIdentical(rd[1][["score"]], score[1:3])

  checkException(RangedData(ranges, universe = c("hg18", "mm9")), silent = TRUE)
  checkException(RangedData(ranges, universe = 1), silent = TRUE)
  checkException(RangedData(both, space = chrom[1:3]), silent = TRUE)
}

test_RangedData_extraction <- function() {
  ranges <- IRanges(c(1,2,3),c(4,5,6))
  filter <- c(1L, 0L, 1L)
  score <- c(10L, 2L, NA)
  rd <- RangedData(ranges, filter, score = score, space = c(1, 1, 2))
  
  checkException(rd[[]], silent = TRUE)
  checkException(rd[[1, 2]], silent = TRUE)
  checkException(rd[[numeric()]], silent = TRUE)
  checkException(rd[[NULL]], silent = TRUE)
  checkException(rd[[c(1,2)]], silent = TRUE)
  checkException(rd[[-1]], silent = TRUE)
  checkException(rd[[5]], silent = TRUE)

  checkIdentical(rd[["vals"]], NULL)
  checkIdentical(rd$vals, NULL)
  checkIdentical(rd[[NA_integer_]], NULL)
  checkIdentical(rd[[1]], filter)
  checkIdentical(rd[[2]], score)
  checkIdentical(rd[["filter"]], filter)
  checkIdentical(rd[["score"]], score)
  checkIdentical(rd$score, score)

  checkIdentical(rd[1][[1]], filter[1:2])
}

test_RangedData_values_replace <- function() {
    ranges <- IRanges(c(1,2,3),c(4,5,6))
    filter <- c(1L, 0L, 1L)
    score <- c(10L, 2L, NA)

    rd1 <- RangedData(ranges, filter, space = c(1, 2, 1))
    values(rd1) <- split(DataFrame(filter=score), c(1, 2, 1))
    rd2 <- RangedData(ranges, filter=score, space = c(1, 2, 1))
    checkIdentical(rd1, rd2)

    rd1 <- RangedData(ranges, filter, space = c(1, 2, 1))
    values(rd1) <- DataFrame(filter=score[c(1,3,2)])
    rd2 <- RangedData(ranges, filter=score, space = c(1, 2, 1))
    checkIdentical(rd1, rd2)
}

test_RangedData_ranges_replace <- function() {
    ranges1 <- IRanges(c(1,2,3),c(4,5,6))
    ranges2 <- IRanges(c(3,2,1),c(4,5,6))
    filter <- c(1L, 0L, 1L)
    score <- c(10L, 2L, NA)

    rd1 <- RangedData(ranges1, filter, space = c(1, 2, 1))
    ranges(rd1) <- split(ranges2, c(1, 2, 1))
    rd2 <- RangedData(ranges2, filter, space = c(1, 2, 1))
    checkIdentical(rd1, rd2)

    rd1 <- RangedData(ranges1, filter, space = c(1, 2, 1))
    ranges(rd1) <- ranges2[c(1,3,2)]
    rd2 <- RangedData(ranges2, filter, space = c(1, 2, 1))
    checkIdentical(rd1, rd2)
}

test_RangedData_data_column_replace <- function() {
  ranges <- IRanges(c(1,2,3),c(4,5,6))
  filter <- c(1L, 0L, 1L)
  score <- c(10L, 2L, NA)
  rd <- RangedData(ranges, filter, space = c(1, 2, 1))
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
  rd$score2 <- score
  checkIdentical(rd$score2, score)
}

test_RangedData_subset <- function() {
  ranges <- IRanges(c(1,2,3),c(4,5,6))
  filter <- c(1L, 0L, 1L)
  score <- c(10L, 2L, NA)
  rd <- RangedData(ranges, filter, score = score, space = c(1, 2, 1))
  filter <- filter[c(1, 3, 2)]
  score <- score[c(1, 3, 2)]

  checkException(rd[list()], silent = TRUE)
  checkException(rd[10], silent = TRUE)
  checkException(rd[c(NA, 2)], silent = TRUE)
  checkException(rd["one"], silent = TRUE)
  checkException(rd[c(TRUE, TRUE, TRUE, TRUE)], silent = TRUE)
  checkException(rd[c(-1,2)], silent = TRUE)

  erd <- new("RangedData")
  names(erd) <- character(0)
  frd <- RangedData(ranges[c(1,3)], filter = filter[1:2], score = score[1:2],
                    space = 1)

  checkIdenticalRD <- function(a, b) {
    checkIdentical(as(ranges(a), "CompressedIRangesList"),
                   as(ranges(b), "CompressedIRangesList"))
    checkIdentical(length(values(a)), length(values(b)))
    if (length(values(a)) > 0)
        checkIdentical(as.data.frame(values(a)), as.data.frame(values(b)))
    else TRUE
  }

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
  checkException(rd[foo = "bar"], silent = TRUE) # invalid argument
  checkException(rd["Sion",], silent = TRUE) # no subsetting by row name yet
  checkException(rd[,"Fert"], silent = TRUE) # bad column name

  checkIdenticalRD(rd[,], rd) # identity

  ## empty
  nocols <- RangedData(ranges, new("DataFrame", nrows=3L), space=c(1,2,1))
  checkIdenticalRD(rd[,NULL], nocols)
  checkIdenticalRD(rd[NULL,], rd[FALSE,])

  ## column subsetting
  onecol <- RangedData(ranges, filter=filter[c(1,3,2)], space=c(1,2,1))
  checkIdenticalRD(rd[,1], onecol)
  checkIdenticalRD(rd[,1:2], rd)
  checkIdenticalRD(rd[,"filter"], rd[,1]) # by name

  firstrow <- RangedData(ranges[1], filter = filter[1], score = score[1],
                         space = 1)
  checkIdenticalRD(rd[1,,drop=TRUE], firstrow) # row subsetting
  splitrow <- RangedData(ranges[1:2], filter = filter[c(1,3)],
                         score = score[c(1,3)], space = c(1,2))
  checkIdenticalRD(rd[c(1,3),], splitrow) # row subsetting
  
  checkIdenticalRD(rd[1:2, 1], onecol[1:2,]) # combined
  ## repeats
  repeated <- RangedData(ranges[c(1,3,1,2)], filter=filter[c(1:2,1,3)],

                         space = c(1,1,1,2))
  checkIdenticalRD(rd[c(1:2,1,3),1], repeated)
}

test_RangedData_combine <- function() {
  ranges <- IRanges(c(1,2,3),c(4,5,6))
  filter <- c(1L, 0L, 1L)
  score <- c(10L, 2L, NA)
  rd <- RangedData(ranges, score, space = filter)

  ## c()
  checkTrue(validObject(c(rd[1], rd[2])))
  checkIdentical(ranges(c(rd[1], rd[2])), ranges(rd))
  checkIdentical(as.data.frame(values(c(rd[1], rd[2]))),
                 as.data.frame(values(rd)))
  checkException(c(rd[1], ranges), silent = TRUE)

  ## split()
  rd2 <- RangedData(ranges, score)
  checkIdentical(as.data.frame(unlist(split(rd2, filter))),
                 as.data.frame(rd2[order(filter),]))
  checkException(split(rd2, filter[1:2]), silent = TRUE)

  ## rbind()
  ranges <- IRanges(c(1,2,3),c(4,5,6))
  filter <- c(1L, 0L, 1L)
  score <- c(10L, 2L, NA)
  rd1 <- RangedData(ranges, score, space = filter)
  score2 <- c(15L, 10L, 3L)
  space2 <- c(0L, 1L, 0L)
  ranges2 <- IRanges(c(2,5,1), c(8, 6, 9))
  rd2 <- RangedData(ranges2, score = score2, space = space2)
  rd <- RangedData(c(ranges, ranges2), score=c(score,score2),
                   space=c(filter, space2))
  checkIdentical(as.data.frame(rbind(rd1, rd2)), as.data.frame(rd))
  rownames(rd1) <- letters[seq_len(nrow(rd1))]
  rownames(rd2) <- letters[seq_len(nrow(rd2))]
  checkTrue(validObject(rbind(rd1, rd2)))

  universe(rd2) <- "foo"
  checkException(rbind(rd1, rd2), silent=TRUE)
}

test_RangedData_lapply <- function() {
  ranges <- IRanges(c(1,2,3),c(4,5,6))
  filter <- c(1L, 0L, 1L)
  score <- c(10L, 2L, NA)
  rd <- RangedData(ranges, score, space = filter)
  checkIdentical(lapply(rd, `[[`, 1), list(`0` = 2L, `1` = c(10L, NA)))  
}

test_RangedData_range <- function() {
  rd1 <- RangedData(IRanges(c(2,5,1), c(3,7,3)))
  rd2 <- RangedData(IRanges(c(5,2,0), c(6,3,1)))
  checkIdentical(range(rd1), IRangesList("1" = IRanges(1, 7)))
  checkIdentical(range(rd1, rd2), IRangesList("1" = IRanges(0, 7)))
  checkException(range(rd1, c(2,3)), silent = TRUE)
}

test_RangedData_dimnames <- function() {
  ranges <- IRanges(c(1,2,3),c(4,5,6))
  filter <- c(1L, 0L, 1L)
  score <- c(10L, 2L, NA)
  rd <- RangedData(ranges, filter, score = score, space = c(1, 2, 1))
  colnames(rd)[2] <- "foo"
  checkTrue(validObject(rd))
  checkIdentical(colnames(rd), c("filter", "foo"))
  rownames(rd) <- c("a", "b", "c")
  checkTrue(validObject(rd))
  checkIdentical(rownames(rd), c("a", "b", "c"))
}

test_RangedData_fromDataFrame <- function() {
  df <- data.frame(start = c(1, 2, 3), end = c(4, 2, 3))
  rd <- RangedData(IRanges(c(1,2,3), c(4,2,3)), df[,NULL])
  checkIdentical(as(df, "RangedData"), rd)
  checkException(as(df[,1,drop=FALSE], "RangedData"), rd, silent=TRUE)
  df <- data.frame(start = c(1, 2, 3), end = c(4, 2, 3), space = c(1, 1, 2),
                   foo = c("a", "b", "c"))
  rd <- RangedData(IRanges(c(1,2,3), c(4,2,3)), df[,"foo",drop=FALSE],
                   space = c(1,1,2))
  checkIdentical(as(df, "RangedData"), rd)
}

test_RangedData_legacy_data.frame <- function() {
    ranges <- IRanges(c(1,2,3),c(4,5,6))
    rd <- RangedData(ranges)
    current <- as.data.frame(rd)

    expected <- data.frame(space = factor(rep(1, 3)), start = 1:3,
                           end = 4:6, width = rep(4L, 3)) 
    checkIdentical(current, expected)
}
