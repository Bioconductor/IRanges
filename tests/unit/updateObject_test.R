## "AnnotatedList" -> "SimpleList"
test_update_AnnotatedList <- function() {
    newSimple <- SimpleList(a = 1:10, b = IRanges(1:10, 1:10))
    metadata(newSimple) <- list("simple list")
    elementMetadata(newSimple) <- DataFrame(a = 1:2, b = IRanges(1:2, 1:2))
    load(file.path("unit", "oldObjects", "oldAnnotatedList.rda"))
    checkIdentical(newSimple, updateObject(oldAnnotatedList))
}

## "LogicalList" -> "SimpleLogicalList" or "CompressedLogicalList"
test_update_LogicalList <- function() {
    newSimpleList <- LogicalList(a = c(TRUE, FALSE, TRUE), b = logical(), c = FALSE, compress = FALSE)
    newCompressedList <- LogicalList(a = c(TRUE, FALSE, TRUE), b = logical(), c = FALSE, compress = TRUE)
    load(file.path("unit", "oldObjects", "oldLogicalLists.rda"))
    checkIdentical(newSimpleList, updateObject(oldSimpleList))
    checkIdentical(newCompressedList, updateObject(oldCompressedList))
}

## "IntegerList" -> "SimpleIntegerList" or "CompressedIntegerList"
test_update_IntegerList <- function() {
    newSimpleList <- IntegerList(a = 1:10, b = integer(), c = 0L, compress = FALSE)
    newCompressedList <- IntegerList(a = 1:10, b = integer(), c = 0L, compress = TRUE)
    load(file.path("unit", "oldObjects", "oldIntegerLists.rda"))
    checkIdentical(newSimpleList, updateObject(oldSimpleList))
    checkIdentical(newCompressedList, updateObject(oldCompressedList))
}

## "NumericList" -> "SimpleNumericList" or "CompressedNumericList"
test_update_NumericList <- function() {
    newSimpleList <- NumericList(a = c(1, 2, 3), b = numeric(), c = 4, compress = FALSE)
    newCompressedList <- NumericList(a = c(1, 2, 3), b = numeric(), c = 4, compress = TRUE)
    load(file.path("unit", "oldObjects", "oldNumericLists.rda"))
    checkIdentical(newSimpleList, updateObject(oldSimpleList))
    checkIdentical(newCompressedList, updateObject(oldCompressedList))
}

## "ComplexList" -> "SimpleComplexList" or "CompressedComplexList"
test_update_ComplexList <- function() {
    newSimpleList <- ComplexList(a = c(1+1i, 2+2i, 3+3i), b = complex(), c = 4+4i, compress = FALSE)
    newCompressedList <- ComplexList(a = c(1+1i, 2+2i, 3+3i), b = complex(), c = 4+4i, compress = TRUE)
    load(file.path("unit", "oldObjects", "oldComplexLists.rda"))
    checkIdentical(newSimpleList, updateObject(oldSimpleList))
    checkIdentical(newCompressedList, updateObject(oldCompressedList))
}

## "CharacterList" -> "SimpleCharacterList" or "CompressedCharacterList"
test_update_CharacterList <- function() {
    newSimpleList <- CharacterList(a = letters, b = character(), c = "A", compress = FALSE)
    newCompressedList <- CharacterList(a = letters, b = character(), c = "A", compress = TRUE)
    load(file.path("unit", "oldObjects", "oldCharacterLists.rda"))
    checkIdentical(newSimpleList, updateObject(oldSimpleList))
    checkIdentical(newCompressedList, updateObject(oldCompressedList))
}

## "RawList" -> "SimpleRawList" or "CompressedRawList"
test_update_RawList <- function() {
    newSimpleList <- RawList(a = charToRaw(paste(letters, collapse = "")), b = raw(), c = charToRaw("A"), compress = FALSE)
    newCompressedList <- RawList(a = charToRaw(paste(letters, collapse = "")), b = raw(), c = charToRaw("A"), compress = TRUE)
    load(file.path("unit", "oldObjects", "oldRawLists.rda"))
    checkIdentical(newSimpleList, updateObject(oldSimpleList))
    checkIdentical(newCompressedList, updateObject(oldCompressedList))
}

## "RleList" -> "SimpleRleList" or "CompressedRleList"
test_update_RleList <- function() {
    newSimpleList <- RleList(a = Rle(1:10, 10:1), b = Rle(), c = Rle(1:10), compress = FALSE)
    newCompressedList <- RleList(a = Rle(1:10, 10:1), b = Rle(), c = Rle(1:10), compress = TRUE)
    load(file.path("unit", "oldObjects", "oldRleLists.rda"))
    checkIdentical(newSimpleList, updateObject(oldSimpleList))
    checkIdentical(newCompressedList, updateObject(oldCompressedList))
}

## "FilterRules" -> "FilterRules"
test_update_FilterRules <- function() {
    load(file.path("unit", "oldObjects", "oldFilterRules.rda"))
    findeboxes <- updateObject(oldFilterRules)@listData$find_eboxes
    filts <- list(peaks = expression(peaks), promoters = expression(promoters),
                  find_eboxes = findeboxes)
    newFilterRules <- FilterRules(filts, active = FALSE)
    checkIdentical(newFilterRules, updateObject(oldFilterRules))
}

## "IRanges" -> "IRanges"
test_update_IRanges <- function() {
    newIRanges <- IRanges(1:26, 1:26, names = letters)
    load(file.path("unit", "oldObjects", "oldIRanges.rda"))
    checkIdentical(newIRanges, updateObject(oldIRanges))
}

## "NormalIRanges" -> "NormalIRanges"
## "IntervalTree" -> "IntervalTree"
## "MaskCollection" -> "MaskCollection"
test_update_MaskCollection <- function() {
    mask1 <- Mask(mask.width=29, start=c(11, 25, 28), width=c(5, 2, 2))
    mask2 <- Mask(mask.width=29, start=c(3, 10, 27), width=c(5, 8, 1))
    mask3 <- Mask(mask.width=29, start=c(7, 12), width=c(2, 4))
    newMaskCollection <- append(append(mask1, mask2), mask3)
    load(file.path("unit", "oldObjects", "oldMaskCollection.rda"))
    checkIdentical(newMaskCollection, updateObject(oldMaskCollection))
}

## "RDApplyParams" -> "RDApplyParams"
test_update_RDApplyParams <- function() {
    load(file.path("unit", "oldObjects", "oldRDApplyParams.rda"))
    ranges <- IRanges(c(1,2,3),c(4,5,6))
    score <- c(2L, 0L, 1L)
    rd <- RangedData(ranges, score, splitter = c("chr1","chr2","chr1"))
    countrows <- updateObject(oldRDApplyParams)@applyFun
    newRDApplyParams <- RDApplyParams(rd, countrows)
    checkIdentical(newRDApplyParams, updateObject(oldRDApplyParams))
}

## "RangedData" -> "RangedData"
test_update_RangedData <- function() {
    ranges <- IRanges(c(1,2,3),c(4,5,6))
    filter <- c(1L, 0L, 1L)
    score <- c(10L, 2L, NA)
    newRangedData <- RangedData(ranges, filter, vals = score, universe = "hg18")
    load(file.path("unit", "oldObjects", "oldRangedData.rda"))
    checkIdentical(newRangedData, updateObject(oldRangedData))
}

## "RangedDataList" -> "RangedDataList"
test_update_RangedDataList <- function() {
    ranges <- IRanges(c(1,2,3),c(4,5,6))
    a <- RangedData(IRanges(c(1,2,3),c(4,5,6)), score = c(10L, 2L, NA))
    b <- RangedData(IRanges(c(1,2,4),c(4,7,5)), score = c(3L, 5L, 7L))
    newRangedDataList <- RangedDataList(sample1 = a, sample2 = b)
    load(file.path("unit", "oldObjects", "oldRangedDataList.rda"))
    checkIdentical(newRangedDataList, updateObject(oldRangedDataList))
}

## "RangesList" -> "SimpleRangesList"
test_update_RangesList <- function() {
    range1 <- IRanges(start=c(1,2,3), end=c(5,2,8))
    range2 <- IRanges(start=c(15,45,20,1), end=c(15,100,80,5))
    newRangesList <- RangesList(one = range1, two = range2, universe = "test")
    load(file.path("unit", "oldObjects", "oldRangesList.rda"))
    checkIdentical(newRangesList, updateObject(oldRangesList))
}

## "IRangesList" -> "SimpleIRangesList" or "CompressedIRangesList"
test_update_IRangesList <- function() {
    range1 <- IRanges(start=c(1,2,3), end=c(5,2,8))
    range2 <- IRanges(start=c(15,45,20,1), end=c(15,100,80,5))
    newIRangesList <- IRangesList(one = range1, two = range2, compress = TRUE,
                                  universe = "test")
    load(file.path("unit", "oldObjects", "oldIRangesList.rda"))
    checkIdentical(newIRangesList, updateObject(oldIRangesList))
}

## "RangesMatchingList" -> "RangesMatchingList"

## "Rle" -> "Rle"
test_update_Rle <- function() {
    newRle <- Rle(1:10, 10:1)
    load(file.path("unit", "oldObjects", "oldRle.rda"))
    checkIdentical(newRle, updateObject(oldRle))
}

## "RleViews" -> "RleViews"
test_update_RleViews <- function() {
    newRleViews <- Views(Rle(1:10, 10:1), start = c(1,25,33), end = c(7, 28, 49))
    load(file.path("unit", "oldObjects", "oldRleViews.rda"))
    checkIdentical(newRleViews, updateObject(oldRleViews))
}

## "XDataFrame" -> "DataFrame"
test_update_XDataFrame <- function() {
    newDataFrame <- DataFrame(swiss)
    load(file.path("unit", "oldObjects", "oldXDataFrame.rda"))
    checkIdentical(newDataFrame, updateObject(oldXDataFrame))
}

## "XDataFrameList" -> "SimpleDataFrameList"
test_update_XDataFrameList <- function() {
    score <- c(1L, 3L, NA)
    counts <- c(10L, 2L, NA)
    row.names <- c("one", "two", "three")
    newDataFrameList <-
      DataFrameList(DataFrame(vals = score, counts, row.names = row.names),
                    DataFrame(swiss))
    load(file.path("unit", "oldObjects", "oldXDataFrameList.rda"))
    checkIdentical(newDataFrameList, updateObject(oldXDataFrameList))
}

## "SplitXDataFrameList" -> "SimpleSplitDataFrameList" or "CompressedSplitDataFrameList"
test_update_SplitXDataFrameList <- function() {
    sw <- DataFrame(swiss)
    newCompressedSplitDataFrameList <- split(sw, sw[["Education"]])
    load(file.path("unit", "oldObjects", "oldSplitXDataFrameList.rda"))
    checkIdentical(newCompressedSplitDataFrameList, updateObject(oldSplitXDataFrameList))
}

## "XInteger" -> "XInteger"
## "XIntegerViews" -> "XIntegerViews"
## "XDouble" -> "XDouble"
## "XRaw" -> "XRaw"
