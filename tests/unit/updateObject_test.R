## "AnnotatedList" -> "SimpleList"
test_update_AnnotatedList <- function() {
    newSimple <- SimpleList(a = 1:10, b = IRanges(1:10, 1:10))
    metadata(newSimple) <- list("simple list")
    elementMetadata(newSimple) <- DataFrame(a = 1:2, b = IRanges(1:2, 1:2))
    load(file.path("unit", "oldObjects", "oldAnnotatedList.rda"))
    checkIdentical(newSimple, updateObject(oldAnnotatedList))
}

## "LogicalList" -> "SimpleLogicalList" or "CompressedLogicalList"
## "IntegerList" -> "SimpleIntegerList" or "CompressedIntegerList"
test_update_IntegerList <- function() {
    newSimpleList <- IntegerList(a = 1:10, b = integer(), c = 0L, compress = FALSE)
    newCompressedList <- IntegerList(a = 1:10, b = integer(), c = 0L, compress = TRUE)
    load(file.path("unit", "oldObjects", "oldIntegerLists.rda"))
    checkIdentical(newSimpleList, updateObject(oldSimpleList))
    checkIdentical(newCompressedList, updateObject(oldCompressedList))
}

## "NumericList" -> "SimpleNumericList" or "CompressedNumericList"
## "ComplexList" -> "SimpleComplexList" or "CompressedComplexList"
## "CharacterList" -> "SimpleCharacterList" or "CompressedCharacterList"
## "RawList" -> "SimpleRawList" or "CompressedRawList"
## "RleList" -> "SimpleRleList" or "CompressedRleList"

## "FilterRules" -> "FilterRules"
test_update_FilterRules <- function() {
    filts <- list(peaks = expression(peaks), promoters = expression(promoters),
                  find_eboxes = function(rd) rep(FALSE, nrow(rd)))
    newFilterRules <- FilterRules(filts, active = FALSE)
    load(file.path("unit", "oldObjects", "oldFilterRules.rda"))
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
    ranges <- IRanges(c(1,2,3),c(4,5,6))
    score <- c(2L, 0L, 1L)
    rd <- RangedData(ranges, score, splitter = c("chr1","chr2","chr1"))
    countrows <- function(rd) nrow(rd)
    newRDApplyParams <- RDApplyParams(rd, countrows)
    load(file.path("unit", "oldObjects", "oldRDApplyParams.rda"))
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
    newRangesList <- RangesList(one = range1, two = range2)
    load(file.path("unit", "oldObjects", "oldRangesList.rda"))
    checkIdentical(newRangesList, updateObject(oldRangesList))
}

## "IRangesList" -> "SimpleIRangesList" or "CompressedIRangesList"
## "RangesMatchingList" -> "RangesMatchingList"
## "Rle" -> "Rle"
## "RleViews" -> "RleViews"
## "XDataFrame" -> "DataFrame"
## "XDataFrameList" -> "SimpleDataFrameList"
## "SplitXDataFrameList" -> "SimpleSplitDataFrameList" or "CompressedSplitDataFrameList"
## "XInteger" -> "XInteger"
## "XIntegerViews" -> "XIntegerViews"
## "XNumeric" -> "XNumeric"
## "XRaw" -> "XRaw"
