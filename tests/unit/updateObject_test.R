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
## "IRanges" -> "IRanges"
## "NormalIRanges" -> "NormalIRanges"
## "IntervalTree" -> "IntervalTree"
## "MaskCollection" -> "MaskCollection"
## "RDApplyParams" -> "RDApplyParams"
## "RangedData" -> "RangedData"
## "RangedDataList" -> "RangedDataList"
## "RangesList" -> "SimpleRangesList"
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
