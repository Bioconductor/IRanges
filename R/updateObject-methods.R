## Functions for updating old TypedList to new infrastructure
toSimpleList <- function(object, newclass, newtype, ...)
{
    annotation <-
      tryCatch(slot(object, "annotation"), error = function(e) list())
    if (!is.list(annotation)) {
        if (is.null(annotation))
            annotation <- list()
        else
            annotation <- list(annotation)
    }
    new(newclass,
        listData =
        lapply(structure(slot(object, "elements"),
                         names = slot(object, "NAMES")),
               updateObject),
        elementMetadata =
        updateObject(tryCatch(slot(object, "elementMetadata"),
                              error = function(e) NULL)),
        elementType = newtype,
        metadata = annotation,
        ...)
}

toCompressedList <- function(object, newclass, newtype, ...)
{
    annotation <-
      tryCatch(slot(object, "annotation"), error = function(e) list())
    if (!is.list(annotation)) {
        if (is.null(annotation))
            annotation <- list()
        else
            annotation <- list(annotation)
    }
    new(newclass,
        partitioning = new("PartitioningByEnd",
                           end = cumsum(slot(object, "elementLengths")),
                           NAMES = slot(object, "NAMES")),
        unlistData = updateObject(slot(object, "elements")[[1L]]),
        elementMetadata =
        updateObject(tryCatch(slot(object, "elementMetadata"),
                              error = function(e) NULL)),
        elementType = newtype,
        metadata = annotation,
        ...)
}

toNewTypeList <- function(object, simpleclass, compressedclass, newtype) {
    if (slot(object, "compress"))
        toCompressedList(object, compressedclass, newtype)
    else
        toSimpleList(object, simpleclass, newtype)
}

### ###################################################################
### Update methods
### ###################################################################

## "AnnotatedList" -> "SimpleList"
setClass("AnnotatedList", representation("VIRTUAL"))
setMethod("updateObject", signature(object="AnnotatedList"),
          function(object, ..., verbose=FALSE) {
              if (verbose) message("updateObject(object = 'AnnotatedList')")
              toSimpleList(asS4(object), "SimpleList", "ANY")
          })

## "LogicalList" -> "SimpleLogicalList" or "CompressedLogicalList"
setMethod("updateObject", signature(object="LogicalList"),
          function(object, ..., verbose=FALSE) {
              if (verbose) message("updateObject(object = 'LogicalList')")
              if (!("metadata" %in% names(attributes(object)))) {
                  object <-
                    toNewTypeList(asS4(object), "SimpleLogicalList",
                                  "CompressedLogicalList", "logical")
              }
              object
          })

## "IntegerList" -> "SimpleIntegerList" or "CompressedIntegerList"
setMethod("updateObject", signature(object="IntegerList"),
          function(object, ..., verbose=FALSE) {
              if (verbose) message("updateObject(object = 'IntegerList')")
              if (!("metadata" %in% names(attributes(object)))) {
                  object <-
                    toNewTypeList(asS4(object), "SimpleIntegerList",
                                  "CompressedIntegerList", "integer")
              }
              object
          })

## "NumericList" -> "SimpleNumericList" or "CompressedNumericList"
setMethod("updateObject", signature(object="NumericList"),
          function(object, ..., verbose=FALSE) {
              if (verbose) message("updateObject(object = 'NumericList')")
              if (!("metadata" %in% names(attributes(object)))) {
                  object <-
                    toNewTypeList(asS4(object), "SimpleNumericList",
                                  "CompressedNumericList", "numeric")
              }
              object
          })

## "ComplexList" -> "SimpleComplexList" or "CompressedComplexList"
setMethod("updateObject", signature(object="ComplexList"),
          function(object, ..., verbose=FALSE) {
              if (verbose) message("updateObject(object = 'ComplexList')")
              if (!("metadata" %in% names(attributes(object)))) {
                  object <-
                    toNewTypeList(asS4(object), "SimpleComplexList",
                                  "CompressedComplexList", "complex")
              }
              object
          })

## "CharacterList" -> "SimpleCharacterList" or "CompressedCharacterList"
setMethod("updateObject", signature(object="CharacterList"),
          function(object, ..., verbose=FALSE) {
              if (verbose) message("updateObject(object = 'CharacterList')")
              if (!("metadata" %in% names(attributes(object)))) {
                  object <-
                    toNewTypeList(asS4(object), "SimpleCharacterList",
                                  "CompressedCharacterList", "character")
              }
              object
          })

## "RawList" -> "SimpleRawList" or "CompressedRawList"
setMethod("updateObject", signature(object="RawList"),
          function(object, ..., verbose=FALSE) {
              if (verbose) message("updateObject(object = 'RawList')")
              if (!("metadata" %in% names(attributes(object)))) {
                  object <-
                    toNewTypeList(asS4(object), "SimpleRawList",
                                  "CompressedRawList", "raw")
              }
              object
          })

## "RleList" -> "SimpleRleList" or "CompressedRleList"
setMethod("updateObject", signature(object="RleList"),
          function(object, ..., verbose=FALSE) {
              if (verbose) message("updateObject(object = 'RleList')")
              if (!("metadata" %in% names(attributes(object)))) {
                  object <-
                    toNewTypeList(asS4(object), "SimpleRleList",
                                  "CompressedRleList", "Rle")
              }
              object
          })

## "FilterRules" -> "FilterRules"
setMethod("updateObject", signature(object="FilterRules"),
          function(object, ..., verbose=FALSE) {
              if (verbose) message("updateObject(object = 'FilterRules')")
              if (!("metadata" %in% names(attributes(object)))) {
                  object <-
                    toSimpleList(asS4(object), "FilterRules",
                                 "expressionORfunction",
                                 active = slot(object, "active"))
              }
              object
          })

## "IRanges" -> "IRanges"
setMethod("updateObject", signature(object="IRanges"),
          function(object, ..., verbose=FALSE) {
              if (verbose) message("updateObject(object = 'IRanges')")
              if (!("metadata" %in% names(attributes(object)))) {
                  object <-
                    new("IRanges",
                        start = slot(object, "start"),
                        width = slot(object, "width"),
                        NAMES = slot(object, "NAMES"))
              }
              object
          })

## "NormalIRanges" -> "NormalIRanges"
setMethod("updateObject", signature(object="NormalIRanges"),
          function(object, ..., verbose=FALSE) {
              if (verbose) message("updateObject(object = 'NormalIRanges')")
              if (!("metadata" %in% names(attributes(object)))) {
                  object <-
                    new("NormalIRanges",
                        start = slot(object, "start"),
                        width = slot(object, "width"),
                        NAMES = slot(object, "NAMES"))
              }
              object
          })

## "IntervalTree" -> "IntervalTree"
setMethod("updateObject", signature(object="IntervalTree"),
        function(object, ..., verbose=FALSE) {
            if (verbose) message("updateObject(object = 'IntervalTree')")
            if (!("metadata" %in% names(attributes(object)))) {
                object <-
                  new("IntervalTree",
                      ptr = slot(object, "ptr"),
                      mode = slot(object, "mode"))
            }
            object
        })

## "MaskCollection" -> "MaskCollection"
setMethod("updateObject", signature(object="MaskCollection"),
          function(object, ..., verbose=FALSE) {
              if (verbose) message("updateObject(object = 'MaskCollection')")
              if (!("metadata" %in% names(attributes(object)))) {
                  object <-
                    new("MaskCollection",
                        nir_list = lapply(slot(object, "nir_list"), updateObject),
                        width = slot(object, "width"),
                        active = slot(object, "active"),
                        NAMES = slot(object, "NAMES"),
                        desc = slot(object, "desc"))
              }
              object
          })

## "RDApplyParams" -> "RDApplyParams"
setMethod("updateObject", signature(object="RDApplyParams"),
          function(object, ..., verbose=FALSE) {
              if (verbose) message("updateObject(object = 'RDApplyParams')")
              if (!("metadata" %in% names(attributes(object)))) {
                  object <-
                    new("RDApplyParams",
                        rangedData = updateObject(slot(object, "rangedData")),
                        applyFun = slot(object, "applyFun"),
                        applyParams = slot(object, "applyParams"),
                        filterRules = updateObject(slot(object, "filterRules")),
                        simplify = slot(object, "simplify"),
                        reducerFun = slot(object, "reducerFun"),
                        reducerParams = slot(object, "reducerParams"))
              }
              object
          })

## "RangedData" -> "RangedData"
setMethod("updateObject", signature(object="RangedData"),
          function(object, ..., verbose=FALSE) {
              if (verbose) message("updateObject(object = 'RangedData')")
              if (!("metadata" %in% names(attributes(object)))) {
                  ranges <- updateObject(slot(object, "ranges"))
                  values <- updateObject(slot(object, "values"))
                  if (is.null(names(ranges))) {
                      names(ranges) <- as.character(seq_len(length(ranges)))
                  }
                  if (is.null(names(values))) {
                      names(values) <- as.character(seq_len(length(values)))
                  }
                  object <- new("RangedData", ranges = ranges, values = values)
              }
              object
          })

## "RangedDataList" -> "RangedDataList"
setMethod("updateObject", signature(object="RangedDataList"),
          function(object, ..., verbose=FALSE) {
              if (verbose) message("updateObject(object = 'RangedDataList')")
              if (!("metadata" %in% names(attributes(object)))) {
                  object <-
                    toSimpleList(asS4(object), "RangedDataList", "RangedData")
              }
              object
          })

## "RangesList" -> "SimpleRangesList"
setMethod("updateObject", signature(object="RangesList"),
          function(object, ..., verbose=FALSE) {
              if (verbose) message("updateObject(object = 'RangesListList')")
              if (!("metadata" %in% names(attributes(object)))) {
                  object <-
                    toSimpleList(asS4(object), "SimpleRangesList", "Ranges")
              }
              object
          })

## "IRangesList" -> "SimpleIRangesList" or "CompressedIRangesList"
setMethod("updateObject", signature(object="IRangesList"),
          function(object, ..., verbose=FALSE) {
              if (verbose) message("updateObject(object = 'IRangesList')")
              if (!("metadata" %in% names(attributes(object)))) {
                  object <-
                    toNewTypeList(asS4(object), "SimpleIRangesList",
                                  "CompressedIRangesList", "IRanges")
              }
              object
          })

## "Rle" -> "Rle"
setMethod("updateObject", signature(object="Rle"),
          function(object, ..., verbose=FALSE) {
              if (verbose) message("updateObject(object = 'Rle')")
              if (!("metadata" %in% names(attributes(object)))) {
                  object <-
                    new("Rle",
                        values = slot(object, "values"),
                        lengths = slot(object, "lengths"))
              }
              object
          })

## "RleViews" -> "RleViews"
setMethod("updateObject", signature(object="RleViews"),
          function(object, ..., verbose=FALSE) {
              if (verbose) message("updateObject(object = 'RleViews')")
              if (!("metadata" %in% names(attributes(object)))) {
                  object <-
                    new("RleViews",
                        subject = updateObject(slot(object, "subject")),
                        start = slot(object, "start"),
                        width = slot(object, "width"),
                        NAMES = slot(object, "NAMES"))
              }
              object
          })

## "XDataFrame" -> "DataFrame"
setClass("XDataFrame", representation("VIRTUAL"))
setMethod("updateObject", signature(object="XDataFrame"),
          function(object, ..., verbose=FALSE) {
              if (verbose) message("updateObject(object = 'XDataFrame')")
              toSimpleList(asS4(object), "DataFrame", "ANY",
                           rownames = slot(object, "rownames"),
                           nrows = slot(object, "nrows"))
          })

## "XDataFrameList" -> "SimpleDataFrameList"
setClass("XDataFrameList", representation("VIRTUAL"))
setMethod("updateObject", signature(object="XDataFrameList"),
          function(object, ..., verbose=FALSE) {
              if (verbose) message("updateObject(object = 'XDataFrameList')")
              toSimpleList(asS4(object), "SimpleDataFrameList", "DataFrame")
          })

## "SplitXDataFrameList" -> "SimpleSplitDataFrameList" or "CompressedSplitDataFrameList"
setClass("SplitXDataFrameList", representation("VIRTUAL"))
setMethod("updateObject", signature(object="SplitXDataFrameList"),
          function(object, ..., verbose=FALSE) {
              if (verbose) message("updateObject(object = 'SplitXDataFrameList')")
              toNewTypeList(asS4(object), "SimpleSplitDataFrameList",
                            "CompressedSplitDataFrameList", "DataFrame")
          })

## "XVector" -> "XVector"
setMethod("updateObject", signature(object="XVector"),
          function(object, ..., verbose=FALSE) {
              if (verbose) message("updateObject(object = 'XVector')")
              if (!("metadata" %in% names(attributes(object)))) {
                  object <-
                    new(class(object),
                        shared = slot(object, "shared"),
                        offset = slot(object, "offset"),
                        length = slot(object, "length"))
              }
              object
          })

## "XIntegerViews" -> "XIntegerViews"
setMethod("updateObject", signature(object="XIntegerViews"),
          function(object, ..., verbose=FALSE) {
              if (verbose) message("updateObject(object = 'XIntegerViews')")
              if (!("metadata" %in% names(attributes(object)))) {
                  object <-
                    new("XIntegerViews",
                        subject = updateObject(slot(object, "subject")),
                        start = slot(object, "start"),
                        width = slot(object, "width"),
                        NAMES = slot(object, "NAMES"))
              }
              object
          })

