### =========================================================================
### AnnotatedTypedListV2 objects
### -------------------------------------------------------------------------

## NOTE: this will throw warnings, because we use XDataFrame, which
## must be defined after this, since it is itself an AnnotatedList. As
## long as NULL is allowed, infinite recursion is avoided.
setClass("AnnotatedTypedListV2",
        contains=c("Annotated", "TypedListV2"),
        representation(
                       "VIRTUAL",
                       elementMetadata = "XDataFrameORNULL"
        ),
        prototype(elementType="ANYTHING")
)

setClass("AnnotatedCompressedTypedList",
        contains=c("AnnotatedTypedListV2", "CompressedTypedList"),
        representation("VIRTUAL")
)

setClass("AnnotatedSimpleTypedList",
        contains=c("AnnotatedTypedListV2", "SimpleTypedList"),
        representation("VIRTUAL")
)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Accessor methods.
###

setGeneric("elementMetadata",
           function(x, ...) standardGeneric("elementMetadata"))
setMethod("elementMetadata", "AnnotatedTypedListV2",
          function(x) {
              emd <- x@elementMetadata
              if (!is.null(emd) && !is.null(names(x)))
                  rownames(emd) <- names(x)
              emd
          })

setGeneric("elementMetadata<-",
           function(x, ..., value) standardGeneric("elementMetadata<-"))
setReplaceMethod("elementMetadata", c("AnnotatedTypedListV2", "XDataFrameORNULL"),
                 function(x, value) {
                     if (!is.null(value) && length(x) != nrow(value))
                         stop("the number of rows in elementMetadata 'value' ",
                                 "(if non-NULL) must match the length of 'x'")
                     if (!is.null(value))
                         rownames(value) <- NULL
                     x@elementMetadata <- value
                     x
                 })

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Validity.
###

.valid.AnnotatedTypedListV2.elementMetadata <- function(x) {
    emd <- elementMetadata(x)
    if (!is.null(emd) && nrow(emd) != length(x))
        "number of rows in non-NULL 'elementMetadata(x)' must match length of 'x'"
    else if (!is.null(emd) && !identical(rownames(emd), names(x)))
        "the rownames of non-NULL 'elementMetadata(x)' must match the names of 'x'"
    else NULL
}
.valid.AnnotatedTypedListV2 <- function(x)
{
    c(.valid.AnnotatedTypedListV2.elementMetadata(x))
}
setValidity2("AnnotatedTypedListV2", .valid.AnnotatedTypedListV2)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Subsetting.
###

setMethod("[", "AnnotatedCompressedTypedList",
          function(x, i, j, ..., drop)
          {
              ans <- callNextMethod()
              if (!is.null(elementMetadata(x)))
                  elementMetadata(ans) <- elementMetadata(x)[i,,drop=FALSE]
              ans
          })

setMethod("[", "AnnotatedSimpleTypedList",
          function(x, i, j, ..., drop)
          {
              ans <- callNextMethod()
              if (!is.null(elementMetadata(x)))
                  elementMetadata(ans) <- elementMetadata(x)[i,,drop=FALSE]
              ans
          })
  
### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Combining and splitting.
###

setMethod("append", c("AnnotatedCompressedTypedList", "AnnotatedCompressedTypedList"),
          function(x, values, after=length(x)) {
              ans <- callNextMethod()
              if (!is.null(elementMetadata(x)))
                  elementMetadata(ans) <-
                    rbind(elementMetadata(x), elementMetadata(values))
              ans
          })
  
setMethod("append", c("AnnotatedSimpleTypedList", "AnnotatedSimpleTypedList"),
          function(x, values, after=length(x)) {
              ans <- callNextMethod()
              if (!is.null(elementMetadata(x)))
                  elementMetadata(ans) <-
                          rbind(elementMetadata(x), elementMetadata(values))
              ans
          })

setMethod("c", "AnnotatedCompressedTypedList",
          function(x, ..., recursive = FALSE) {
              ans <- callNextMethod(x, ...)
              if (!is.null(elementMetadata(x)))
                  elementMetadata(ans) <-
                    do.call(rbind, lapply(c(list(x), ...), elementMetadata))
              ans
          })

setMethod("c", "AnnotatedSimpleTypedList",
          function(x, ..., recursive = FALSE) {
              ans <- callNextMethod(x, ...)
              if (!is.null(elementMetadata(x)))
                  elementMetadata(ans) <-
                          do.call(rbind, lapply(c(list(x), ...), elementMetadata))
              ans
          })
