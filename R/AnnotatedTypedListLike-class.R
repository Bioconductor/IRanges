### =========================================================================
### AnnotatedTypedListLike objects
### -------------------------------------------------------------------------

setClass("AnnotatedTypedListLike",
        contains=c("Annotated", "TypedListLike"),
        representation(
                       "VIRTUAL",
                       elementMetadata = "DataFrameORNULL"
                       ),
        prototype(elementType="ANYTHING")
)

setClass("AnnotatedCompressedTypedListLike",
        contains=c("AnnotatedTypedListLike", "CompressedTypedListLike"),
        representation("VIRTUAL")
)

setClass("AnnotatedSimpleTypedListLike",
        contains=c("AnnotatedTypedListLike", "SimpleTypedListLike"),
        representation("VIRTUAL")
)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Accessor methods.
###

setGeneric("elementMetadata",
           function(x, ...) standardGeneric("elementMetadata"))
setMethod("elementMetadata", "AnnotatedTypedListLike",
          function(x) {
              emd <- x@elementMetadata
              if (!is.null(emd) && !is.null(names(x)))
                  rownames(emd) <- names(x)
              emd
          })

setGeneric("elementMetadata<-",
           function(x, ..., value) standardGeneric("elementMetadata<-"))
setReplaceMethod("elementMetadata", c("AnnotatedTypedListLike", "DataFrameORNULL"),
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

.valid.AnnotatedTypedListLike.elementMetadata <- function(x) {
    emd <- elementMetadata(x)
    if (!is.null(emd) && nrow(emd) != length(x))
        "number of rows in non-NULL 'elementMetadata(x)' must match length of 'x'"
    else if (!is.null(emd) && !identical(rownames(emd), names(x)))
        "the rownames of non-NULL 'elementMetadata(x)' must match the names of 'x'"
    else NULL
}
.valid.AnnotatedTypedListLike <- function(x)
{
    c(.valid.AnnotatedTypedListLike.elementMetadata(x))
}
setValidity2("AnnotatedTypedListLike", .valid.AnnotatedTypedListLike)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Subsetting.
###

setMethod("[", "AnnotatedCompressedTypedListLike",
          function(x, i, j, ..., drop)
          {
              ans <- callNextMethod()
              if (!is.null(elementMetadata(x)))
                  elementMetadata(ans) <- elementMetadata(x)[i,,drop=FALSE]
              ans
          })

setMethod("[", "AnnotatedSimpleTypedListLike",
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

setMethod("append",
          c("AnnotatedCompressedTypedListLike", "AnnotatedCompressedTypedListLike"),
          function(x, values, after=length(x)) {
              ans <- callNextMethod()
              if (!is.null(elementMetadata(x)))
                  elementMetadata(ans) <-
                    rbind(elementMetadata(x), elementMetadata(values))
              ans
          })
  
setMethod("append",
          c("AnnotatedSimpleTypedListLike", "AnnotatedSimpleTypedListLike"),
          function(x, values, after=length(x)) {
              ans <- callNextMethod()
              if (!is.null(elementMetadata(x)))
                  elementMetadata(ans) <-
                          rbind(elementMetadata(x), elementMetadata(values))
              ans
          })

setMethod("c", "AnnotatedCompressedTypedListLike",
          function(x, ..., recursive = FALSE) {
              ans <- callNextMethod(x, ...)
              if (!is.null(elementMetadata(x)))
                  elementMetadata(ans) <-
                    do.call(rbind, lapply(c(list(x), ...), elementMetadata))
              ans
          })

setMethod("c", "AnnotatedSimpleTypedListLike",
          function(x, ..., recursive = FALSE) {
              ans <- callNextMethod(x, ...)
              if (!is.null(elementMetadata(x)))
                  elementMetadata(ans) <-
                          do.call(rbind, lapply(c(list(x), ...), elementMetadata))
              ans
          })
