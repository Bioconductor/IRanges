### =========================================================================
### AnnotatedList objects
### -------------------------------------------------------------------------

## NOTE: this will throw warnings, because we use XDataFrame, which
## must be defined after this, since it is itself an AnnotatedList. As
## long as NULL is allowed, infinite recursion is avoided.
setClass("AnnotatedList",
         representation(annotation = "list",
                        elementMetadata = "XDataFrameORNULL"),
         contains = "TypedList")

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Accessor methods.
###

setGeneric("metadata", function(x, ...) standardGeneric("metadata"))
setMethod("metadata", "AnnotatedList", function(x) {
  if (is.null(x@annotation) || is.character(x@annotation))
    list(annotation = x@annotation)
  else x@annotation
})

setGeneric("metadata<-",
           function(x, ..., value) standardGeneric("metadata<-"))
setReplaceMethod("metadata", c("AnnotatedList", "list"),
          function(x, value) {
            if (!length(value))
              names(value) <- NULL # instead of character()
            x@annotation <- value
            x
          })

setGeneric("elementMetadata",
           function(x, ...) standardGeneric("elementMetadata"))
setMethod("elementMetadata", "AnnotatedList", function(x) {
  emd <- x@elementMetadata
  if (!is.null(emd) && !is.null(names(x)))
    rownames(emd) <- names(x)
  emd
})

setGeneric("elementMetadata<-",
           function(x, ..., value) standardGeneric("elementMetadata<-"))
setReplaceMethod("elementMetadata", c("AnnotatedList", "XDataFrameORNULL"),
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

.valid.AnnotatedList.elementMetadata <- function(x) {
  emd <- elementMetadata(x)
  if (!is.null(emd) && nrow(emd) != length(x))
    "number of rows in non-NULL 'elementMetadata(x)' must match length of 'x'"
  else if (!is.null(emd) && !identical(rownames(emd), names(x)))
    "the rownames of non-NULL 'elementMetadata(x)' must match the names of 'x'"
  else NULL
}

.valid.AnnotatedList <- function(x) {
  .valid.AnnotatedList.elementMetadata(x)
}

setValidity2("AnnotatedList", .valid.AnnotatedList)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Constructor.
###

AnnotatedList <- function(elements = list(), metadata = list(),
                          elementMetadata = NULL)
{
  if (!is.list(metadata))
    stop("'metadata' must be a list")
  if (!is.null(elementMetadata) && length(elements) != nrow(elementMetadata))
    stop("the number of rows in 'elementMetadata' ",
         "(if non-NULL) must match the number of list 'elements'")
  new("AnnotatedList", elements = elements, annotation = metadata,
      elementMetadata = elementMetadata)
}
