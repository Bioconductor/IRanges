### =========================================================================
### ViewsList objects
### -------------------------------------------------------------------------

setClass("ViewsList",
    contains="List",
    representation("VIRTUAL"),
    prototype(elementType="Views")
)

setClass("SimpleViewsList",
    contains=c("ViewsList", "SimpleList"),
    representation("VIRTUAL"),
    prototype(elementType="Views")
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Accessor methods.
###

setMethod("ranges", "SimpleViewsList",
    function(x, ...)
        newSimpleList("SimpleIRangesList", lapply(x, ranges))
)

setMethod("start", "SimpleViewsList", function(x, ...) start(ranges(x)))
setMethod("end", "SimpleViewsList", function(x, ...) end(ranges(x)))
setMethod("width", "SimpleViewsList", function(x) width(ranges(x)))

### TODO: Why not define this at the List level? Or even at the Vector level?
setMethod("universe", "ViewsList",
          function(x)
          {
            ### FIXME: for compatibility with older versions, eventually emit warning
            if (is.null(metadata(x)) || is.character(metadata(x)))
              metadata(x)
            else
              metadata(x)$universe
          })

### TODO: Why not define this at the List level? Or even at the Vector level?
setReplaceMethod("universe", "ViewsList",
                 function(x, value)
                 {
                   if (!is.null(value) && !isSingleString(value))
                     stop("'value' must be a single string or NULL")
                   metadata(x)$universe <- value
                   x
                 })

