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
    function(x, use.names=TRUE, use.mcols=FALSE)
        S4Vectors:::new_SimpleList_from_list("SimpleIRangesList",
            lapply(x, ranges, use.names=use.names, use.mcols=use.mcols))
)

setMethod("start", "SimpleViewsList", function(x, ...) start(ranges(x)))
setMethod("end", "SimpleViewsList", function(x, ...) end(ranges(x)))
setMethod("width", "SimpleViewsList", function(x) width(ranges(x)))

### TODO: Why not define this at the List level? Or even at the Vector level?
setMethod("universe", "ViewsList",
          function(x)
          {
            .Deprecated(msg="The universe() getter is deprecated.")
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
                   .Deprecated(msg="The universe() setter is deprecated.")
                   if (!is.null(value) && !isSingleString(value))
                     stop("'value' must be a single string or NULL")
                   metadata(x)$universe <- value
                   x
                 })

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion
###

setMethod("as.matrix", "ViewsList",
          function(x, rev = FALSE, use.names = FALSE)
          {
            if (!isTRUEorFALSE(use.names))
              stop("use.names must be TRUE or FALSE")
            rev <- normargAtomicList1(rev, LogicalList, length(x))
            max_width <- max(max(width(restrict(ranges(x), start = 1L))))
            m <- do.call(rbind, mapply(as.matrix, x, rev,
                                       IntegerList(max_width),
                                       SIMPLIFY = FALSE))
            nms <- names(x)
            if (!is.null(nms) && use.names) {
              nms <- rep(nms, elementNROWS(x))
              rownms <- rownames(m)
              if (is.null(rownms))
                rownms <- as.integer(IRanges(1L, width = elementNROWS(x)))
              rownames(m) <- paste(nms, rownms, sep = ".")
            }
            m
          })
