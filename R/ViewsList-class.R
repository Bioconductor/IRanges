### =========================================================================
### ViewsList objects
### -------------------------------------------------------------------------


setClass("ViewsList",
    contains="IntegerRangesList",
    representation("VIRTUAL"),
    prototype(elementType="Views")
)

setClass("SimpleViewsList",
    contains=c("ViewsList", "SimpleList"),
    representation("VIRTUAL")
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


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion
###

setMethod("as.matrix", "ViewsList",
          function(x, rev = FALSE, use.names = FALSE)
          {
            if (!isTRUEorFALSE(use.names))
              stop("use.names must be TRUE or FALSE")
            if (!is(rev, "List"))
              rev <- as(rev, "List")
            rev <- S4Vectors:::VH_recycle(rev, x, "rev", "x")
            max_width <- max(max(width(restrict(ranges(x), start = 1L))))
            m <- do.call(rbind, mapply(as.matrix, x, rev,
                                       IntegerList(max_width),
                                       SIMPLIFY = FALSE))
            nms <- names(x)
            if (!is.null(nms) && use.names) {
              nms <- rep(nms, elementNROWS(x))
              rownms <- rownames(m)
              if (is.null(rownms))
                rownms <- unlist_as_integer(IRanges(1L, width=elementNROWS(x)))
              rownames(m) <- paste(nms, rownms, sep = ".")
            }
            m
          })
