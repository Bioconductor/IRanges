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


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### findOverlaps() and family.
###

setMethod("findOverlaps", c("ViewsList", "ViewsList"),
    function(query, subject, maxgap=0L, minoverlap=1L,
             type=c("any", "start", "end", "within", "equal"),
             select=c("all", "first", "last", "arbitrary"),
             drop=FALSE)
    {
        findOverlaps(ranges(query), ranges(subject),
                     maxgap=maxgap, minoverlap=minoverlap,
                     type=type, select=select, drop=drop)
    }
)

setMethod("findOverlaps", c("ANY", "ViewsList"),
    function(query, subject, maxgap=0L, minoverlap=1L,
             type=c("any", "start", "end", "within", "equal"),
             select=c("all", "first", "last", "arbitrary"),
             drop=FALSE)
    {
        findOverlaps(query, ranges(subject),
                     maxgap=maxgap, minoverlap=minoverlap,
                     type=type, select=select, drop=drop)
    }
)

setMethod("findOverlaps", c("ViewsList", "ANY"),
    function(query, subject, maxgap=0L, minoverlap=1L,
             type=c("any", "start", "end", "within", "equal"),
             select=c("all", "first", "last", "arbitrary"),
             drop=FALSE)
    {
        findOverlaps(ranges(query), subject,
                     maxgap=maxgap, minoverlap=minoverlap,
                     type=type, select=select, drop=drop)
    }
)

setMethod("countOverlaps", c("ViewsList", "ViewsList"),
    function(query, subject, maxgap=0L, minoverlap=1L,
             type=c("any", "start", "end", "within", "equal"))
    {
         countOverlaps(ranges(query), ranges(subject),
                       maxgap=maxgap, minoverlap=minoverlap,
                       type=type)
    }
)

setMethod("countOverlaps", c("ANY", "ViewsList"),
    function(query, subject, maxgap=0L, minoverlap=1L,
             type=c("any", "start", "end", "within", "equal"))
    {
         countOverlaps(query, ranges(subject),
                       maxgap=maxgap, minoverlap=minoverlap,
                       type=type)
    }
)

setMethod("countOverlaps", c("ViewsList", "ANY"),
    function(query, subject, maxgap=0L, minoverlap=1L,
             type=c("any", "start", "end", "within", "equal"))
    {
         countOverlaps(ranges(query), subject,
                       maxgap=maxgap, minoverlap=minoverlap,
                       type=type)
    }
)

setMethod("match", c("ViewsList", "ViewsList"),
    function(x, table, nomatch=NA_integer_, incomparables=NULL)
    {
        match(ranges(x), ranges(table),
              nomatch=nomatch, incomparables=incomparables)
    }
)

setMethod("match", c("ANY", "ViewsList"),
    function(x, table, nomatch=NA_integer_, incomparables=NULL)
    {
        match(x, ranges(table),
              nomatch=nomatch, incomparables=incomparables)
    }
)

setMethod("match", c("ViewsList", "ANY"),
    function(x, table, nomatch=NA_integer_, incomparables=NULL)
    {
        match(ranges(x), table,
              nomatch=nomatch, incomparables=incomparables)
    }
)

setMethod("%in%", c("ViewsList", "ViewsList"),
    function(x, table) ranges(x) %in% ranges(table)
)

setMethod("%in%", c("ANY", "ViewsList"),
    function(x, table) x %in% ranges(table)
)

setMethod("%in%", c("ViewsList", "ANY"),
    function(x, table) ranges(x) %in% table
)

