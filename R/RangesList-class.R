### =========================================================================
### RangesList objects
### -------------------------------------------------------------------------

## Accepts any type of Ranges object as an element

setClass("RangesList", representation("VIRTUAL"),
         prototype = prototype(elementType = "Ranges"),
         contains = "Sequence")
setClass("SimpleRangesList",
         prototype = prototype(elementType = "Ranges"),
         contains = c("SimpleList", "RangesList"))

setClass("IRangesList", representation("VIRTUAL"),
         prototype = prototype(elementType = "IRanges"),
         contains = "RangesList")
setClass("CompressedIRangesList",
         prototype = prototype(elementType = "IRanges",
                               unlistData = new("IRanges")),
         contains = c("IRangesList", "CompressedList"))
setClass("SimpleIRangesList",
         prototype = prototype(elementType = "IRanges"),
         contains = c("IRangesList", "SimpleRangesList"))

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Accessor methods.
###

setMethod("start", "RangesList",
          function(x) {
            if (length(x) == 0)
              integer(0)
            else
              unlist(lapply(x, start), use.names = FALSE)
          })
setMethod("end", "RangesList",
          function(x) {
            if (length(x) == 0)
              integer(0)
            else
              unlist(lapply(x, end), use.names = FALSE)
          })
setMethod("width", "RangesList",
          function(x) {
            if (length(x) == 0)
              integer(0)
            else
              unlist(lapply(x, width), use.names = FALSE)
          })

setMethod("start", "CompressedIRangesList",
          function(x) {
            if (length(x) == 0)
              integer(0)
            else
              start(unlist(x))
          })
setMethod("end", "CompressedIRangesList",
          function(x) {
            if (length(x) == 0)
              integer(0)
            else
              end(unlist(x))
          })
setMethod("width", "CompressedIRangesList",
          function(x) {
            if (length(x) == 0)
              integer(0)
            else
              width(unlist(x))
          })
setGeneric("space", function(x, ...) standardGeneric("space"))
setMethod("space", "RangesList",
          function(x) {
            space <- names(x)
            if (!is.null(space))
              space <- rep(space, elementLengths(x))
            space
          })

setGeneric("universe", function(x) standardGeneric("universe"))
setMethod("universe", "RangesList", function(x) {
  ### FIXME: for compatibility with older versions, eventually emit warning
  if (is.null(metadata(x)) || is.character(metadata(x)))
      metadata(x)
  else
      metadata(x)$universe
})

setGeneric("universe<-", function(x, value) standardGeneric("universe<-"))
setReplaceMethod("universe", "RangesList",
                 function(x, value) {
                   if (!is.null(value) && !isSingleString(value))
                     stop("'value' must be a single string or NULL")
                   metadata(x)$universe <- value
                   x
                 })

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Constructor.
###

RangesList <- function(..., universe = NULL)
{
  if (!is.null(universe) && !isSingleString(universe))
    stop("'universe' must be a single string or NULL")
  ranges <- list(...)
  if (!all(sapply(ranges, is, "Ranges")))
    stop("all elements in '...' must be Ranges objects")
  ans <- newSimpleList("SimpleRangesList", ranges)
  universe(ans) <- universe
  ans
}

IRangesList <- function(..., universe = NULL, compress = TRUE)
{
  if (!is.null(universe) && !isSingleString(universe))
    stop("'universe' must be a single string or NULL")
  ranges <- list(...)
  if (!all(sapply(ranges, is, "IRanges")))
    stop("all elements in '...' must be IRanges objects")
  if (compress)
    ans <- newCompressedList("CompressedIRangesList", ranges)
  else
    ans <- newSimpleList("SimpleIRangesList", ranges)
  universe(ans) <- universe
  ans
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Subsetting.
###

### Support subsetting by another RangesList
rangesListSingleSquareBracket <- function(x, i, j, ..., drop)
{
  if (!missing(j) || length(list(...)) > 0)
    stop("invalid subsetting")
  if (missing(i))
    return(x)
  if (is(i, "RangesList")) {
    ol <- overlap(i, x, multiple = FALSE)
    els <- as.list(x)
    for (j in seq_len(length(x))) {
      els[[j]] <- els[[j]][!is.na(ol[[j]])]
    }
    if (is(x, "CompressedList"))
      ans <- newCompressedList(class(x), els)
    else
      ans <- newSimpleList(class(x), els)
  } else {
    ans <- callNextMethod(x, i)
  }
  ans
}
setMethod("[", "SimpleRangesList", rangesListSingleSquareBracket)
setMethod("[", "CompressedIRangesList", rangesListSingleSquareBracket)
setMethod("[", "SimpleIRangesList", rangesListSingleSquareBracket)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Some useful endomorphisms: "reduce" and "gaps".
###

### 'with.inframe.attrib' is ignored.
setMethod("reduce", "RangesList",
          function(x, with.inframe.attrib=FALSE)
          {
            if (length(x) == 0) {
              nirl <- list(new("NormalIRanges"))
            } else {
              ranges <- new2("IRanges", start=start(x), width=width(x), check=FALSE)
              nirl <- list(asNormalIRanges(ranges, force=TRUE))
            }
            ## This transformation must be atomic.
            if (is(x, "CompressedList"))
              newCompressedList(class(x), nirl)
            else
              newSimpleList(class(x), nirl)
        })

setMethod("gaps", "RangesList",
          function(x, start=NA, end=NA)
          {
            if (is(x, "CompressedList"))
              newCompressedList(class(x),
                                lapply(x, gaps, start = start, end = end))
            else
              newSimpleList(class(x), lapply(x, gaps, start = start, end = end))
          })

setMethod("range", "RangesList",
          function(x, ..., na.rm) {
            args <- list(x, ...)
            if (!all(sapply(sapply(args, universe), identical, universe(x))))
              stop("All args in '...' must have the same universe as 'x'")
            spaceList <- lapply(args, names)
            names <- spaces <- unique(do.call(c, spaceList))
            if (any(sapply(spaceList, is.null))) {
              if (!all(unlist(lapply(args, length)) == length(x)))
                stop("If any args are missing names, all must have same length")
              spaces <- seq_len(length(x))
            }
            ranges <- lapply(spaces, function(space) {
              r <- lapply(args, `[[`, space)
              do.call(range, r[!sapply(r, is.null)])
            })
            names(ranges) <- names
            if (is(x, "CompressedList"))
              newCompressedList(class(x), ranges)
            else
              newSimpleList(class(x), ranges)
        })

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Overlap.
###

setMethod("overlap", c("RangesList", "RangesList"),
          function(object, query, maxgap = 0, multiple = TRUE, drop = FALSE)
          {
            query <- as.list(query)
            subject <- as.list(object)
            origSubject <- subject
            if (!is.null(names(subject)) && !is.null(names(query))) {
              subject <- subject[names(query)]
              names(subject) <- names(query) # get rid of NA's in names
            }
            else subject <- subject[seq_along(query)]
            ## NULL's are introduced where they do not match
            ## We replace those with empty IRanges
            subject[sapply(subject, is.null)] <- IRanges()
            ans <- lapply(seq_len(length(subject)), function(i) {
              overlap(subject[[i]], query[[i]], maxgap, multiple)
            })
            names(ans) <- names(subject)
            if (multiple)
              ans <- RangesMatchingList(ans, names(origSubject))
            else if (drop) {
              off <- head(c(0, cumsum(sapply(origSubject, length))), -1)
              names(off) <- names(origSubject)
              if (is.null(names(ans)))
                off <- off[seq_along(ans)]
              else off <- off[names(ans)]
              ans <- unlist(ans, use.names=FALSE) +
                rep(unname(off), sapply(ans, length))
            }
            ans
          })

setMethod("%in%", c("RangesList", "RangesList"),
          function(x, table)
          !is.na(unlist(overlap(table, x, multiple = FALSE), use.names=FALSE)))

setMethod("match", c("RangesList", "RangesList"),
          function(x, table, nomatch = NA_integer_, incomparables = NULL)
          {
            if (length(nomatch) != 1)
              stop("'nomatch' must be of length 1") 
            ans <- overlap(table, x, multiple=FALSE, drop=TRUE)
            if (!is.na(nomatch))
              ans[is.na(ans)] <- as.integer(nomatch)
            ans
          })

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Arithmetic Operations
###

setMethod("Ops", c("RangesList", "ANY"),
          function(e1, e2) {
            for (i in seq_len(length(e1)))
              e1[[i]] <- callGeneric(e1[[i]], e2)
            e1
          })

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "show" method.
###

setMethod("show", "RangesList",
          function(object)
          {
            nranges <- length(start(object))
            cat(class(object), ": ", nranges, " range", sep = "")
            if (nranges != 1)
              cat("s")
            cat("\n")
            if (length(object)) {
              if (is.null(names(object)))
                nms <- seq_len(length(object))
              else nms <- paste("\"", names(object), "\"", sep = "")
              if (length(object) > 1) {
                cat(labeledLine("sequences", nms))
              } else {
                ranges <- object[[1]]
                if (length(ranges)) {
                  str <- paste(start(ranges), ":", end(ranges), sep = "")
                  cat(labeledLine(nms, str, count = FALSE))
                }
              }
            }
          })

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion.
###

setMethod("as.data.frame", "RangesList",
          function(x, row.names=NULL, optional=FALSE, ...)
          {
            if (!(is.null(row.names) || is.character(row.names)))
              stop("'row.names'  must be NULL or a character vector")
            if (!missing(optional) || length(list(...)))
              warning("'optional' and arguments in '...' ignored")
            xi <- as(x, "CompressedIRangesList")
            names(xi) <- NULL
            df <- as.data.frame(unlist(xi), row.names = row.names)
            if (length(names(x)) > 0)
              df <- cbind(space = rep(names(x), elementLengths(x)), df)
            df
          })

### From an IRangesList object to a NormalIRanges object.
setAs("IRangesList", "NormalIRanges",
      function(from) reduce(from)[[1]])

### FIXME: Not clear why this is needed
setAs("IRangesList", "list",
      function(from) as(as(from, "RangesList"), "list"))

setAs("RangesList", "CompressedIRangesList",
      function(from) {
        newCompressedList("CompressedIRangesList", lapply(from, as, "IRanges"),
                          metadata = metadata(from),
                          elementMetadata = elementMetadata(from))
      })

setAs("RangesList", "SimpleIRangesList",
      function(from) {
        newSimpleList("SimpleIRangesList", lapply(from, as, "IRanges"),
                      metadata = metadata(from),
                      elementMetadata = elementMetadata(from))
      })

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "summary" method.
###

setMethod("summary", "CompressedIRangesList",
         function(object)
         {
             .Call("CompressedIRangesList_summary", object, PACKAGE="IRanges")
         })


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Splitting.
###

setMethod("split", "Ranges",
          function(x, f, drop = FALSE, ...)
          {
            do.call(RangesList, callNextMethod())
          })

setMethod("split", "IRanges",
          function(x, f, drop = FALSE, ...)
          {
            newCompressedList("CompressedIRangesList", x,
                              splitFactor = f, drop = drop)
          })
