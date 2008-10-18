### =========================================================================
### XRanges objects
### -------------------------------------------------------------------------

## Abstract top-level class for external representations of ranges.
## Examples: databases, search trees, etc
## All derivatives should support coercion to a native IRanges instance.

setClass("XRanges", contains = "Ranges")

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Accessor methods.
###

setMethod("start", "XRanges", function(x) start(as(x, "IRanges")))
setMethod("width", "XRanges", function(x) width(as(x, "IRanges")))

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Utility wrappers.
###

setMethod("gaps", "XRanges",
          function(x, start=NA, end=NA) {
            ir <- as(x, "IRanges")
            g <- gaps(ir, start, end)
            as(g, class(x))
          })

setMethod("reduce", "XRanges",
          function(x, with.inframe.attrib=FALSE) {
            ir <- as(x, "IRanges")
            g <- reduce(ir, with.inframe.attrib)
            as(g, class(x))
          })
