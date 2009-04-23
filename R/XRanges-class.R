### =========================================================================
### XRanges objects
### -------------------------------------------------------------------------

### Herve - 4/22/09: Do we really need this class?

### Abstract top-level class for external representations of ranges.
### Examples: databases, search trees, etc
### All derivatives should support coercion to a native IRanges instance.
###
### Herve - 4/22/09: Or we could go the other way around: any *Ranges*
### subclass (not only XRanges subclasses) should support 2 of the
### "start"/"end"/"width" methods plus the "names" method. That way
### coercion to IRanges will work out-of-the-box (thanks to the
### Ranges->IRanges coercion method). Then, no need to define the inefficient
### accessor methods below.
###

setClass("XRanges", contains=c("Ranges", "VIRTUAL"))

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Accessor methods.
###

setMethod("start", "XRanges", function(x) start(as(x, "IRanges")))
setMethod("end", "XRanges", function(x) end(as(x, "IRanges")))
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

