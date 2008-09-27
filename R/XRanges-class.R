### =========================================================================
### XRanges objects
### -------------------------------------------------------------------------

## Abstract top-level class for external representations of ranges.
## Examples: databases, search trees, etc
## All derivatives should support coercion to a native Ranges instance.

setClass("XRanges",
         representation(rangeMode = "character"), ## storage mode of ranges
         contains = "VIRTUAL")
