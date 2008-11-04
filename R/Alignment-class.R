setClass("AlignmentSpace",
         representation(ranges = "IRanges", # start in A, width
                        offset = "integer", # offset to start in B
                        score = "integer", # rl-encoded scores
                        space = "character", # rl-encoded spaces
                        length = "integer")) # lengths for rle scores/spaces

setClass("Alignment",
         prototype = prototype(elementClass = "AlignmentSpace"),
         contains = "TypedList")

read.chain <- function(path, exclude = "_") {
  .Call("readChain", path, exclude, PACKAGE="IRanges")
}
