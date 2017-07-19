### =========================================================================
### IMPORTANT NOTE - 7/2/2014
### Most of the stuff that used to be in the IRanges/R/Rle-class.R file was
### moved to the S4Vectors package (to R/Rle-class.R and R/Rle-utils.R).
### The stuff that could not be moved there was *temporarily* kept here in
### Rle-class-leftovers.R but will need to find a new home (in S4Vectors
### or in IRanges).
###


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Accessor methods.
###

setMethod("ranges", "Rle",
    function(x, use.names=TRUE, use.mcols=FALSE)
        IRanges(start(x), width=width(x))
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion
###

setAs("Rle", "IRanges",
      function(from)
      {
          if (!is.logical(runValue(from)) || S4Vectors:::anyMissing(runValue(from)))
              stop("cannot coerce a non-logical 'Rle' or a logical 'Rle' ",
                   "with NAs to an IRanges object")
          keep <- runValue(from)
          ## The returned IRanges instance is guaranteed to be normal.
          ans_start <- start(from)[keep]
          ans_width <- runLength(from)[keep]
          new2("IRanges", start=ans_start, width=ans_width, check=FALSE)
      })

setAs("Rle", "NormalIRanges",
      function(from) newNormalIRangesFromIRanges(as(from, "IRanges"), check=FALSE))


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### General methods
###

setGeneric("findRange", signature = "vec",
           function(x, vec) standardGeneric("findRange"))

setMethod("findRange", signature = c(vec = "Rle"),
          function(x, vec) {
              run <- findRun(x, vec)
              if (S4Vectors:::anyMissing(run))
                stop("all 'x' values must be in [1, 'length(vec)']")
              IRanges(start = start(vec)[run], width = width(vec)[run],
                      names = names(x))
          })

setGeneric("orderAsRanges", signature = c("x"),  # not exported
           function(x, na.last = TRUE, decreasing = FALSE)
               standardGeneric("orderAsRanges"))

setMethod("orderAsRanges", "Rle",
           function(x, na.last = TRUE, decreasing = FALSE)
           {
               ord <- base::order(runValue(x), na.last = na.last,
                                  decreasing = decreasing)
               new2("IRanges", start = start(x)[ord], width = runLength(x)[ord],
                    check = FALSE)
           })

setGeneric("splitRanges", signature = "x",
           function(x) standardGeneric("splitRanges"))

setMethod("splitRanges", "Rle",
          function(x) {
              split(IRanges(start = start(x), width = runLength(x)),
                    runValue(x))
          })

setMethod("splitRanges", "vector_OR_factor",
          function(x) {
              callGeneric(Rle(x))
          })

