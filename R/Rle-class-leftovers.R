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

setMethod("ranges", "Rle", function(x) IRanges(start(x), width = width(x)))


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

.window.Rle <- function(x, start=NA, end=NA, width=NA,
                          frequency=NULL, delta=NULL, ...)
{
    solved_SEW <- solveUserSEWForSingleSeq(length(x), start, end, width)
    if (is.null(frequency) && is.null(delta)) {
        info <- S4Vectors:::getStartEndRunAndOffset(x, start(solved_SEW),
                                                       end(solved_SEW))
        runStart <- info[["start"]][["run"]]
        offsetStart <- info[["start"]][["offset"]]
        runEnd <- info[["end"]][["run"]]
        offsetEnd <- info[["end"]][["offset"]]
        ans <- .Call2("Rle_window",
                      x, runStart, runEnd, offsetStart, offsetEnd,
                      new("Rle"), PACKAGE = "S4Vectors")
        if (is.factor(runValue(x)))
            attributes(runValue(ans)) <-
                list(levels = levels(x), class = "factor")
            ans
    } else {
        idx <- stats:::window.default(seq_len(length(x)),
                                      start = start(solved_SEW),
                                      end = end(solved_SEW),
                                      frequency = frequency,
                                      deltat = delta, ...)
        attributes(idx) <- NULL
        x[idx]
    }
}
setMethod("window", "Rle", .window.Rle)

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
               ord <- S4Vectors:::orderInteger(runValue(x), na.last = na.last,
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

setMethod("splitRanges", "vectorORfactor",
          function(x) {
              callGeneric(Rle(x))
          })

