### =========================================================================
### The SEW (Start/End/Width) interface
### -------------------------------------------------------------------------
###
### Some of the functions that use the SEW interface: narrow(), subseq(),
### Views(), Biostrings:::BStringSet() (and family), BSgenome:::getSeq(), etc...
###

solveUserSEW <- function(refwidths, start=NA, end=NA, width=NA,
                         translate.nonpositive.coord=TRUE,
                         allow.nonnarrowing=FALSE)
{
    if (!is.numeric(refwidths))
        stop("'refwidths' must be a vector of integers")
    if (!is.integer(refwidths))
        refwidths <- as.integer(refwidths)
    start <- normargIntegerOrNA(start, "start")
    end <- normargIntegerOrNA(end, "end")
    width <- normargIntegerOrNA(width, "width")
    ## From here, 'refwidths', 'start', 'end' and 'width' are guaranteed to be
    ## integer vectors. NAs in 'start', 'end' and 'width' are OK but not in
    ## 'refwidths' so this needs to be checked by C function solve_user_SEW().
    l1 <- length(start)
    l2 <- length(end)
    l3 <- length(width)
    if (max(l1, l2, l3) > length(refwidths))
        stop("'start', 'end' or 'width' has more elements than 'refwidths'")
    if (min(l1, l2, l3) == 0 && length(refwidths) != 0)
        stop("'start', 'end' or 'width' is empty but 'refwidths' is not")
    if (!isTRUEorFALSE(translate.nonpositive.coord))
        stop("'translate.nonpositive.coord' must be TRUE or FALSE")
    if (!isTRUEorFALSE(allow.nonnarrowing))
        stop("'allow.nonnarrowing' must be TRUE or FALSE")
    .Call("solve_user_SEW",
          refwidths, start, end, width,
          translate.nonpositive.coord, allow.nonnarrowing,
          PACKAGE="IRanges")
}

