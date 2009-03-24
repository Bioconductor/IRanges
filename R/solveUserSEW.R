### =========================================================================
### The SEW (Start/End/Width) interface
### -------------------------------------------------------------------------
###
### Some of the functions that support the SEW interface: narrow(), subseq(),
### Views(), Biostrings:::BStringSet() (and family), BSgenome:::getSeq(), etc...
###

solveUserSEW <- function(refwidths, start=NA, end=NA, width=NA,
                         translate.negative.coord=TRUE,
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
    min123 <- min(l1, l2, l3)
    max123 <- max(l1, l2, l3)
    if (length(refwidths) == 0) {
        if (max123 != min123 || max123 > 1)
            stop("'start', 'end' and 'width' must have the same length ",
                 "and it must be 0 or 1 when 'refwidths' is empty")
    } else {
        if (max123 > length(refwidths))
            stop("'start', 'end' or 'width' has more elements than 'refwidths'")
        if (min123 == 0)
            stop("'start', 'end' or 'width' is empty but 'refwidths' is not")
    }
    if (!isTRUEorFALSE(translate.negative.coord))
        stop("'translate.negative.coord' must be TRUE or FALSE")
    if (!isTRUEorFALSE(allow.nonnarrowing))
        stop("'allow.nonnarrowing' must be TRUE or FALSE")
    .Call("solve_user_SEW",
          refwidths, start, end, width,
          translate.negative.coord, allow.nonnarrowing,
          PACKAGE="IRanges")
}

