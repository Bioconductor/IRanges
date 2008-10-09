### =========================================================================
### solveUserSEW()
### -------------------------------------------------------------------------
###
### Some of the functions that use the start/end/width interface: narrow(),
### subseq(), Views(), Biostrings:::BStringSet() (and family),
### BSgenome:::getSeq(), etc...
###

solveUserSEW <- function(refwidths, start=NA, end=NA, width=NA,
                         if.non.positive.start.or.end="translate")
{
    #ranges <- new2("IRanges", start=rep.int(1L, length(refwidths)),
    #                          width=refwidths, check=FALSE)
    #narrow(ranges, start=start, end=end, width=width, use.names=FALSE)
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
    if (!isSingleString(if.non.positive.start.or.end))
        stop("'if.non.positive.start.or.end' must be a single string")
    if.non.positive.start.or.end <- match.arg(if.non.positive.start.or.end,
                                              c("translate", "keep", "error"))
    .Call("solve_user_SEW",
          refwidths, start, end, width, if.non.positive.start.or.end,
          PACKAGE="IRanges")
}

