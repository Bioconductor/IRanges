### Safe alternative to 'strsplit(x, NULL, fixed=TRUE)[[1]]'.
safeExplode <- function(x)
{
    if (!isSingleString(x))
        stop("'x' must be a single string")
    .Call("safe_strexplode", x, PACKAGE="IRanges")
}


strsplitAsListOfIntegerVectors <- function(x, split)
{
    if (!is.character(x))
        stop("'x' must be a character vector")
    if (!isSingleString(split))
        stop("'split' must be a single string")
    .Call("strsplit_asIntList", x, split, PACKAGE="IRanges")
}

