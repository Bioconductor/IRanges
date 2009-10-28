### Safe alternative to 'strsplit(x, NULL, fixed=TRUE)[[1]]'.
safeExplode <- function(x)
{
    if (!isSingleString(x))
        stop("'x' must be a single string")
    .Call("safe_strexplode", x, PACKAGE="IRanges")
}


strsplitAsListOfIntegerVectors <- function(x, sep=",")
{
    if (!is.character(x))
        stop("'x' must be a character vector")
    if (!isSingleString(sep) || nchar(sep) != 1L)
        stop("'sep' must be a string containing just one single-byte character")
    .Call("strsplit_asIntList", x, sep, PACKAGE="IRanges")
}

