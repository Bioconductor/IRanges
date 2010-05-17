### Safe alternative to 'strsplit(x, NULL, fixed=TRUE)[[1L]]'.
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
    ans <- .Call("strsplit_as_list_of_ints", x, sep, PACKAGE="IRanges")
    names(ans) <- names(x)
    ans
}

### svn.time() returns the time in Subversion format, e.g.:
###   "2007-12-07 10:03:15 -0800 (Fri, 07 Dec 2007)"
### The -0800 part will be adjusted if daylight saving time is in effect.
### TODO: Find a better home for this function.
svn.time <- function()
{
    format(Sys.time(), "%Y-%m-%d %X %z (%a, %d %b %Y)") 
}
