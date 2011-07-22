### Safe alternative to 'strsplit(x, NULL, fixed=TRUE)[[1L]]'.
safeExplode <- function(x)
{
    if (!isSingleString(x))
        stop("'x' must be a single string")
    .Call2("safe_strexplode", x, PACKAGE="IRanges")
}

### strsplitAsListOfIntegerVectors(x) is an alternative to:
###   lapply(strsplit(x, ",", fixed=TRUE), as.integer)
### except that:
###  - strsplit() accepts NAs, we don't (raise an error);
###  - as.integer() introduces NAs by coercion (with a warning), we don't
###    (raise an error);
###  - as.integer() supports "inaccurate integer conversion in coercion"
###    when the value to coerce is > INT_MAX (then it's coerced to INT_MAX),
###    we don't (raise an error);
###  - as.integer() will coerce non-integer values (e.g. 10.3) to an int
###    by truncating them, we don't (raise an error).
### When it fails, strsplit_as_list_of_ints() will print a detailed parse
### error message.
### It's also faster and uses much less memory. E.g. it's 8x faster and uses
### < 1 Mb versus > 60 Mb on the character vector 'biginput' created with:
###   library(rtracklayer)
###   session <- browserSession()
###   genome(session) <- "hg18"
###   query <- ucscTableQuery(session, "UCSC Genes")
###   tx <- getTable(query)
###   biginput <- c(tx$exonStarts, tx$exonEnds)  # 133606 elements
strsplitAsListOfIntegerVectors <- function(x, sep=",")
{
    if (!is.character(x))
        stop("'x' must be a character vector")
    if (!isSingleString(sep) || nchar(sep) != 1L)
        stop("'sep' must be a string containing just one single-byte character")
    ans <- .Call2("strsplit_as_list_of_ints", x, sep, PACKAGE="IRanges")
    names(ans) <- names(x)
    ans
}

### svn.time() returns the time in Subversion format, e.g.:
###   "2007-12-07 10:03:15 -0800 (Fri, 07 Dec 2007)"
### The -0800 part will be adjusted if daylight saving time is in effect.
### TODO: Find a better home for this function.
svn.time <- function() .Call2("svn_time", PACKAGE="IRanges")

