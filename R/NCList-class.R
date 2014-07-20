### =========================================================================
### NCList objects
### -------------------------------------------------------------------------
###
### An S4 implementation of Nested Containment List (NCList).
###

setClass("NCList",
    contains="Ranges",
    representation(
        nclist="list",
        ranges="IRanges"
    )
)

setMethod("length", "NCList", function(x) length(x@ranges))

setMethod("start", "NCList", function(x, ...) start(x@ranges))
setMethod("end", "NCList", function(x, ...) end(x@ranges))
setMethod("width", "NCList", function(x) width(x@ranges))
setMethod("names", "NCList", function(x) names(x@ranges))

NCList <- function(x)
{
    if (!is(x, "Ranges"))
        stop("'x' must be a Ranges object")
    if (!is(x, "IRanges"))
        x <- as(x, "IRanges")
    ans_nclist <- .Call("NCList_build", start(x), end(x), PACKAGE="IRanges")
    new2("NCList", nclist=ans_nclist, ranges=x, check=FALSE)
}

## NOT exported.
findOverlaps_NCList <- function(query, subject)
{
    if (!is(query, "Ranges"))
        stop("'query' must be a Ranges object")
    if (!is(subject, "NCList"))
        stop("'subject' must be an NCList object")
    .Call("NCList_find_overlaps", start(query), end(query),
                                  subject@nclist,
                                  start(subject@ranges), end(subject@ranges),
                                  PACKAGE="IRanges")
}

