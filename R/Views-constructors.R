### =========================================================================
### Constructor-like functions and generics for Views objects
### -------------------------------------------------------------------------

.safeMakeViews <- function(subject, start, end)
{
    if (!isNumericOrNAs(start) || !isNumericOrNAs(end))
        stop("'start' and 'end' must be numerics")
    if (!is.integer(start))
        start <- as.integer(start)
    start[is.na(start)] <- as.integer(1)
    if (!is.integer(end))
        end <- as.integer(end)
    end[is.na(end)] <- length(subject)
    if (length(start) < length(end))
        start <- recycleVector(start, length(end))
    else if (length(end) < length(start))
        end <- recycleVector(end, length(start))
    if (!all(start <= end))
        stop("'start' and 'end' must verify 'start <= end'")
    new("Views", start=start, width=end-start+1L, check=FALSE)
}

setGeneric("views", signature = "subject",
           function(subject, start=NA, end=NA) standardGeneric("views"))
setGeneric("trim", signature="x", function(x, use.names=TRUE) standardGeneric("trim"))
setGeneric("subviews", signature="x",
           function(x, start=NA, end=NA, width=NA, use.names=TRUE) standardGeneric("subviews"))
