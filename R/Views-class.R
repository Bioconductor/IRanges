### =========================================================================
### Views objects
### -------------------------------------------------------------------------
###

### A Views object is an UnlockedIRanges object with no null widths.
setClass("Views", contains="UnlockedIRanges")

### Views objects.
.valid.Views.width <- function(object)
{
    if (length(object) != 0 && any(width(object) == 0))
        return("null widths are not allowed")
    NULL
}
setValidity("Views",
    function(object)
    {
        problems <- .valid.Views.width(object)
        if (is.null(problems)) TRUE else problems
    }
)

setMethod("initialize", "Views",
    function(.Object, start=integer(0), width=integer(0),
                      names=NULL, check=TRUE)
    {
        .Object <- callNextMethod(.Object, start=start, width=width,
                                           names=names, check=check)
        if (check)
            stopIfProblems(.valid.Views.width(.Object))
        .Object
    }
)

setAs("Views", "NormalIRanges",
    function(from) asNormalIRanges(from, check=TRUE)
)

setReplaceMethod("width", "Views",
    function(x, check=TRUE, value)
    {
        x <- callNextMethod()
        if (check)
            stopIfProblems(.valid.Views.width(x))
        x
    }
)


### Views generics.
setGeneric("subject", function(x) standardGeneric("subject"))
