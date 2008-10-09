### =========================================================================
### XRleInteger objects
### -------------------------------------------------------------------------
###
### The XRleInteger class is a container for storing a run length encodings
### for XInteger objects.
###

setClass("XRleInteger",
    contains="XRle",
    representation(
        values="XInteger"
    )
)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Initialization.
###

setMethod("initialize", "XRleInteger",
    function(.Object, values, lengths)
    {
        if (length(values) != length(lengths))
            stop("'values' and 'lengths' must have the same length")
        .Object@values <- values
        .Object@lengths <- lengths
        .Object@vectorLength <- sum(as.integer(lengths))
        .Object
    }
)

XRleInteger <- function(x) {
    if (is(x, "XInteger"))
        x <- as.integer(x)
    if (!is.integer(x))
        stop("'x' must be an integer vector")
    xRle <- rle(x)
    new("XRleInteger", 
        values = XInteger(length(xRle$values), val = xRle$values),
        lengths = XInteger(length(xRle$lengths), val = xRle$lengths))
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion
###

setAs("integer", "XRleInteger",
    function(from) XRleInteger(from))

setMethod("as.integer", "XRleInteger",
    function(x) rep.int(as.integer(x@values), as.integer(x@lengths)))

setMethod("as.vector", c("XRleInteger", "missing"),
    function(x, mode) as.integer(x))

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "show" method.
###

setMethod("show", "XRleInteger",
    function(object)
    {
        lo <- length(object)
        cat("  ", lo, "-integer \"", class(object), "\" instance\n", sep="")
        cat(" [1] ")
        cat(toNumSnippet(object, getOption("width") - 4))
        cat("\n")
        invisible(object)
    }
)
