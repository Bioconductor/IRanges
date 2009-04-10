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
        .Object@vectorLength <- sum(as.integer(lengths))
        .Object@lengths <- lengths
        .Object@values <- values
        .Object
    }
)

XRleInteger <- function(x) {
    .Deprecated("Rle")
    if (is(x, "XInteger"))
        x <- as.integer(x)
    if (!is.integer(x))
        stop("'x' must be an integer vector")
    xRle <- rle(x)
    new("XRleInteger", 
        values = as(xRle$values, "XInteger"),
        lengths = as(xRle$lengths, "XInteger"))
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Arithmetic operations.
###

setMethod("Arith", signature(e1 = "XRleInteger", e2 = "XRleInteger"),
    function (e1, e2)
    {
        if (.Generic == '/')
            stop("'/' is not implemented for objects of class 'XRleInteger' - try '%/%'")
        if (.Generic == '%/%')
            .Generic <- '/'
        else if (.Generic == '%%')
            .Generic <- '%'
        .Call("XRleInteger_Arith", e1, e2, .Generic, PACKAGE="IRanges")
    }
)

setMethod("Arith", signature(e1 = "XRleInteger", e2 = "integer"),
    function (e1, e2)
    {
        if (length(e2) == 1)
            e2 <-
              new("XRleInteger",
                  values = as(e2, "XInteger"),
                  lengths = as(e1, "XInteger"))
        else
            e2 <- XRleInteger(e2)
        callNextMethod(e1, e2)
    }
)

setMethod("Arith", signature(e1 = "integer", e2 = "XRleInteger"),
    function (e1, e2)
    {
        if (length(e1) == 1)
            e1 <-
              new("XRleInteger",
                  values = as(e1, "XInteger"),
                  lengths = as(e2, "XInteger"))
        else
            e1 <- XRleInteger(e1)
        callNextMethod(e1, e2)
    }
)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion
###

setAs("integer", "XRleInteger",
    function(from) XRleInteger(from))

setMethod("as.integer", "XRleInteger",
    function(x, ...) rep.int(as.integer(x@values), as.integer(x@lengths)))

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
        if (lo != 0L)
            cat(" [1] ", toNumSnippet(object, getOption("width")-5), "\n", sep="")
        invisible(object)
    }
)
