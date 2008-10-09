### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "coverage" generic and methods.
###

setGeneric("coverage", signature="x",
    function(x, start=NA, end=NA, ...) standardGeneric("coverage")
)

setMethod("coverage", "IRanges",
    function(x, start=NA, end=NA, weight=1L)
    {
        if (!isSingleNumber(start))
            stop("'start' must be a single integer")
        if (!is.integer(start))
            start <- as.integer(start)
        if (!isSingleNumber(end))
            stop("'end' must be a single integer")
        if (!is.integer(end))
            end <- as.integer(end)
        width <- end - start + 1L
        if (width < 0)
            stop("'end' must be >= 'start' - 1")
        if (!is.numeric(weight) || !(length(weight) %in% c(1, length(x))))
            stop("'weight' must be an integer vector with length 1 or 'length(x)'")
        if (!is.integer(weight))
            weight <- as.integer(weight)
        x1 <- shift(restrict(x, start=start, end=end), 1L - start)
        .Call("IRanges_coverage", x1, weight, order(start(x1)), width, PACKAGE="IRanges")
    }
)

setMethod("coverage", "MaskCollection",
    function(x, start=NA, end=NA, weight=1L)
    {
        if (!isSingleNumberOrNA(start))
            stop("'start' must be a single integer or NA")
        if (!is.integer(start))
            start <- as.integer(start)
        if (is.na(start))
            start <- 1L
        if (!isSingleNumberOrNA(end))
            stop("'end' must be a single integer or NA")
        if (!is.integer(end))
            end <- as.integer(end)
        if (is.na(end))
            end <- width(x)
        width <- end - start + 1L
        if (width < 0)
            stop("'end' must be >= 'start' - 1")
        if (!is.numeric(weight))
            stop("'weight' must be an integer vector")
        if (!is.integer(weight))
            weight <- as.integer(weight)
        if (length(weight) != length(x)) {
            if (length(weight) < 1 || length(weight) > length(x))
                stop("'length(weight)' must be >= 1 and <= 'length(x)'")
                weight <- rep(weight, length.out=length(x))
        }
        ans <- new("XRleInteger", values = XInteger(1, 0L), lengths = XInteger(1, width))
        for (i in seq_len(length(x))) {
            x1 <- shift(restrict(x[[i]], start=start, end=end), 1L - start)
            ans <-
              .Call("XRleInteger_add", ans,
                    .Call("IRanges_coverage", x1, weight[i], order(start(x1)), width,
                          PACKAGE="IRanges"),
                    PACKAGE="IRanges")
        }
        ans
    }
)

