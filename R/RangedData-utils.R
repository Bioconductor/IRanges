### =========================================================================
### RangedData utilities
### -------------------------------------------------------------------------

setGeneric("rlencode", function(x, ...) standardGeneric("rlencode"))

setMethod("rlencode", "RangedData",
          function(x, start = NA, end = NA, gapvalue = NA)
          {
            if (length(gapvalue) != 1)
              stop("length of 'gapvalue' must be 1")
            ranges <- ranges(x)
            keep <- rep(TRUE, length(x))
            if (!is.na(start))
              keep[end(ranges) < start] <- FALSE
            if (!is.na(end))
              keep[start(ranges) > end] <- FALSE
            x <- x[keep]
            g <- gaps(ranges, start, end)
            ranges <- ranges(x)
            widths <- c(width(ranges), width(g))
            starts <- c(start(ranges), start(g))
            startorder <- order(starts)
            widths <- widths[startorder]
            vals <- values(x)
            if (!canCoerce(vals, "vector"))
              stop("cannot coerce values of class '", class(vals),
                   "' to a vector")
            vals <- as.vector(vals)
            if (!is.atomic(vals))
              stop("could not coerce values to an atomic vector")
            vals <- c(vals, rep(as(gapvalue, class(vals)), length(g)))
            structure(list(lengths=widths, values=vals[startorder]),
                      class = "rle")
          })
