### =========================================================================
### Some low-level (not exported) utility functions to operate on integer
### vectors
### -------------------------------------------------------------------------


anyMissingOrOutside <- function(x, lower = -.Machine$integer.max,
                                   upper = .Machine$integer.max)
{
    if (!is.integer(x))
        stop("'x' must be an integer vector")
    if (!is.integer(lower))
        lower <- as.integer(lower)
    if (!is.integer(upper))
        upper <- as.integer(upper)
    .Call("Integer_any_missing_or_outside", x, lower, upper, PACKAGE="IRanges")
}

### Equivalent to (but much faster than):
###
###     diff(c(0L, x))
###
### except that NAs are not supported.
diffWithInitialZero <- function(x)
{
    if (!is.integer(x))
        stop("'x' must be an integer vector")
    .Call("Integer_diff_with_0", x, PACKAGE="IRanges")
}

### For 'x' and 'y' integer vectors of equal length with no NAs,
### 'runEndsOfIntegerPairs(x, y)' finds the runs of identical rows in
### 'cbind(x, y)' and returns the indices of the last row in each run.
### In other words, it's equivalent to (but much faster than):
###
###     cumsum(runLength(Rle(paste(x, y, sep="|"))))
###
### Note that, if the rows in 'cbind(x, y)' are already sorted, then
### 'runEndsOfIntegerPairs(x, y)' returns the indices of the unique rows.
### In other words, 'runEndsOfIntegerPairs()' could be used to efficiently
### extract the unique pairs of integers from a presorted set of pairs.
### However, at the moment (April 2011) using 'duplicatedTwoIntegers()' is
### still faster than using 'runEndsOfIntegerPairs()' for finding the
### duplicated or unique pairs of integers in a presorted set of pairs.
### But this only because 'runEndsOfIntegerPairs()' is not as fast as it
### could/should be (an all-in-C implementation would probably solve this).
###
### For efficiency reasons, we don't support (and don't even check) for NAs.
### TODO: What happens if 'x' and 'y' don't have the same length? Shouldn't
### we check for that?
runEndsOfIntegerPairs <- function(x, y)
{
    not_same_as_prev <- diffWithInitialZero(x) != 0L |
                        diffWithInitialZero(y) != 0L
    if (length(not_same_as_prev) == 0L)
        return(integer())
    which(c(not_same_as_prev[-1L], TRUE))
}

orderInteger <- function(x, decreasing = FALSE, na.last = NA)
{
    if (!is.integer(x) && !is.factor(x))
        stop("'x' must be an integer vector")
    if ((is.integer(x) && diff(range(x[!is.na(x)])) < 100000) ||
        (is.factor(x) && length(levels(x)) < 100000))
      sort.list(x, decreasing = decreasing, na.last = na.last, method = "radix")
    else if (!anyMissing(x))
        .Call("Integer_order", x, decreasing, PACKAGE="IRanges")
    else sort.list(x, decreasing = decreasing, na.last = na.last)
}

### For 'x' and 'y' integer vectors of equal length with no NAs,
### 'orderTwoIntegers(x, y)' is equivalent to (but faster than):
###
###    order(x, y)
###
### For efficiency reasons, we don't support (and don't even check) for NAs.
### TODO: Maybe rename orderIntegerPairs().
### TODO: What happens if 'x' and 'y' don't have the same length? Shouldn't
### we check for that?
orderTwoIntegers <- function(x, y, decreasing = FALSE)
{
    if (!is.integer(x) && !is.factor(x))
        stop("'x' must be an integer vector or factor")
    if (!is.integer(y) && !is.factor(y))
        stop("'y' must be an integer vector or factor")
    .Call("Integer_order_two", x, y, decreasing, PACKAGE="IRanges")
}

### For 'x' and 'y' integer vectors of equal length with no NAs,
### 'duplicatedTwoIntegers(x, y)' is equivalent to (but much faster than):
###
###    duplicated(cbind(x, y))
###
### For efficiency reasons, we don't support (and don't even check) for NAs.
### TODO: Maybe rename duplicatedIntegerPairs().
duplicatedTwoIntegers <- function(x, y,
                                  fromLast=FALSE,
                                  method=c("auto", "quick", "hash"))
{
    if (is.factor(x))
        x <- as.integer(x)
    else if (!is.integer(x))
        stop("'x' must be an integer vector or factor")
    if (is.factor(y))
        y <- as.integer(y)
    else if (!is.integer(y))
        stop("'y' must be an integer vector or factor")
    if (length(x) != length(y))
        stop("'x' and 'y' must have the same length")
    if (!isTRUEorFALSE(fromLast))
        stop("'fromLast' must be TRUE or FALSE")
    method <- match.arg(method)
    if (length(x) == 0L)
        return(logical(0L))
    if (length(x) == 1L)
        return(FALSE)
    ## This is a temporary workaround until "quick" and "hash" methods can
    ## natively support fromLast=TRUE.
    ## TODO: Add support for fromLast=TRUE to "quick" and "hash" methods.
    if (fromLast)
        return(rev(duplicatedTwoIntegers(rev(x), rev(y), method=method)))
    if (method == "auto") {
        if (length(x) <= 2^29)
            method <- "hash"
        else
            method <- "quick"
    }
    if (method == "quick") {
        ans <- .Call("Integer_duplicated_xy_quick", x, y, PACKAGE="IRanges")
    } else {
        ## Author: Martin Morgan
        ans <- .Call("Integer_duplicated_xy_hash", x, y, PACKAGE="IRanges")
    }
    ans
}

sortedMerge <- function(x, y)
    .Call("Integer_sorted_merge", x, y, PACKAGE="IRanges")

mseq <- function(from, to)
{
    if (!is.integer(from))
        from <- as.integer(from)
    if (!is.integer(to))
        to <- as.integer(to)
    .Call("Integer_mseq", from, to, PACKAGE="IRanges")
}

findIntervalAndStartFromWidth <- function(x, width)
    .Call("findIntervalAndStartFromWidth", x, width, PACKAGE="IRanges")

### Reverse an injection from 1:M to 1:N.
### The injection is represented by an integer vector of length M (eventually
### with NAs). Fundamental property:
###     reverseIntegerInjection(reverseIntegerInjection(injection, N), M)
### is the identity function.
### Can be used to efficiently reverse the result of a call to 'order()'.
reverseIntegerInjection <- function(injection, N)
{
    M <- length(injection)
    ans <- rep.int(NA_integer_, N)
    is_not_na <- !is.na(injection)
    ans[injection[is_not_na]] <- seq_len(M)[is_not_na]
    ans
}

