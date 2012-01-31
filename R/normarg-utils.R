### =========================================================================
### Utility functions for checking/fixing user-supplied arguments
### -------------------------------------------------------------------------


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### For checking only.
###

isTRUEorFALSE <- function(x)
{
    is.logical(x) && length(x) == 1L && !is.na(x)
}

isSingleInteger <- function(x)
{
    is.integer(x) && length(x) == 1L && !is.na(x)
}

isSingleNumber <- function(x)
{
    is.numeric(x) && length(x) == 1L && !is.na(x)
}

isSingleString <- function(x)
{
    is.character(x) && length(x) == 1L && !is.na(x)
}

### We want these functions to return TRUE when passed an NA of whatever type.
isSingleNumberOrNA <- function(x)
{
    is.atomic(x) && length(x) == 1L && (is.numeric(x) || is.na(x))
}

isSingleStringOrNA <- function(x)
{
    is.atomic(x) && length(x) == 1L && (is.character(x) || is.na(x))
}

### NOT exported.
anyMissing <- function(x) .Call2("anyMissing", x, PACKAGE="IRanges")

### NOT exported.
isNumericOrNAs <- function(x)
{
    is.numeric(x) || (is.atomic(x) && is.vector(x) && all(is.na(x)))
}

### NOT exported.
### isNotStrictlySorted() takes for granted that 'x' contains no NAs (behaviour
### is undefined if this is not the case). This allows isNotStrictlySorted() to
### be MUCH faster than is.unsorted() in some situations:
###   > x <- c(99L, 1:1000000)
###   > system.time(for (i in 1:1000) isNotStrictlySorted(x))
###    user  system elapsed 
###   0.004   0.000   0.003 
###   > system.time(for (i in 1:1000) is.unsorted(x, strictly=TRUE))
###    user  system elapsed 
###   6.925   1.756   8.690 
### So let's keep it for now! Until someone has enough time and energy to
### convince the R core team to fix is.unsorted()...
### Note that is.unsorted() does not only have a performance problem:
###   a) It also has a semantic problem: is.unsorted(NA) returns NA despite the
###      man page stating that all objects of length 0 or 1 are sorted (sounds
###      like a fair statement).
###   b) The sort()/is.unsorted() APIs and semantics are inconsistent.
###   c) Why did they choose to have is.unsorted() instead of is.sorted() in the
###      first place? Having is.unsorted( , strictly=TRUE) being a "looser test"
###      (or a "weaker condition") than is.unsorted( , strictly=FALSE) is really
###      counterintuitive!
###        > is.unsorted(c(5L, 5:8), strictly=FALSE)
###        [1] FALSE
###        > is.unsorted(c(5L, 5:8), strictly=TRUE)
###        [1] TRUE
###      Common sense would expect to have less objects that are "strictly
###      something" than objects that are "just something".
isNotSorted <- function(x) .Internal(is.unsorted(x, FALSE))
isNotStrictlySorted <- function(x) .Internal(is.unsorted(x, TRUE))


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### For checking AND fixing (aka normalizing).
###

### NOT exported.
numeric2integer <- function(x)
{
    if (is.numeric(x) && !is.integer(x)) as.integer(x) else x
}

### NOT exported.
### recycleVector() vs rep(x, length.out=length):
###   - The former seems a little bit faster (1.5x - 2x).
###   - The former will issue a warning that "number of items to replace is not
###     a multiple of replacement length". The latter will always remain silent.
recycleVector <- function(x, length.out)
{
    if (length(x) == length.out) {
        x
    } else {
        ans <- vector(storage.mode(x), length.out)
        ans[] <- x
        ans
    }
}

### NOT exported.
### Must always drop the names of 'arg'.
recycleArg <- function(arg, argname, length.out)
{
    if (length.out == 0L) {
        if (length(arg) > 1L)
            stop("invalid length for '", argname, "'")
        return(recycleVector(arg, length.out))
    }
    if (length(arg) == 0L)
        stop("'", argname, "' has no elements")
    if (length(arg) > length.out)
        stop("'", argname, "' is longer than 'x'")
    if (anyMissing(arg))
        stop("'", argname, "' contains NAs")
    if (length(arg) < length.out)
        arg <- recycleVector(arg, length.out)  # will drop the name
    else
        arg <- unname(arg)
    arg
}

recycleIntegerArg <- function(arg, argname, length.out)
{
    if (!is.numeric(arg))
        stop("'", argname, "' must be a vector of integers")
    if (!is.integer(arg))
        arg <- as.integer(arg)
    recycleArg(arg, argname, length.out)
}

recycleNumericArg <- function(arg, argname, length.out)
{
    if (!is.numeric(arg))
        stop("'", argname, "' must be a numeric vector")
    recycleArg(arg, argname, length.out)
}

### We use a signature in the style of successiveIRanges() or
### successiveViews().
### The current implementation should be fast enough if length(x)/circle.length
### is small (i.e. < 10 or 20). This will actually be the case for the typical
### usecase which is the calculation of "circular coverage vectors", that is,
### we use fold() on the "linear coverage vector" to turn it into a "circular
### coverage vector" of length 'circle.length' where 'circle.length' is the
### length of the circular sequence.  
fold <- function(x, circle.length, from=1)
{
    if (typeof(x) != "S4" && !is.numeric(x) && !is.complex(x))
        stop("'x' must be a vector-like object with elements that can be added")
    if (!isSingleNumber(circle.length))
        stop("'circle.length' must be a single integer")
    if (!is.integer(circle.length))
        circle.length <- as.integer(circle.length)
    if (circle.length <= 0L)
        stop("'circle.length' must be positive")
    if (!isSingleNumber(from))
        stop("'from' must be a single integer")
    if (!is.integer(from))
        from <- as.integer(from)
    from <- 1L + (from - 1L) %% circle.length
    if (typeof(x) == "S4") {
        ans <- as(rep.int(0L, circle.length), class(x))
        if (length(ans) != circle.length)
            stop("don't know how to handle 'x' of class ", class(x))
    } else {
        ans <- vector(typeof(x), length=circle.length)
    }
    if (from > length(x)) {
        ## Nothing to fold
        jj <- seq_len(length(x)) + circle.length - from + 1L
        ans[jj] <- x
        return(ans)
    }
    if (from > 1L) {
        ii <- seq_len(from - 1L)
        jj <- ii + circle.length - from + 1L
        ans[jj] <- x[ii]
    }
    max_from <- length(x) - circle.length + 1L
    while (from <= max_from) {
        ii <- from:(from+circle.length-1L)
        ans[] <- ans[] + x[ii]
        from <- from + circle.length
    }
    if (from > length(x))
        return(ans)
    ii <- from:length(x)
    jj <- ii - from + 1L
    ans[jj] <- ans[jj] + x[ii]
    ans
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Other non exported normarg* functions.
###

normargSingleStartOrNA <- function(start)
{
    if (!isSingleNumberOrNA(start))
        stop("'start' must be a single integer or NA")
    if (!is.integer(start))
        start <- as.integer(start)
    start
}

normargSingleEndOrNA <- function(end)
{
    if (!isSingleNumberOrNA(end))
        stop("'end' must be a single integer or NA")
    if (!is.integer(end)) 
        end <- as.integer(end)
    end
}

normargUseNames <- function(use.names)
{
    if (is.null(use.names))
        return(TRUE)
    if (!isTRUEorFALSE(use.names))
        stop("'use.names' must be TRUE or FALSE")
    use.names
}

normargAtomicList1 <- function(arg, List, lx, argname = deparse(substitute(arg)))
{
    if (is.vector(arg))
        arg <- List(as.list(recycleVector(arg, lx)))
    else if (!is(arg, "AtomicList"))
        stop("'", argname,"' must be a vector or AtomicList object")
    arg
}

normargAtomicList2 <- function(arg, List, lx, eln, argname = deparse(substitute(arg)))
{
    if (!(is.vector(arg) && length(arg) == 1L)) {
        if (is.vector(arg))
          arg <- as(rep(recycleVector(arg, lx), eln), class(unlist(List())))
        else {
          if (!is(arg, "AtomicList"))
            stop("'arg' must be a vector or AtomicList object")
          if (!isTRUE(all.equal(elementLengths(arg), eln,
                                check.attributes=FALSE)))
            arg <- mapply(recycleVector, arg, List(as.list(eln)))
          arg <- unlist(arg, use.names=FALSE)
        }
    } else if (is.list(arg)){
        arg <- unlist(arg, use.names=FALSE)
    }
    arg
}

normargRunK <- function(k, n, endrule)
{
    if (!is.numeric(k))
        stop("'k' must be a numeric vector")
    if (k < 0)
        stop("'k' must be positive")
    if ((endrule != "drop") && (k %% 2 == 0)) {
        k <- 1L + 2L * (k %/% 2L)
        warning(paste("'k' must be odd when 'endrule != \"drop\"'!",
                      "Changing 'k' to ", k))
    }
    if (k > n) {
        k <- 1L + 2L * ((n - 1L) %/% 2L)
        warning("'k' is bigger than 'n'! Changing 'k' to ", k)
    }
    as.integer(k)
}

normargSubset2_iOnly <-
    function(x, i, j, ..., .conditionPrefix=character())
{
    if (!missing(j) || length(list(...)) > 0)
        warning(.conditionPrefix, "arguments beyond 'i' ignored")
    if (missing(i))
        stop(.conditionPrefix, "subscript 'i' is missing")
    if (!is.character(i) && !is.numeric(i))
        stop(.conditionPrefix, "invalid subscript 'i' type")
    if (length(i) < 1L)
        stop(.conditionPrefix, "attempt to select less than one element")
    if (length(i) > 1L)
        stop(.conditionPrefix, "attempt to select more than one element")
    if (is.numeric(i) && (i < 1L || i > length(x)+1))
        stop(.conditionPrefix, "subscript 'i' out of bounds")
    if (is.character(i)) {
        i <- match(i, names(x))
        if (is.na(i))
            i <- length(x) + 1L
    }
    i
}

extraArgsAsList <- function(.valid.argnames, ...)
{
    args <- list(...)
    argnames <- names(args)
    if (length(args) != 0L
        && (is.null(argnames) || any(argnames %in% c("", NA))))
        stop("all extra arguments must be named")
    if (!is.null(.valid.argnames) && !all(argnames %in% .valid.argnames))
        stop("valid extra argument names are ",
             paste("'", .valid.argnames, "'", sep="", collapse=", "))
    if (anyDuplicated(argnames))
        stop("argument names must be unique")
    args
}

