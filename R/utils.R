### Some low-level (not exported) helper functions

extraArgsAsList <- function(.valid.argnames, ...)
{
    args <- list(...)
    argnames <- names(args)
    if (length(args) != 0
        && (is.null(argnames) || any(argnames %in% c("", NA))))
        stop("all extra arguments must be named")
    if (!is.null(.valid.argnames) && !all(argnames %in% .valid.argnames))
        stop("valid extra argument names are ",
             paste("'", .valid.argnames, "'", sep="", collapse=", "))
    if (any(duplicated(argnames)))
        stop("argument names must be unique")
    args
}

isTRUEorFALSE <- function(x)
{
    is.logical(x) && length(x) == 1 && !is.na(x)
}

isSingleInteger <- function(x)
{
    is.integer(x) && length(x) == 1 && !is.na(x)
}

isSingleNumber <- function(x)
{
    is.numeric(x) && length(x) == 1 && !is.na(x)
}

isSingleNumberOrNA <- function(x)
{
    is.vector(x) && is.atomic(x) && length(x) == 1 && (is.numeric(x) || is.na(x))
}

isNumericOrNAs <- function(x)
{
    is.numeric(x) || (is.atomic(x) && is.vector(x) && all(is.na(x)))
}

isSingleString <- function(x)
{
    is.character(x) && length(x) == 1 && !is.na(x)
}

numeric2integer <- function(x)
{
    if (is.numeric(x) && !is.integer(x)) as.integer(x) else x
}

stopIfProblems <- function(problems)
{
    if (!is.null(problems)) stop(paste(problems, collapse="\n  "))
}

normargIntegerOrNA <- function(x, name)
{
    if (!isNumericOrNAs(x))
        stop(paste("'", name, "' must be an integer vector", sep = ""))
    if (!is.integer(x))
        x <- as.integer(x)
    x
}

normargSingleStart <- function(start)
{
    if (!isSingleNumber(start))
        stop("'start' must be a single integer")
    if (!is.integer(start))
        start <- as.integer(start)
    start
}

normargSingleEnd <- function(end)
{
    if (!isSingleNumber(end))
        stop("'end' must be a single integer")
    if (!is.integer(end))
        end <- as.integer(end)
    end
}

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

normargSingleWidthOrNA <- function(width)
{
    if (!isSingleNumberOrNA(width))
        stop("'width' must be a single integer or NA")
    if (!is.integer(width))    
        width <- as.integer(width)
    width
}

normargUseNames <- function(use.names)
{
    if (is.null(use.names))
        return(TRUE)
    if (!isTRUEorFALSE(use.names))
        stop("'use.names' must be 'TRUE' or 'FALSE'")
    use.names
}

recycleVector <- function(x, length)
{
	y <- vector(storage.mode(x), length)
	y[] <- x
	y
}

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
isNotStrictlySorted <- function(x) .Internal(is.unsorted(x, TRUE))

