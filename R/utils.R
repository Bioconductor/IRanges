### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Some low-level (exported) helper functions and classes.
###

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

isSingleString <- function(x)
{
    is.character(x) && length(x) == 1 && !is.na(x)
}

### We want these functions to return TRUE when passed an NA of whatever type.
isSingleIntegerOrNA <- function(x)
{
    is.atomic(x) && length(x) == 1 && (is.integer(x) || is.na(x))
}

isSingleNumberOrNA <- function(x)
{
    is.atomic(x) && length(x) == 1 && (is.numeric(x) || is.na(x))
}

isSingleStringOrNA <- function(x)
{
    is.atomic(x) && length(x) == 1 && (is.character(x) || is.na(x))
}

### The fastest implementation of isConstant() is hard to guess:
###   isConstant1 <- function(x) {length(x) != 0L && all(x == x[1L])}
###   isConstant2 <- function(x) {length(unique(x)) == 1L}
###   isConstant3 <- function(x) {sum(duplicated(x)) == length(x) - 1L}
###   isConstant4 <- function(x) {length(x) != 0L && min(x) == max(x)}
###   isConstant5 <- function(x) {length(x) != 0L && {rx <- range(x); rx[1] == rx[2]}}
### And the winner is... isConstant4()! It's 2x faster than isConstant1()
### and isConstant5(), 4x faster than isConstant2(), and 9x faster than
### isConstant3(). Results obtained on 'x0 <- rep.int(112L, 999999L)' with
### R-2.9 Under development (unstable) (2009-01-26 r47727).
isConstant <- function(x) {length(x) != 0L && min(x) == max(x)}

normargShift <- function(shift, nseq)
{
    if (!is.numeric(shift))
        stop("'shift' must be a vector of integers")
    if (!is.integer(shift))
        shift <- as.integer(shift)
    if (nseq == 0L)
        return(integer())
    if (length(shift) == 0L)
        stop("'shift' has no elements")
    if (length(shift) > nseq)
        stop("'shift' is longer than 'x'")
    if (any(is.na(shift)))
        stop("'shift' contains NAs")
    if (length(shift) < nseq)
        shift <- recycleVector(shift, nseq)
    shift
}

### Implements the same logic as normargShift() (except for the coercion to an
### integer vector).
normargWeight <- function(weight, nseq)
{
    if (!is.numeric(weight))
        stop("'weight' must be a numeric vector")
    if (nseq == 0L)
        return(integer())
    if (length(weight) == 0L)
        stop("'weight' has no elements")
    if (length(weight) > nseq)
        stop("'weight' is longer than 'x'")
    if (any(is.na(weight)))
        stop("'weight' contains NAs")
    if (length(weight) < nseq)
        weight <- recycleVector(weight, nseq)
    weight
}

setClassUnion("characterORNULL", c("character", "NULL"))

### We define the coercion method below as a workaround to the following
### bug in R:
###
###   setClass("A", representation(stuff="numeric"))
###   setMethod("as.vector", c("A", "missing"), function(x, mode) x@stuff)
###
###   a <- new("A", stuff=3:-5)
###   > as.vector(a)
###   [1]  3  2  1  0 -1 -2 -3 -4 -5
###   > as(a, "vector")
###   Error in as.vector(from) : 
###     no method for coercing this S4 class to a vector
###   > selectMethod("coerce", c("A", "vector"))
###   Method Definition:
###
###   function (from, to, strict = TRUE) 
###   {
###       value <- as.vector(from)
###       if (strict) 
###           attributes(value) <- NULL
###       value
###   }
###   <environment: namespace:methods>
###
###   Signatures:
###           from  to      
###   target  "A"   "vector"
###   defined "ANY" "vector"
###   > setAs("ANY", "vector", function(from) as.vector(from))
###   > as(a, "vector")
###   [1]  3  2  1  0 -1 -2 -3 -4 -5
setAs("ANY", "vector", function(from) as.vector(from))


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Other low-level (not exported) helper functions.
###

isNumericOrNAs <- function(x)
{
    is.numeric(x) || (is.atomic(x) && is.vector(x) && all(is.na(x)))
}

numeric2integer <- function(x)
{
    if (is.numeric(x) && !is.integer(x)) as.integer(x) else x
}

normargIntegerOrNA <- function(x, argname)
{
    if (!isNumericOrNAs(x))
        stop("'", argname, "' must be a vector of integers")
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

normargUseNames <- function(use.names)
{
    if (is.null(use.names))
        return(TRUE)
    if (!isTRUEorFALSE(use.names))
        stop("'use.names' must be TRUE or FALSE")
    use.names
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

### recycleVector() vs rep(x, length.out=length):
###   - The former seems a little bit faster (1.5x - 2x).
###   - The former will issue a warning that "number of items to replace is not
###     a multiple of replacement length". The latter will always remain silent.
recycleVector <- function(x, length)
{
    ans <- vector(storage.mode(x), length)
    ans[] <- x
    ans
}

### Pretty printing

labeledLine <- function(label, els, count = TRUE, sep = " ", ellipsis = "...") {
  if (count)
    label <- paste(label, "(", length(els), ")", sep = "")
  label <- paste(label, ": ", sep = "")
  width <- getOption("width") - nchar(label)
  line <- ellipsize(els, width, sep, ellipsis)
  paste(label, line, "\n", sep = "")
}

ellipsize <- function(obj, width = getOption("width"), sep = " ",
                      ellipsis = "...")
{
  str <- encodeString(obj)
  ## get order selectSome() would print
  half <- seq_len(ceiling(length(obj) / 2))
  ind <- t(cbind(half, length(obj) - half + 1))[seq_along(obj)]
  nc <- cumsum(nchar(str[ind]) + nchar(sep)) - nchar(sep)
  last <- findInterval(width, nc)
  if (length(obj) > last) {
    ## make sure ellipsis fits
    while (last && (nc[last] + nchar(sep)*2^(last>1) + nchar(ellipsis)) > width)
      last <- last - 1
    if (last == 0) ## have to truncate the first element
      str <- paste(substring(str[1], 1, width - nchar(ellipsis)), ellipsis,
                   sep = "")
    else if (last == 1) ## can only show the first
      str <- c(str[1], "...")
    else str <- selectSome(str, last+1)
  }
  paste(str, collapse = sep)
}

## taken directly from Biobase
selectSome <- function (obj, maxToShow = 5) 
{
  len <- length(obj)
  if (maxToShow < 3) 
    maxToShow <- 3
  if (len > maxToShow) {
    maxToShow <- maxToShow - 1
    bot <- ceiling(maxToShow/2)
    top <- len - (maxToShow - bot - 1)
    nms <- obj[c(1:bot, top:len)]
    c(as.character(nms[1:bot]), "...", as.character(nms[-c(1:bot)]))
  }
  else obj
}

mseq <- function(from, to) {
  from <- as.integer(from)
  to <- as.integer(to)
  if (length(from) != length(to))
    stop("lengths of 'from' and 'to' must be equal")
  if (any(to < from))
    stop("every element in 'to' must be >= corresponding element in 'from'")
  .Call("Integer_mseq", from, to, PACKAGE="IRanges")
}
