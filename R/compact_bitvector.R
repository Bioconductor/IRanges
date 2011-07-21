### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Basic manipulation of a "compact bit vector" i.e. a bit vector stored in
### a standard raw vector.
###

logicalAsCompactBitvector <- function(x)
{
    if (!is.logical(x))
        stop("'x' must be a logical vector")
    .Call2("logical_as_compact_bitvector", x, PACKAGE="IRanges")
}

compactBitvectorAsLogical <- function(x, length.out)
{
    if (!is.raw(x))
        stop("'x' must be a raw vector")
    if (!isSingleNumber(length.out))
        stop("'length.out' must be a single number")
    if (!is.integer(length.out))
        length.out <- as.integer(length.out)
    .Call2("compact_bitvector_as_logical", x, length.out, PACKAGE="IRanges")
}

subsetCompactBitvector <- function(x, i)
{
    if (!is.raw(x))
        stop("'x' must be a raw vector")
    if (!is.integer(i))
        stop("'i' must be an integer vector")
    .Call2("subset_compact_bitvector", x, i, PACKAGE="IRanges")
}

