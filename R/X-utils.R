### Return the hexadecimal address of any R object in a string.
address <- function(x)
{
    .Call("address_asSTRSXP", x, PACKAGE="IRanges")
}

sapplyLength <- function(x)
{
    if (!is.list(x))
        stop("'x' must be a list")
    .Call("sapply_length", x, PACKAGE="IRanges")
}

