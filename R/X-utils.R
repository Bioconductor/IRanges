### Return the hexadecimal address of any R object in a string.
address <- function(x)
{
    .Call("address_asSTRSXP", x, PACKAGE="IRanges")
}

### Helper function (for debugging purpose).
### Print some obscure info about an "externalptr" object.
### Typical use:
###   show(new("externalptr"))
setMethod("show", "externalptr",
    function(object)
    {
        .Call("ExternalPtr_show", object, PACKAGE="IRanges")
    }
)

sapplyLength <- function(x)
{
    if (!is.list(x))
        stop("'x' must be a list")
    .Call("sapply_length", x, PACKAGE="IRanges")
}

