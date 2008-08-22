### Return the hexadecimal address of any R object in a string.
address <- function(x)
{
    .Call("IRanges_sexp_address", x, PACKAGE="IRanges")
}

### Helper function (for debugging purpose).
### Print some obscure info about an "externalptr" object.
### Typical use:
###   show(new("externalptr"))
setMethod("show", "externalptr",
    function(object)
    {
        .Call("IRanges_xp_show", object, PACKAGE="IRanges")
    }
)
