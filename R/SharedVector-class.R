### =========================================================================
### SharedVector objects
### -------------------------------------------------------------------------
###
### A SharedVector object is an external pointer to an ordinary vector.
###

setClass("SharedVector",
    representation("VIRTUAL",
        xp="externalptr",
        ## Any object that is never copied on assignment would be fine here.
        ## See R/BSgenome-class.R in the BSgenome package for how this slot
        ## is used for automatic uncaching of the sequences of a BSgenome
        ## object.
        .link_to_cached_object="environment"
    )
)

setMethod("length", "SharedVector",
    function(x) .Call("SharedVector_length", x, PACKAGE="IRanges")
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### First, a couple of very low-level utilities on externalptr objects.
###

typeoftag <- function(x)
{
    if (!is(x, "externalptr"))
        stop("'x' must be an externalptr object")
    .Call("externalptr_typeoftag", x, PACKAGE="IRanges")
}

### Helper function (for debugging purpose).
### Print some info about an externalptr object.
### Typical use:
###   show(new("externalptr"))
setMethod("show", "externalptr",
    function(object)
        .Call("externalptr_show", object, PACKAGE="IRanges")
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Validity.
###

.valid.SharedVector <- function(x)
{
    type <- typeoftag(x@xp)
    if (type != "double" && !extends(typeoftag(x@xp), "vector"))
        return("'x@xp' must be an external pointer to a vector")
    NULL
}

setValidity2("SharedVector", .valid.SharedVector)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion.
###

### Works as long as as.integer() works on 'x'.
setMethod("as.numeric", "SharedVector",
    function(x, ...) as.numeric(as.integer(x))
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Comparison.
###
### Be careful with the semantic of the "==" operator: the addresses are
### compared, not the data they are pointing at!
###

### Return the hexadecimal address of any R object in a string.
address <- function(x)
{
    .Call("address_asSTRSXP", x, PACKAGE="IRanges")
}

setMethod("==", signature(e1="SharedVector", e2="SharedVector"),
    function(e1, e2) address(e1@xp) == address(e2@xp)
)

setMethod("!=", signature(e1="SharedVector", e2="SharedVector"),
    function(e1, e2) address(e1@xp) != address(e2@xp)
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Miscellaneous stuff (not SharedVector related, but I didn't find a better
### place for now).
###

### Safe alternative to 'strsplit(x, NULL, fixed=TRUE)[[1]]'.
safeExplode <- function(x)
{
    if (!isSingleString(x))
        stop("'x' must be a single string")
    .Call("safe_strexplode", x, PACKAGE="IRanges")
}

