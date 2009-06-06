### =========================================================================
### External pointer to an atomic vector: the "SequencePtr" class
### -------------------------------------------------------------------------

### Note: instead of defining the "SequencePtr" class with just one slot of
### type "externalptr" (it HAS an "externalptr", and nothing else), an
### alternative could have been to simply extend the "externalptr" type.
### After all, a SequencePtr object IS an "externalptr" object.
### However, when doing this, then creating new RawPtr/IntegerPtr/NumericPtr
### objects with new() would ALWAYS return the SAME instance (its address in
### memory will always be the same for a given R session).

setClass("SequencePtr",
    representation("VIRTUAL",
        xp="externalptr",
        ## Any object that is never copied on assignment would be fine here.
        ## See R/BSgenome-class.R in the BSgenome package for how this slot
        ## is used for automatic uncaching of the sequences of a BSgenome
        ## object.
        .link_to_cached_object="environment"
    )
)

setMethod("length", "SequencePtr",
    function(x) .Call("SequencePtr_length", x, PACKAGE="IRanges")
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion.
###

### Works as long as as.integer() works on 'x'.
setMethod("as.numeric", "SequencePtr",
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

setMethod("==", signature(e1="SequencePtr", e2="SequencePtr"),
    function(e1, e2) address(e1@xp) == address(e2@xp)
)

setMethod("!=", signature(e1="SequencePtr", e2="SequencePtr"),
    function(e1, e2) address(e1@xp) != address(e2@xp)
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Miscellaneous stuff (not SequencePtr related, but I didn't find a better
### place for now).
###

### Helper function (for debugging purpose).
### Print some obscure info about an "externalptr" object.
### Typical use:
###   show(new("externalptr"))
setMethod("show", "externalptr",
    function(object)
        .Call("ExternalPtr_show", object, PACKAGE="IRanges")
)

### Safe alternative to 'strsplit(x, NULL, fixed=TRUE)[[1]]'.
safeExplode <- function(x)
{
    if (!isSingleString(x))
        stop("'x' must be a single string")
    .Call("safe_strexplode", x, PACKAGE="IRanges")
}

