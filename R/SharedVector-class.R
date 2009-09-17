### =========================================================================
### SharedVector and SharedVector_Pool objects
### -------------------------------------------------------------------------
###
### A SharedVector object is an external pointer to an ordinary vector.
### A SharedVector_Pool object *represents* a list of SharedVector objects but
### it is NOT *represented* as a list of such objects. This is a way to avoid
### having to generate long lists of S4 objects which the current S4
### implementation is *very* inefficient at.
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

setClass("SharedVector_Pool",
    representation("VIRTUAL",
        xp_list="list",
        .link_to_cached_object_list="list"
    )
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### SharedVector low-level methods.
###

setMethod("length", "SharedVector",
    function(x) .Call("SharedVector_length", x, PACKAGE="IRanges")
)

### Return the hexadecimal representation of the adress of the first
### element of the tag (i.e. the first element of the external vector).
### NOT exported.
setGeneric("address0", function(x) standardGeneric("address0"))

### NOT exported.
setGeneric("oneLineDesc", function(x) standardGeneric("oneLineDesc"))

setMethod("oneLineDesc", "SharedVector",
    function(x)
        paste(class(x), " instance of length ", length(x),
              " (data starting at address ", address0(x), ")", sep="")
)

setMethod("show", "SharedVector",
    function(object)
    {
        cat(oneLineDesc(object), "\n", sep="")
        ## What is correct here? The documentation (?show) says that 'show'
        ## should return an invisible 'NULL' but, on the other hand, the 'show'
        ## method for integers returns its 'object' argument...
        invisible(object)
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### SharedVector_Pool low-level methods.
###

setMethod("length", "SharedVector_Pool", function(x) length(x@xp_list))

setMethod("show", "SharedVector_Pool",
    function(object)
    {
        cat(class(object), " instance of length ", length(object), ":\n", sep="")
        for (i in seq_len(length(object)))
            cat(i, ": ", oneLineDesc(object[[i]]), "\n", sep="")
        invisible(object)
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Some very low-level utilities on externalptr objects.
###

tagtype <- function(x)
{
    if (!is(x, "externalptr"))
        stop("'x' must be an externalptr object")
    .Call("externalptr_tagtype", x, PACKAGE="IRanges")
}

isExternalVector <- function(x, tagtype=NA)
{
    if (!is(x, "externalptr"))
        return(FALSE)
    x_tagtype <- tagtype(x)
    if (!is.na(tagtype))
        return(x_tagtype == tagtype)
    return(x_tagtype == "double" || extends(x_tagtype, "vector"))
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

problemIfNotExternalVector <- function(what, tagmustbe="a vector")
{
    msg <- paste(what, "must be an external pointer to", tagmustbe)
    return(msg)
}

.valid.SharedVector <- function(x)
{
    if (!isExternalVector(x@xp))
        return(problemIfNotExternalVector("'x@xp'"))
    NULL
}

setValidity2("SharedVector", .valid.SharedVector)

.valid.SharedVector_Pool <- function(x)
{
    if (length(x@xp_list) != length(x@.link_to_cached_object_list))
        return("'x@xp_list' and 'x@.link_to_cached_object_list' must have the same length")
    if (!all(sapply(x@xp_list,
                    function(elt) isExternalVector(elt))))
        return(problemIfNotExternalVector("each element in 'x@xp_list'"))
    if (!all(sapply(x@.link_to_cached_object_list,
                    function(elt) is.environment(elt))))
        return("each element in 'x@.link_to_cached_object_list' must be an environment")
    NULL
}

setValidity2("SharedVector_Pool", .valid.SharedVector_Pool)


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

