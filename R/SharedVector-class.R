### =========================================================================
### SharedVector and SharedVector_Pool objects
### -------------------------------------------------------------------------
###
### A SharedVector object is an external pointer to an ordinary vector.
### A SharedVector_Pool object is *conceptually* a list of SharedVector
### objects but is actually NOT *implemented* as a list of such objects.
### This is to avoid having to generate long lists of S4 objects which the
### current S4 implementation is *very* inefficient at.
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
### Some very low-level utilities on externalptr objects.
###

tagtype <- function(x)
{
    if (!is(x, "externalptr"))
        stop("'x' must be an externalptr object")
    .Call("externalptr_tagtype", x, PACKAGE="IRanges")
}

tagIsVector <- function(x, tagtype=NA)
{
    if (!is(x, "externalptr"))
        stop("'x' must be an externalptr object")
    x_tagtype <- tagtype(x)
    if (!is.na(tagtype))
        return(x_tagtype == tagtype)
    return(x_tagtype == "double" || extends(x_tagtype, "vector"))
}

taglength <- function(x)
{
    if (!is(x, "externalptr"))
        stop("'x' must be an externalptr object")
    .Call("externalptr_taglength", x, PACKAGE="IRanges")
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
### SharedVector low-level methods.
###

setMethod("length", "SharedVector", function(x) taglength(x@xp))

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

setMethod("width", "SharedVector_Pool",
    function(x)
        if (length(x) == 0L) integer(0) else sapply(x@xp_list, taglength)
)

setMethod("show", "SharedVector_Pool",
    function(object)
    {
        cat(class(object), " instance of length ", length(object), ":\n", sep="")
        for (i in seq_len(length(object)))
            cat(i, ": ", oneLineDesc(object[[i]]), "\n", sep="")
        invisible(object)
    }
)

### If 'x' is a SharedVector object, then 'as(x, "SharedVector_Pool")[[1]]'
### is identical to 'x'.
setAs("SharedVector", "SharedVector_Pool",
    function(from)
    {
        ans_class <- paste(class(from), "_Pool", sep="")
        new2(ans_class,
             xp_list=list(from@xp),
             .link_to_cached_object_list=list(from@.link_to_cached_object),
             check=FALSE)
    }
)

setMethod("c", "SharedVector_Pool",
    function(x, ..., recursive=FALSE)
    {
        if (!identical(recursive, FALSE))
            stop("'recursive' argument not supported")
        x@xp_list <-
            do.call(c, lapply(list(x, ...),
                              function(arg) arg@xp_list))
        x@.link_to_cached_object_list <-
            do.call(c, lapply(list(x, ...),
                              function(arg) arg@.link_to_cached_object_list))
        x
    }
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
    if (!tagIsVector(x@xp))
        return(problemIfNotExternalVector("'x@xp'"))
    NULL
}

setValidity2("SharedVector", .valid.SharedVector)

.valid.SharedVector_Pool <- function(x)
{
    if (length(x@xp_list) != length(x@.link_to_cached_object_list))
        return("'x@xp_list' and 'x@.link_to_cached_object_list' must have the same length")
    if (!all(sapply(x@xp_list,
                    function(elt) tagIsVector(elt))))
        return(problemIfNotExternalVector("each element in 'x@xp_list'"))
    if (!all(sapply(x@.link_to_cached_object_list,
                    function(elt) is.environment(elt))))
        return("each element in 'x@.link_to_cached_object_list' must be an environment")
    NULL
}

setValidity2("SharedVector_Pool", .valid.SharedVector_Pool)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Low-level copy.
###

### 'lkup' must be NULL or a vector of integers
SharedVector.copy <- function(dest, i, imax=integer(0), src, lkup=NULL)
{
    if (!is(src, "SharedVector"))
        stop("'src' must be a SharedVector object")
    if (!is.integer(i))
        i <- as.integer(i)
    if (length(i) == 1) {
        if (length(imax) == 0)
            imax <- i
        else
            imax <- as.integer(imax)
        width <- imax - i + 1L
        .Call("SharedVector_copy_from_start",
              dest, src, i, width, lkup, FALSE, PACKAGE="IRanges")
    } else {
        .Call("SharedVector_copy_from_subset",
              dest, src, i, lkup, PACKAGE="IRanges")
    }
    dest
}

### 'lkup' must be NULL or a vector of integers
SharedVector.reverseCopy <- function(dest, i, imax=integer(0), src, lkup=NULL)
{
    if (!is(src, "SharedVector"))
        stop("'src' must be a SharedVector object")
    if (length(i) != 1)
        stop("'i' must be a single integer")
    if (!is.integer(i))
        i <- as.integer(i)
    if (length(imax) == 0)
        imax <- i
    else
        imax <- as.integer(imax)
    width <- imax - i + 1L
    .Call("SharedVector_copy_from_start",
          dest, src, i, width, lkup, TRUE, PACKAGE="IRanges")
    dest
}


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

