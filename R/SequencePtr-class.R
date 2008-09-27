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
        ## any object that is never copied on assignment would be fine here,
        ## we don't do anything with this slot, except calling
        ## monitorGarbageCollection() on it for the SequencePtr instances that
        ## we want to monitor.
        .momitor_me="environment"
    )
)

setMethod("length", "SequencePtr",
    function(x) .Call("SequencePtr_length", x, PACKAGE="IRanges")
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

### 'e' must be an environment or external pointer.
### 'symbol' and 'env' are the name and the location of the variable
### used for monitoring.
monitorGarbageCollection <- function(e, symbol, env)
{
    if (exists(symbol, envir=env, inherits=FALSE)) 
        status0 <- get(symbol, envir=env, inherits=FALSE) + 1L
    else
        status0 <- 1L
    reg.finalizer(e,
        function(e)
        {
            status <- get(symbol, envir=env, inherits=FALSE) - 1L
            assign(symbol, status, envir=env)
        }
    )
    assign(symbol, status0, envir=env)
}

sapplyLength <- function(x)
{
    if (!is.list(x))
        stop("'x' must be a list")
    .Call("sapply_length", x, PACKAGE="IRanges")
}

### Safe alternative to 'strsplit(x, NULL, fixed=TRUE)[[1]]'.
safeExplode <- function(x)
{
    if (!isSingleString(x))
        stop("'x' must be a single string")
    .Call("safe_strexplode", x, PACKAGE="IRanges")
}

