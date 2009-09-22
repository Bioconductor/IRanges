### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "copy" generic and methods.
###

setGeneric("copy", function(x, ...) standardGeneric("copy"))

setMethod("copy", "SharedRaw",
    function(x, lkup=NULL, reverse=FALSE)
    {
        ans_length <- length(x)
        ans <- SharedRaw(ans_length)
        if (is.null(lkup)) {
            if (reverse)
                .Call("SharedRaw_reverse_copy_from_i1i2",
                      ans, x, 1L, ans_length, PACKAGE="IRanges")
            else
                .Call("SharedRaw_copy_from_i1i2",
                      ans, x, 1L, ans_length, PACKAGE="IRanges")
        } else {
            if (reverse)
                .Call("SharedRaw_reverse_translate_copy_from_i1i2",
                      ans, x, 1L, ans_length, lkup, PACKAGE="IRanges")
            else
                .Call("SharedRaw_translate_copy_from_i1i2",
                      ans, x, 1L, ans_length, lkup, PACKAGE="IRanges")
        }
        return(ans)
    }
)

setMethod("copy", "SharedVector_Pool",
    function(x, lkup=NULL, reverse=FALSE)
    {
        ## FIXME: This limitation must be temporary only. We need copy()
        ## to work on a SharedVector_Pool object of arbitrary length!
        if (length(x) != 1L)
            stop("IRanges internal error: length(x) != 1")
        as(copy(x[[1L]], lkup=lkup, reverse=reverse), "SharedVector_Pool")
    }
)

