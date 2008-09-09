### =========================================================================
### XInteger objects
### -------------------------------------------------------------------------


### FIXME: This is a temporary "redirection" of the XInteger class to the
### IntegerPtr class. Defining the XInteger class this way is broken: if
### XInteger objects are just this, i.e. IntegerPtr objects, then it's not
### possible to extract a subvector from an XInteger object and return a new
### XInteger object without copying the original data!
### For example, the [[ operator for XIntegerViews objects cannot be made a
### "no-data-copy" operator: it will be forced to make a partial copy of the
### original vector (the subject).
setClass("XInteger", contains="IntegerPtr")

### Temporary constructor.
XInteger <- function(...) as(IntegerPtr(...), "XInteger")

### The problem described above can be addressed by defining the XInteger
### class like this (definition analog to the one used for the XString class
### in the Biostrings package):
###
###   setClass("XInteger",
###     representation(
###         xdata="IntegerPtr",   # an external pointer to the "seed" vector
###         offset="integer",     # a single integer
###         length="integer"      # a single integer
###     ),
###     prototype(
###         #xdata=IntegerPtr(0), # see newEmptyXInteger() below for why this
###                               # doesn't work
###         offset=0L,
###         length=0L
###     )
###   )


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "newEmptyXInteger" constructor.
### For internal use only. No need to export.
###
### Note that this cannot be made the prototype part of the XInteger class
### definition (and trying to do so will cause an error at installation time)
### because the DLL of the package needs to be loaded before IntegerPtr() can
### be called.
###

#newEmptyXInteger <- function(class) new(class, xdata=IntegerPtr(0))


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "show" method.
###

toNumSnippet <- function(x, width = getOption("width"))
{
    width <- max(0, width - 4)
    element_length <- format.info(x[seq_len(min(length(x), width %/% 2))])[1] + 1
    number_of_elements <- min(length(x), width %/% element_length)
    if (number_of_elements == length(x))
        ending <- ""
    else
        ending <- " ..."
        paste(paste(format(x[seq_len(number_of_elements)]), collapse = " "), ending, sep = "")
}

setMethod("show", "XInteger",
    function(object)
    {
        lo <- length(object)
        cat("  ", lo, "-integer \"", class(object), "\" instance\n", sep="")
        cat(" [1] ")
        cat(toNumSnippet(object, getOption("width") - 4))
        cat("\n")
        ## What is correct here? The documentation (?show) says that 'show'
        ## should return an invisible 'NULL' but, on the other hand, the 'show'
        ## method for intergers returns its 'object' argument...
        invisible(object)
    }
)

