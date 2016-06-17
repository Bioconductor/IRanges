###

.onUnload <- function(libpath)
{
    library.dynam.unload("IRanges", libpath)
}

.test <- function() BiocGenerics:::testPackage("IRanges")

