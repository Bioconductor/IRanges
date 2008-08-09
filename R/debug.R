###

debug_AEbufs <- function()
    invisible(.Call("debug_AEbufs", PACKAGE="IRanges"))

debug_IRanges_class <- function()
    invisible(.Call("debug_IRanges_class", PACKAGE="IRanges"))

debug_IRanges_utils <- function()
    invisible(.Call("debug_IRanges_utils", PACKAGE="IRanges"))

